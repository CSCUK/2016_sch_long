# Code information ----

# Longitudinal Scholar tracking survey: data analysis
# Matt Mawer, The Association of Commonwealth Universities
# February, 2017

# --- Library calls ----

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(RODBC, tidyverse, forcats, stringr)

# --- Global Variables ----

#update these values to correct SurveyIDs for your longitudinal follow-up and baseline surveys

long_10 <- "Sch_2006_Ten"
long_8 <- "Sch_2008_Eight"
long_6 <- "Sch_2010_Six"
long_4<- "Sch_2012_Four"
long_2 <- "Sch_2014_Two"
base_0 <- "Sch_2016_0"
SurveyName <- "2016 Alumni Survey"
opar = par() # no need to change this variable: opar stores graphical parameters, as a backup

# 1] Data import ----

# Data from sources:
# 1. Libra
# 2. Evaluation database

## a] DB connections ----

con.evaldb <- odbcConnect("EvalDb")
con.libra <- odbcConnect("Libra") 

con.evaldb
con.libra

# 1. DB query for population data

## Stage 1] DB query for admin data to join with survey results
## Limited to 2000 onwards because: 1) this removes some problems with database query of legacy data, 2) all survey resps. should be >2000
population <- sqlQuery(con.libra, "
     SELECT 
       TBL_AWARD.AWDID AS AWDID,
       TBL_AWARD.AWDSCH AS Scheme,
       TBL_AWARD.AWDYR AS Year,
       TBL_LKUPGENDER.GENDERNAME AS Gender,
       TBL_AWCOURSE.AWCRSPHD AS PhD,
       TBL_AWCOURSE.AWCRSSTATUS AS Status,
       TBL_AWARD.AWDPQDEGREE AS DegreeCode,
       TBL_AWARD.AWDCTTEESCORE AS CtteeScore,
       TBL_AWARD.AWDCTTEEACADGRD AS AcadGrade,
       TBL_AWARD.AWDCTTEEDEVGRD AS DevGrade,
       TBL_AWARD.AWDCTTEEPROPGRD AS PropGrade,
       TBL_AWARD.AWDCTTEELDRGRD AS LdrGrade,
       TBL_LKUPDISCCATEGORY.DISCCATNAME AS JacsCat,
       TBL_LKUPDISCSUBJECT.DISCSUBJECTNAME AS JacsSubj
     FROM 
       TBL_AWARD 
       LEFT JOIN TBL_PERSON ON TBL_PERSON.PRSNID = TBL_AWARD.PRSNID
       LEFT JOIN TBL_AWCOURSE ON TBL_AWCOURSE.AWDID = TBL_AWARD.AWDID
       LEFT JOIN TBL_LKUPDISCSUBJECT ON TBL_LKUPDISCSUBJECT.DISCSUBJCODETXT = TBL_AWARD.AWPROPDISCSUBJ
       LEFT JOIN TBL_LKUPDISCCATEGORY ON TBL_LKUPDISCCATEGORY.DISCCATCODE = TBL_LKUPDISCSUBJECT.DISCCATCODE
       LEFT JOIN TBL_LKUPGENDER ON TBL_LKUPGENDER.GENDERCODE = TBL_PERSON.PRSNGENDER
     WHERE
       TBL_AWARD.AWDYR >=2000 AND
       TBL_AWARD.AWDSCH NOT LIKE 'F%' AND TBL_AWARD.AWDSCH NOT LIKE 'Z%' AND TBL_AWARD.AWDSCH <>'CT' AND
       TBL_AWCOURSE.AWCRSSTATUS NOT IN ('SR','ST','SF', 'AT','NN','NX','NW','DA','DC') AND 
       NOT (TBL_AWARD.AWDSCH <> ('CD') and TBL_AWCOURSE.AWCRSSTATUS in ('TT', 'AH','DH'))
     ")

## Stage 2] check for duplicates that need correcting in Libra records
population %>% filter(duplicated(population$AWDID)) %>% select(-starts_with("Jacs")) %>% tbl_df

## If there are no further problems with duplicates - pay close note to PhD duplicates - then refine to only unique records
population <- population %>% filter(!duplicated(population$AWDID)) %>% tbl_df

# Stage 3] Create schemetype and ctteegroup variables: later separates out CR as a separate group

doctorateCodes <- c("24","25","26","27","28","29","30","31","51","53","54","57","82","89","150","151",
                     "153","189","195","196","230","242","245","283","524","550","67","499","556")
  # Codes from LKUPDEGREE in Libra that correspond to doctorates - need these to filter into schemes appropriately

population <- population %>% 
              mutate(SchemeType = 
                        ifelse(Scheme %in% c("CS", "CA", "CR") & DegreeCode=="520", "Split Site",
                        ifelse(Scheme %in% c("CA", "CS", "CR") & DegreeCode %in% doctorateCodes, "PhD", 
                        ifelse(Scheme %in% c("CS", "CA", "CR"), "Masters",   
                        ifelse(Scheme %in% ("CN"), "Split Site",
                        ifelse(Scheme %in% ("CD"), "Distance",
                        ifelse(Scheme %in% ("SS"), "Shared",
                        ifelse(Scheme %in% ("CF"), "Academic Fellow",
                        ifelse(Scheme %in% ("CM"), "Medical Fellow",
                        ifelse(Scheme %in% ("CP"), "Professional Fellow",
                        paste(Scheme) ))))))))),
                     CtteeGroup = ifelse(Scheme %in% "CR","Agency: Developed", paste(SchemeType))
                )

## Stage 4] Calculate z-scores within each year for each ctteegroup
population <- population %>%   
              group_by(Year, CtteeGroup) %>%
              mutate(CtteeScore = replace(CtteeScore, CtteeScore==0, NA),
                     ZCtteeScore = (CtteeScore - mean(CtteeScore, na.rm=T))/sd(CtteeScore, na.rm=T)
                    ) %>% 
              ungroup()

## Stage 5] Remove intermediary variables used in this process
population <- select(population,-DegreeCode, -PhD)
  
# 2. DB query for survey data

## Query data and join onto admin and geodata
## Two step process required for alumni data because +2 survey has diferent variables, so needs join and bind rows, removes unneeded columns
alum.query <-"SELECT tbl_DATA_Sch_%s.*,tbl_Ctrl_EvalInfo.PhD FROM tbl_DATA_Sch_%s LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_DATA_Sch_%s.AWDID WHERE tbl_DATA_Sch_%s.SurveyID='%s'"

alumni.data <- 
  bind_rows(list(
    sqlQuery(con.evaldb, sprintf(alum.query,4,4,4,4,long_4)),
    sqlQuery(con.evaldb, sprintf(alum.query,6,6,6,6,long_6)),
    sqlQuery(con.evaldb, sprintf(alum.query,8,8,8,8,long_8)),
    sqlQuery(con.evaldb, sprintf(alum.query,10,10,10,10,long_10)))) %>%
  full_join(sqlQuery(con.evaldb, sprintf(alum.query,2,2,2,2,long_2)))

alumni.data <- 
  alumni.data %>% 
  left_join(population, by="AWDID") %>% 
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Origin"="Country")) %>%
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Residency"="Country")) %>% 
  select(-CurrentSector, -DateAdded) %>% 
  rename(OriginRegion=Region.x,ResidencyRegion = Region.y, CurrentSector=DCurrentSector) %>% 
  tbl_df()

## 3. DB query for baseline data, joins onto admin and geodata
base.data <- 
  sqlQuery(con.evaldb, sprintf("SELECT tbl_DATA_Sch_0.* FROM tbl_DATA_Sch_0 WHERE tbl_DATA_Sch_0.SurveyID='%s'",base_0)) %>% 
  left_join(select(population,-SchemeType), by="AWDID") %>% 
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Origin"="Country")) %>% 
  select(-DateAdded,-StudyScholFunder,-PreSector) %>% 
  rename(OriginRegion = Region, PreSector= DPreSector, StudyScholFunder = DStudyScholFunder) %>% 
  tbl_df()

# 4. DB query for (alumni) response rates data

#define query with dynamic variables
resp.query <-
  "SELECT 
      tbl_Ctrl_Respondents.AWDID,
      tbl_LKUP_ResponseStatus.ResponseName,
      tbl_Ctrl_EvalInfo.YearGroup
    FROM
      ((tbl_Ctrl_Respondents
      LEFT JOIN tbl_LKUP_ResponseStatus ON tbl_LKUP_ResponseStatus.StatusCode = tbl_Ctrl_Respondents.ResponseStatus)
      LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_Ctrl_Respondents.AWDID)
    WHERE 
      tbl_Ctrl_Respondents.SurveyID in ('%s') OR tbl_Ctrl_Respondents.SurveyID in ('%s') OR 
      tbl_Ctrl_Respondents.SurveyID in ('%s') OR tbl_Ctrl_Respondents.SurveyID in ('%s') OR 
      tbl_Ctrl_Respondents.SurveyID in ('%s')"

# incorrect line breaks and whitespace introduced by splitting sprintf over multiple lines (worth keeping on multiple lines to make readable)
resp.query <- str_replace_all(str_replace_all(resp.query,"\n",""),"\\s+"," ")

#query database for response rates data
response.data <- 
  sqlQuery(con.evaldb,sprintf(resp.query,long_2,long_4,long_6,long_8,long_10)) %>% 
  left_join(population, by="AWDID") %>% 
  left_join(sqlQuery(con.libra,"SELECT TBL_AWARD.AWDID, TBL_LKUPCOUNTRY.CTRYNAME AS Origin FROM TBL_AWARD LEFT JOIN TBL_LKUPCOUNTRY ON TBL_LKUPCOUNTRY.CTRYCODE = TBL_AWARD.AWDCTRYORIGIN"),by="AWDID") %>%  
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Origin"="Country")) %>%
  select(-Year,-Status,-CtteeScore) %>% 
  tbl_df()

# Cleanup
odbcCloseAll()

## b] Additional variables ----

#Add residency status variables
alumni.data <- 
  alumni.data %>% 
  mutate(
       resStatusCountry = ifelse(Origin==Residency,"Home","Other"),
       resStatusRegion = ifelse(OriginRegion==ResidencyRegion,"Home","Other")
       )

#Add index variables - 
#1. define function to recode to number
#2. Select index constituents and apply function
#3. Calculate index (inc. replacing NA with 0) and join results back onto main dataset
recfct1 <- function(x) {as.numeric(factor(fct_recode(x,"4"="All the time","3"="Often","2"="Rarely","1"="Never"),levels = c("1","2","3","4"), ordered=T))}
recfct2 <- function(x) {as.numeric(factor(fct_recode(x,"1"="No","2"="Yes"), levels= c("1","2"), ordered=T))}

alumni.data <-  alumni.data %>% 
  select(AWDID,starts_with("App"),starts_with("Ldr"),starts_with("ResCollab"),starts_with("ResComp"), -AppExamples,-LdrGrade) %>% 
  mutate_at(vars(starts_with("App")), .funs=recfct1) %>%
  mutate_at(vars(starts_with("Ldr")), .funs=recfct2) %>%
  mutate_at(vars(starts_with("ResCollab")), .funs=recfct1) %>%
  mutate_at(vars(starts_with("ResComp")), .funs=recfct1) %>% 
  mutate(
    i.skills = round((select(.,starts_with("App")) %>% rowSums()-7)/21,2),
    i.ldr = round((select(.,starts_with("Ldr")) %>% rowSums()-4)/4,2),
    i.research = round((select(.,starts_with("ResCollab")) %>% rowSums()-3)/9,2),
    i.collab = round((select(.,starts_with("ResComp")) %>% rowSums()-3)/9,2)
  ) %>% 
  replace_na(list(i.skills=0,i.ldr = 0,i.research=0,i.collab=0)) %>% 
  select(AWDID,starts_with("i.")) %>% 
  left_join(alumni.data,by="AWDID")

#base indices
base.data <- base.data %>% 
  select(AWDID,starts_with("Ldr"),starts_with("ResCollab"),starts_with("ResComp"),-LdrGrade) %>% 
  mutate_at(vars(starts_with("Ldr")), .funs=recfct2) %>%
  mutate_at(vars(starts_with("ResCollab")), .funs=recfct1) %>%
  mutate_at(vars(starts_with("ResComp")), .funs=recfct1) %>% 
  mutate(
    i.ldr = round((select(.,starts_with("Ldr")) %>% rowSums()-4)/4,2),
    i.research = round((select(.,starts_with("ResCollab")) %>% rowSums()-3)/9,2),
    i.collab = round((select(.,starts_with("ResComp")) %>% rowSums()-3)/9,2)
  ) %>% 
  replace_na(list(i.ldr = 0,i.research=0,i.collab=0)) %>% 
  select(AWDID,starts_with("i.")) %>% 
  left_join(base.data,by="AWDID")


## c] Setting variables ----

# Set ordered factors - note: the ReturnIntention factor is ordered, but it is easier to set this in Section D because it also needs quite specifc recoding (see below)

recfct3 <- function(x) {parse_factor(x, levels = c("Never","Rarely","Often","All the time"), ordered=T)}
recfct4 <- function(x) {parse_factor(x, levels = c("Lower","About the same","Higher"), ordered=T)}
recfct5 <- function(x) {parse_factor(x, levels = c("No change","Slight change","Moderate change","Substantial change"), ordered=T)}
recfct6 <- function(x) {parse_factor(x, levels = c("5","4","3","2","1"), ordered=T)}

alumni.data <- alumni.data %>% 
  mutate_at(vars(CurrentSkillMatch, CurrentSalaryChange), .funs=recfct4) %>% 
  mutate_at(vars(starts_with("Skill")), .funs=recfct5) %>% 
  mutate_at(vars(matches("NetA|NetU|NetH|NetO|NetP|NetIn|InnLead|Cxt|AppS|AppA|AppT|AppD|AppM|ResC|TeachCMW")),.funs=recfct3) %>% 
  mutate_at(vars(ends_with("Grade")),.funs=recfct6) %>% 
  mutate(
     Scheme = parse_factor(Scheme, levels=c("CA","CD","CN","CS","CR","SS")),
     SurveyID = as.character(SurveyID),
     NetCourseImpact = as.character(NetCourseImpact),
     CurrentEmployUnemployed = parse_factor(CurrentEmployUnemployed, 
                               levels=c("Seeking employment","Caring for dependents full time","Retired","None of the above")),
     CurrentSector = parse_factor(CurrentSector, levels=c("Public","Private","NGO","Academic","Other")),
     Recommend = parse_factor(Recommend, levels=c("Definitely disagree", "Mostly disagree","Neither agree nor disagree",
                                                  "Mostly agree","Definitely agree","Don't know"), ordered=T),
     CurrentJobChange = parse_factor(CurrentJobChange, levels=c("Never","Once","Twice","Three or more times"),ordered=T),
     AcaQualCMWContribution = parse_factor(AcaQualCMWContribution, levels=c("Very unlikely","Unlikely","Likely", "Very likely"), ordered=T),
     ReturnIntention = factor(ReturnIntention),
     YearGroup = factor(YearGroup),
     OriginRegion = factor(OriginRegion),
     ResidencyRegion = factor(ResidencyRegion),
     resStatusCountry = factor(resStatusCountry),
     resStatusRegion = factor(resStatusRegion),
     SchemeType = factor(SchemeType)
     )

base.data <- base.data %>%
  mutate(
    SurveyID = as.character(SurveyID),
    StudyScholFunderSpecify = as.character(StudyScholFunderSpecify),
    CurrentStatusDetail = as.character(CurrentStatusDetail),
    BondDetail = as.character(BondDetail),
    BenefitShort = as.character(BenefitShort),
    BenefitLong = as.character(BenefitLong),
    Scheme = parse_factor(Scheme, levels=c("CA","CD","CN","CS","CR","SS")),
    PreSector = parse_factor(PreSector, levels=c("Public","Private","NGO","Academic","Other")),
    PreSupportiveness = parse_factor(PreSupportiveness, levels=c("Very unsupportive","Somewhat unsupportive","Neither supportive nor unsupportive",
                                                                 "Somewhat supportive","Very supportive"),ordered=T),
    PreSupportivenessDetail = as.character(PreSupportivenessDetail),
    CurrentJobChange = parse_factor(CurrentJobChange, levels=c("Never","Once","Twice","Three or more times"),ordered=T),
    StudyScholFunder = parse_factor(StudyScholFunder, levels=c("Domestic","International","University","Charity","Company","Supranational","Other")),
    OriginRegion = factor(OriginRegion),
    SchemeType = factor(SchemeType)
        ) %>% 
  mutate_at(vars(ends_with("Grade")),.funs=recfct6) %>%
  mutate_at(vars(matches("NetA|NetU|NetH|NetO|NetP|NetIn|InnLead|Cxt|ResC|TeachCMW")),.funs=recfct3) %>% 
  mutate_at(vars(CurrentSkillMatch), .funs=recfct4)


## d] Recoding factor levels ----

# Correcting various factor levels that have unwieldy names. Note: Worth preserving basic factor names for easier graphs.

alumni.data <- 
  alumni.data %>% 
  mutate(
    ReturnIntention=replace(as.character(ReturnIntention),grepl("^I never",ReturnIntention),"Never"),
    ReturnOrganisation = fct_recode(ReturnOrganisation,"Unemployed"="I was not employed before my Scholarship"),
    ResidencyReason = fct_recode(ResidencyReason, "Accompanied family member"="Accompanied a family member due to their employment", "Other"="Another reason"),
    ReturnEmploy = parse_factor(fct_recode(ReturnEmploy,
                                           "0-3 Months"="Within 0 - 3 months",
                                           "4-6 Months"="Within 4 - 6 months",
                                           "7-9 Months"="Within 7 - 9 months",
                                           "10-12 Months"="Within 10 - 12 months", 
                                           "13+ Months"="After 13+ months",
                                           "Not at all" = "Not at all"),
                                levels=c("0-3 Months","4-6 Months","7-9 Months","10-12 Months","13+ Months", "Not at all"), ordered=T),
    ReturnIntention = parse_factor(fct_recode(ReturnIntention,
                                              "0-2 Years"="I intend to remain abroad for between 0 and 2 more years",
                                              "3-4 Years"="I intend to remain abroad for between 3 and 4 further years",
                                              "5-10 Years"="I intend to remain abroad for between 5 and 10 further years",
                                              "10+ Years"="I intend to remain abroad for 10 or more years",
                                              "Never" = "Never"),
                                   levels=c("0-2 Years","3-4 Years","5-10 Years","10+ Years","Never"), ordered=T ),
    CurrentEmployUnemployed = fct_recode(CurrentEmployUnemployed, "Caring for dependents"="Caring for dependents full time", "Other"="None of the above"),
    SchemeNom = recode(Scheme, "CD"="Distance Learners",
                                "CR"="Agency: Developed",
                                "CA"="University Staff", 
                                "CS"="Agency: Developing",
                                "CN"="Split Site",
                                "SS"="Shared Scholars"),
    SchemeType = recode(SchemeType, "SS" = "Shared", "Distance Learning" = "Distance")
    )

base.data <- 
  base.data %>% 
  mutate(
    CurrentStatus = fct_recode(CurrentStatus, "Paid leave"="I am on paid leave (full- or part-pay), my job will be held for me",
                               "Unpaid leave"="I am on unpaid leave, my job will be held for me",
                               "Resigned"="I have resigned from my job",
                               "Remain, full-time"="I remain employed at the same job full-time",
                               "Remain, part-time"="I remain employed at the same job part-time",
                               "Other"="None of the above"),
    CFUKFunding = fct_recode(CFUKFunding, "Combination of funding"="A combination of the above"),
    CFHomeFunding = fct_recode(CFHomeFunding, "Combination of funding"="A combination of the above"),
    CFOtherFunding = fct_recode(CFOtherFunding, "Combination of funding"="A combination of the above"),
    PreEmployUnemployed = fct_recode(PreEmployUnemployed, "Caring for dependents"="Caring for dependents full time", "Other"="None of the above"),
    SchemeNom = recode(Scheme, "CD"="Distance Learners",
                       "CR"="Agency: Developed",
                       "CA"="University Staff", 
                       "CS"="Agency: Developing",
                       "CN"="Split Site",
                       "SS"="Shared Scholars"),
    SchemeType = recode(SchemeType, "SS" = "Shared","Distance Learning" = "Distance")
  )

response.data <- 
  response.data %>% 
    mutate(SchemeNom = recode(Scheme, "CD"="Distance Learners","CR"="Agency: Developed","CA"="University Staff",
                              "CS"="Agency: Developing","CN"="Split Site","SS"="Shared Scholars") ) %>% 
    select(-Scheme, Response=ResponseName, OriginRegion=Region)

## e] Cleanup ----

rm(list= ls()[!(ls() %in% c("opar","alumni.data","base.data","response.data"))]) #remove everything that doesn't match this list
gc() #clean the memory


# 2] Data Analysis ----

# --- Functions ----

score_summary <- function(dataframe){
  summarise(
    filter(dataframe,!SchemeNom=="Shared Scholars", !SchemeNom=="Distance Learners"),
    Freq = n(),
    Median = round(median(ZCtteeScore, na.rm=T),2),
    Mean = round(mean(ZCtteeScore, na.rm=T),2), 
    SD = round(sd(ZCtteeScore, na.rm=T),2), 
    Max = round(max(ZCtteeScore, na.rm=T),2),
    Min = round(min(ZCtteeScore, na.rm=T),2) ) %>% 
    rownames_to_column() %>% 
    mutate(rowname=colnames(.)[2]) %>% 
    rename_(Response = colnames(.)[2]) %>% 
    select(Variable = rowname, everything())
} #Statistical summary for committee scores, group data first to yield useful comparisons. In long data format.

pop_summary <- function(dataframe, variable){
  dataframe %>% 
    group_by_(variable) %>% 
    summarise_(freq = ~n()) %>% 
    mutate(prop = round((freq / sum(freq))*100,1)) %>% 
    rownames_to_column() %>%
    mutate(rowname=colnames(.)[2]) %>% 
    rename_(Response = variable) %>% 
    select(Variable = rowname, everything()) 
} #population level summary for a variable, in long data format

subgroup_summary <- function(dataframe, group, variable){
  dataframe %>% 
    group_by_(group, variable) %>% 
    summarise_(freq = ~n()) %>% 
    mutate(prop = round((freq / sum(freq))*100,1)) %>% 
    rownames_to_column() %>%
    mutate(rowname=colnames(.)[3]) %>% 
    rename_(Response = variable) %>% 
    select(Variable = rowname, everything()) 
} #subgroup summary for a variable: takes group as the second argument - in long data format

# --- Dataset structure ----

str(alumni.data, list.len=nrow(alumni.data))

# VARIABLE SUFFIXES:
# Gender = gender
# Scheme = sch
# Scheme type = schtype
# YearGroup = year
# Origin region = orireg
# Residency region = resreg
# JACS category = jacs. Note: throughout, JACs categories included in dataframes are limited to those with 20+ cases. JACs = NA is usually excluded.

# --- Survey response rates ----

resp_overall <- pop_summary(response.data,~Response) %>% select(-Variable,Rate=prop)

resp_gender <- subgroup_summary(response.data,~Gender,~Response) %>% select(-Variable,Rate=prop)
resp_sch <- subgroup_summary(response.data,~SchemeNom,~Response) %>% select(-Variable,Rate=prop)
resp_schtype <- subgroup_summary(response.data,~SchemeType,~Response) %>% select(-Variable,Rate=prop)
resp_YearGroup <- subgroup_summary(response.data,~YearGroup,~Response) %>% select(-Variable,Rate=prop)
resp_orireg <- subgroup_summary(response.data,~OriginRegion,~Response) %>% select(-Variable,Rate=prop)
resp_ctry <- subgroup_summary(response.data,~Origin,~Response) %>% select(-Variable,Rate=prop)
resp_jacs <- subgroup_summary(response.data,~JacsCat,~Response) %>% select(-Variable,Rate=prop) %>% filter(!JacsCat=="NA", sum(freq)>20)
resp_score <- response.data %>% group_by(Response) %>% score_summary()


## Data overview ----
overview_gender <- pop_summary(alumni.data,~Gender) %>% arrange(desc(prop))
overview_sch <- pop_summary(alumni.data,~SchemeNom) %>% arrange(desc(prop))
overview_schtype <- pop_summary(alumni.data,~SchemeType) %>% arrange(desc(prop))
overview_year <- pop_summary(alumni.data,~YearGroup) %>% arrange(desc(prop))
overview_orireg <- pop_summary(alumni.data,~OriginRegion) %>% arrange(desc(prop))
overview_resreg <- pop_summary(alumni.data,~ResidencyRegion) %>% arrange(desc(prop))
overview_jacs <- pop_summary(alumni.data,~JacsCat) %>% arrange(desc(prop))
overview_score <- alumni.data %>% group_by(CtteeGroup) %>% score_summary()


## Residency ----

# prefix = 'res'

#overall
rescountry_overall <- pop_summary(alumni.data,~resStatusCountry)
resregion_overall <- pop_summary(alumni.data,~resStatusRegion)
resreason_overall <- alumni.data %>% filter(!ResidencyReason=="NA") %>%  pop_summary(~ResidencyReason) %>% arrange(desc(prop))

# Residency status (home country)
rescountry_gender <- subgroup_summary(alumni.data,~Gender,~resStatusCountry)
rescountry_sch <- subgroup_summary(alumni.data,~SchemeNom,~resStatusCountry)
rescountry_schtype <- subgroup_summary(alumni.data,~SchemeType,~resStatusCountry)
rescountry_year <- subgroup_summary(alumni.data,~YearGroup,~resStatusCountry)
rescountry_orireg <- subgroup_summary(alumni.data,~OriginRegion,~resStatusCountry)
rescountry_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~resStatusCountry)
rescountry_jacs <- subgroup_summary(alumni.data,~JacsCat,~resStatusCountry) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescountry_score <- alumni.data %>% group_by(resStatusCountry) %>% score_summary()

# Residency status (home region)
resregion_gender <- subgroup_summary(alumni.data,~Gender,~resStatusRegion)
resregion_sch <- subgroup_summary(alumni.data,~SchemeNom,~resStatusRegion)
resregion_schtype <- subgroup_summary(alumni.data,~SchemeType,~resStatusRegion)
resregion_year <- subgroup_summary(alumni.data,~YearGroup,~resStatusRegion)
resregion_orireg <- subgroup_summary(alumni.data,~OriginRegion,~resStatusRegion)
resregion_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~resStatusRegion)
resregion_jacs <- subgroup_summary(alumni.data,~JacsCat,~resStatusRegion) %>% filter(!JacsCat=="NA", sum(freq)>20)
resregion_score <- alumni.data %>% group_by(resStatusRegion) %>% score_summary()

## Employment ----

# prefix = 'emp'
# TO ADD - CONTEXT VARIABLES

# Overall
empcurrent_overall <- pop_summary(alumni.data,~CurrentEmploy)
empsector_overall <- pop_summary(alumni.data,~CurrentSector)
empskillmatch_overall <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% pop_summary(~CurrentSkillMatch)
empchange_overall <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% pop_summary(~CurrentJobChange)
empreturnorg_overall <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% pop_summary(~ReturnOrganisation)
empreturntime_overall <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% pop_summary(~ReturnEmploy)

# Current employment - limited to 'Employed' and 'Studying' for ease of viewing
empcurrent_gender <- subgroup_summary(alumni.data,~Gender,~CurrentEmploy)
empcurrent_sch <- subgroup_summary(alumni.data,~SchemeNom,~CurrentEmploy)
empcurrent_schtype <- subgroup_summary(alumni.data,~SchemeType,~CurrentEmploy)
empcurrent_year <- subgroup_summary(alumni.data,~YearGroup,~CurrentEmploy)
empcurrent_orireg <- subgroup_summary(alumni.data,~OriginRegion,~CurrentEmploy)
empcurrent_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~CurrentEmploy)
empcurrent_jacs <- subgroup_summary(alumni.data,~JacsCat,~CurrentEmploy) %>% filter(!JacsCat=="NA", sum(freq)>20)
empcurrent_score <- alumni.data %>% group_by(CurrentEmploy) %>% score_summary

# Current employment sector
empsector_gender <- subgroup_summary(alumni.data,~Gender,~CurrentSector)
empsector_sch <- subgroup_summary(alumni.data,~SchemeNom,~CurrentSector)
empsector_schtype <- subgroup_summary(alumni.data,~SchemeType,~CurrentSector)
empsector_year <- subgroup_summary(alumni.data,~YearGroup,~CurrentSector)
empsector_orireg <- subgroup_summary(alumni.data,~OriginRegion,~CurrentSector)
empsector_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~CurrentSector)
empsector_jacs <- subgroup_summary(alumni.data,~JacsCat,~CurrentSector) %>% filter(!JacsCat=="NA", sum(freq)>20)
empsector_score <- alumni.data %>% group_by(CurrentSector) %>% score_summary

# Current employment skill level match
empskillmatch_gender <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~Gender,~CurrentSkillMatch)
empskillmatch_sch <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~SchemeNom,~CurrentSkillMatch)
empskillmatch_schtype <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~SchemeType,~CurrentSkillMatch)
empskillmatch_year <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~YearGroup,~CurrentSkillMatch)
empskillmatch_orireg <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~OriginRegion,~CurrentSkillMatch)
empskillmatch_resreg <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~ResidencyRegion,~CurrentSkillMatch)
empskillmatch_jacs <- alumni.data %>% filter(!JacsCat=="NA",!CurrentSkillMatch=="NA") %>% subgroup_summary(~JacsCat,~CurrentSkillMatch) %>% filter(sum(freq)>20)
empskillmatch_score <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(CurrentSkillMatch) %>% score_summary()

# Job changes in the last 2 years
empchange_gender <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~Gender,~CurrentJobChange)
empchange_sch <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~SchemeNom,~CurrentJobChange)
empchange_schtype <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~SchemeType,~CurrentJobChange)
empchange_year <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~YearGroup,~CurrentJobChange)
empchange_orireg <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~OriginRegion,~CurrentJobChange)
empchange_resreg <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~ResidencyRegion,~CurrentJobChange)
empchange_jacs <- alumni.data %>% filter(!JacsCat=="NA",!CurrentJobChange=="NA") %>% subgroup_summary(~JacsCat,~CurrentJobChange) %>% filter(sum(freq)>20)
empchange_score <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(CurrentJobChange) %>% score_summary()

# Return to previous organisation for +2 year respondents only (note 'YearGroup' is missing as a variable because it is meaningless: all are +2 group)
empreturnorg_gender <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% subgroup_summary(~Gender,~ReturnOrganisation)
empreturnorg_sch <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% subgroup_summary(~SchemeNom,~ReturnOrganisation)
empreturnorg_schtype <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% subgroup_summary(~SchemeType,~ReturnOrganisation)
empreturnorg_orireg <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% subgroup_summary(~OriginRegion,~ReturnOrganisation)
empreturnorg_resreg <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% subgroup_summary(~ResidencyRegion,~ReturnOrganisation)
empreturnorg_jacs <- alumni.data %>% filter(grepl("_Two$", SurveyID),!JacsCat=="NA") %>% subgroup_summary(~JacsCat,~ReturnOrganisation) %>% filter(sum(freq)>20)
empreturnorg_score <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(ReturnOrganisation) %>% score_summary()

# Time to gain employment for those +2 year respondents who did not return to their former organistion (N quite small for some groups, not very meaningful)
# Define a helper function for subsetting (note: subset is easier to use here than Dplyr's filter)
subset_returntime <- function(dataframe){
  subset(dataframe, grepl("_Two$", SurveyID) &
           !ReturnOrganisation=="Yes" &
           CurrentEmploy=="Employed")}
empreturntime_gender <- alumni.data %>% subset_returntime() %>% subgroup_summary(~Gender,~ReturnEmploy)
empreturntime_sch <- alumni.data %>% subset_returntime() %>% subgroup_summary(~SchemeNom,~ReturnEmploy)
empreturntime_schtype <- alumni.data %>% subset_returntime() %>% subgroup_summary(~SchemeType,~ReturnEmploy)
empreturntime_orireg <- alumni.data %>% subset_returntime() %>% subgroup_summary(~OriginRegion,~ReturnEmploy)
empreturntime_resreg <- alumni.data %>% subset_returntime() %>% subgroup_summary(~ResidencyRegion,~ReturnEmploy)
empreturntime_jacs <- alumni.data %>% subset_returntime() %>% subgroup_summary(~JacsCat,~ReturnEmploy)
empreturntime_score <- alumni.data %>% subset_returntime() %>% group_by(ReturnEmploy) %>% score_summary()

## Further qualifications----

# prefix = "Aca"

#Overall
acapostaward_overall <- pop_summary(alumni.data, ~AcaPostaward)
acaquallevel_overall <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% pop_summary(~AcaQualLevel) %>% arrange(desc(prop))
acacontribution_overall <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% pop_summary(~AcaQualCMWContribution)

#Gender
acapostaward_gender <- subgroup_summary(alumni.data, ~Gender,~AcaPostaward)
acaquallevel_gender <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~Gender,~AcaQualLevel)
acacontribution_gender <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~Gender, ~AcaQualCMWContribution)

#Scheme
acapostaward_sch <- subgroup_summary(alumni.data, ~SchemeNom,~AcaPostaward)
acaquallevel_sch <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeNom,~AcaQualLevel)
acacontribution_sch <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeNom, ~AcaQualCMWContribution)

#Scheme Type
acapostaward_schtype <- subgroup_summary(alumni.data, ~SchemeType,~AcaPostaward)
acaquallevel_schtype <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeType,~AcaQualLevel)
acacontribution_schtype <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeType, ~AcaQualCMWContribution)

#Year Group
acapostaward_year <- subgroup_summary(alumni.data, ~YearGroup,~AcaPostaward)
acaquallevel_year <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~YearGroup,~AcaQualLevel)
acacontribution_year <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~YearGroup, ~AcaQualCMWContribution)

#Origin Region
acapostaward_orireg <- subgroup_summary(alumni.data, ~OriginRegion,~AcaPostaward)
acaquallevel_orireg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~OriginRegion,~AcaQualLevel)
acacontribution_orireg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~OriginRegion, ~AcaQualCMWContribution)

#Residency Region
acapostaward_resreg <- subgroup_summary(alumni.data, ~ResidencyRegion,~AcaPostaward)
acaquallevel_resreg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~ResidencyRegion,~AcaQualLevel)
acacontribution_resreg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~ResidencyRegion, ~AcaQualCMWContribution)

#Subect Studied
acapostaward_jacs <- subgroup_summary(alumni.data, ~JacsCat,~AcaPostaward) %>% filter(!JacsCat=="NA", sum(freq)>20)
acaquallevel_jacs <- alumni.data %>% filter(!is.na(AcaQualLevel),!JacsCat=="NA") %>% subgroup_summary(~JacsCat,~AcaQualLevel)
acacontribution_jacs <- alumni.data %>% filter(!is.na(AcaQualLevel),!JacsCat=="NA") %>% subgroup_summary(~JacsCat, ~AcaQualCMWContribution)

#Committee Score
acapostaward_score <- alumni.data %>% group_by(AcaPostaward) %>% score_summary()
acaquallevel_score <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% group_by(AcaQualLevel) %>% score_summary()
acacontribution_score <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% group_by(AcaQualCMWContribution) %>% score_summary()

## Leadership ----

# prefix = "ldr" and "Inn"
# LEADERSHIP QUESTIONS, LEADERSHIP INDEX, INNOVATION QUESTIONS

# Overall
ldrbudget_overall <- pop_summary(alumni.data,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_overall<- pop_summary(alumni.data,~LdrSupervising)
ldrmanaging_overall <- pop_summary(alumni.data,~LdrManaging)
ldrstrategy_overall <- pop_summary(alumni.data,~LdrStrategy)
ldrinnproject_overall <- pop_summary(alumni.data,~InnLeadProject)
ldrinnfunding_overall <- pop_summary(alumni.data,~InnLeadFunding) 
ldrinnstartup_overall <- pop_summary(alumni.data,~InnStartup)

#Gender
ldrbudget_gender <- subgroup_summary(alumni.data,~Gender,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_gender <- subgroup_summary(alumni.data,~Gender,~LdrSupervising)
ldrmanaging_gender <- subgroup_summary(alumni.data,~Gender, ~LdrManaging)
ldrstrategy_gender <- subgroup_summary(alumni.data,~Gender,~LdrStrategy)
ldrinnproject_gender <- subgroup_summary(alumni.data,~Gender,~InnLeadProject)
ldrinnfunding_gender <- subgroup_summary(alumni.data,~Gender,~InnLeadFunding) 
ldrinnstartup_gender <- subgroup_summary(alumni.data,~Gender,~InnStartup)

#Scheme
ldrbudget_sch <- subgroup_summary(alumni.data,~SchemeNom,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_sch <- subgroup_summary(alumni.data,~SchemeNom,~LdrSupervising)
ldrmanaging_sch <- subgroup_summary(alumni.data,~SchemeNom, ~LdrManaging)
ldrstrategy_sch <- subgroup_summary(alumni.data,~SchemeNom,~LdrStrategy)
ldrinnproject_sch <- subgroup_summary(alumni.data,~SchemeNom,~InnLeadProject)
ldrinnfunding_sch <- subgroup_summary(alumni.data,~SchemeNom,~InnLeadFunding) 
ldrinnstartup_sch <- subgroup_summary(alumni.data,~SchemeNom,~InnStartup)

#Scheme Type
ldrbudget_schtype <- subgroup_summary(alumni.data,~SchemeType,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_schtype <- subgroup_summary(alumni.data,~SchemeType,~LdrSupervising)
ldrmanaging_schtype <- subgroup_summary(alumni.data,~SchemeType, ~LdrManaging)
ldrstrategy_schtype <- subgroup_summary(alumni.data,~SchemeType,~LdrStrategy)
ldrinnproject_schtype <- subgroup_summary(alumni.data,~SchemeType,~InnLeadProject)
ldrinnfunding_schtype <- subgroup_summary(alumni.data,~SchemeType,~InnLeadFunding) 
ldrinnstartup_schtype <- subgroup_summary(alumni.data,~SchemeType,~InnStartup)

#Year group
ldrbudget_year <- subgroup_summary(alumni.data,~YearGroup,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_year <- subgroup_summary(alumni.data,~YearGroup,~LdrSupervising)
ldrmanaging_year <- subgroup_summary(alumni.data,~YearGroup, ~LdrManaging)
ldrstrategy_year <- subgroup_summary(alumni.data,~YearGroup,~LdrStrategy)
ldrinnproject_year <- subgroup_summary(alumni.data,~YearGroup,~InnLeadProject)
ldrinnfunding_year <- subgroup_summary(alumni.data,~YearGroup,~InnLeadFunding) 
ldrinnstartup_year <- subgroup_summary(alumni.data,~YearGroup,~InnStartup)

#Origin region
ldrbudget_orireg <- subgroup_summary(alumni.data,~OriginRegion,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_orireg <- subgroup_summary(alumni.data,~OriginRegion,~LdrSupervising)
ldrmanaging_orireg <- subgroup_summary(alumni.data,~OriginRegion, ~LdrManaging)
ldrstrategy_orireg <- subgroup_summary(alumni.data,~OriginRegion,~LdrStrategy)
ldrinnproject_orireg <- subgroup_summary(alumni.data,~OriginRegion,~InnLeadProject)
ldrinnfunding_orireg <- subgroup_summary(alumni.data,~OriginRegion,~InnLeadFunding) 
ldrinnstartup_orireg <- subgroup_summary(alumni.data,~OriginRegion,~InnStartup)

#Residency region
ldrbudget_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~LdrSupervising)
ldrmanaging_resreg <- subgroup_summary(alumni.data,~ResidencyRegion, ~LdrManaging)
ldrstrategy_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~LdrStrategy)
ldrinnproject_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~InnLeadProject)
ldrinnfunding_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~InnLeadFunding) 
ldrinnstartup_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~InnStartup)

#Subject studied
ldrbudget_jacs <- subgroup_summary(alumni.data,~JacsCat,~LdrBudget) %>% filter(!JacsCat=="NA", sum(freq)>20) #NA = question not asked: not currently employed
ldrsupervise_jacs <- subgroup_summary(alumni.data,~JacsCat,~LdrSupervising) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrmanaging_jacs <- subgroup_summary(alumni.data,~JacsCat, ~LdrManaging) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrstrategy_jacs <- subgroup_summary(alumni.data,~JacsCat,~LdrStrategy) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrinnproject_jacs <- subgroup_summary(alumni.data,~JacsCat,~InnLeadProject) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrinnfunding_jacs <- subgroup_summary(alumni.data,~JacsCat,~InnLeadFunding)  %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrinnstartup_jacs <- subgroup_summary(alumni.data,~JacsCat,~InnStartup) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee score
ldrbudget_score <- alumni.data %>% group_by(LdrBudget) %>% score_summary()
ldrsupervise_score <- alumni.data %>% group_by(LdrSupervising) %>% score_summary()
ldrmanaging_score <- alumni.data %>% group_by(LdrManaging) %>% score_summary()
ldrstrategy_score <- alumni.data %>% group_by(LdrStrategy) %>% score_summary()
ldrinnproject_score <- alumni.data %>% group_by(InnLeadProject) %>% score_summary()
ldrinnfunding_score <- alumni.data %>% group_by(InnLeadFunding) %>% score_summary()
ldrinnstartup_score <- alumni.data %>% group_by(InnStartup) %>% score_summary()



## Skill application----

# prefix = "App" and "Skill"

#Overall

## Skill gain questions - only asked of +2 year follow up group (_Two)
skillrestech_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillResearchTechniques)
skillresfield_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillResearchField)
skillcritical_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillCritical)
skilltechnical_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillTechnical)
skillleadership_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillLeadership)
skilldisseminate_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillDisseminate)
skillinfluence_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillInfluence)
skillethical_overall <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% pop_summary(~SkillEthical)


## Application of skills, asked to all alumni survey participants
appskillwork_overall <- pop_summary(alumni.data,~AppSkillWork)
appskillnonwork_overall <- pop_summary(alumni.data,~AppSkillNonwork)
appapproach_overall <- pop_summary(alumni.data,~AppApproachProblem)
apptrain_overall <- pop_summary(alumni.data,~AppTrainColleagues)
appresources_overall <- pop_summary(alumni.data,~AppDevelopResources)
appadvocate_overall <- pop_summary(alumni.data,~AppAdvocateChange)
appchange_overall <- pop_summary(alumni.data,~AppMakeChange)

#Gender
## Skill gain
skillrestech_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillResearchTechniques)
skillresfield_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillResearchField)
skillcritical_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillCritical)
skilltechnical_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillTechnical)
skillleadership_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillLeadership)
skilldisseminate_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillDisseminate)
skillinfluence_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillInfluence)
skillethical_gender <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~Gender,~SkillEthical)

## Application of skills
appskillwork_gender <- subgroup_summary(alumni.data,~Gender,~AppSkillWork)
appskillnonwork_gender <- subgroup_summary(alumni.data,~Gender,~AppSkillNonwork)
appapproach_gender <- subgroup_summary(alumni.data,~Gender,~AppApproachProblem)
apptrain_gender <- subgroup_summary(alumni.data,~Gender,~AppTrainColleagues)
appresources_gender <- subgroup_summary(alumni.data,~Gender,~AppDevelopResources)
appadvocate_gender <- subgroup_summary(alumni.data,~Gender,~AppAdvocateChange)
appchange_gender <- subgroup_summary(alumni.data,~Gender,~AppMakeChange)

#Scheme
## Skill gain
skillrestech_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillResearchTechniques)
skillresfield_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillResearchField)
skillcritical_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillCritical)
skilltechnical_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillTechnical)
skillleadership_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillLeadership)
skilldisseminate_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillDisseminate)
skillinfluence_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillInfluence)
skillethical_sch <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeNom,~SkillEthical)

## Application of skills
appskillwork_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppSkillWork)
appskillnonwork_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppSkillNonwork)
appapproach_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppApproachProblem)
apptrain_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppTrainColleagues)
appresources_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppDevelopResources)
appadvocate_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppAdvocateChange)
appchange_sch <- subgroup_summary(alumni.data,~SchemeNom,~AppMakeChange)

#Scheme Type
## Skill gain
skillrestech_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillResearchTechniques)
skillresfield_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillResearchField)
skillcritical_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillCritical)
skilltechnical_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillTechnical)
skillleadership_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillLeadership)
skilldisseminate_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillDisseminate)
skillinfluence_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillInfluence)
skillethical_schtype <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~SchemeType,~SkillEthical)

## Application of skills
appskillwork_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppSkillWork)
appskillnonwork_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppSkillNonwork)
appapproach_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppApproachProblem)
apptrain_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppTrainColleagues)
appresources_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppDevelopResources)
appadvocate_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppAdvocateChange)
appchange_schtype <- subgroup_summary(alumni.data,~SchemeType,~AppMakeChange)

#Year Group
## Skill gain - not meaningful for Year group: question is only asked to +2 year (_Two) group

## Application of skills
appskillwork_year <- subgroup_summary(alumni.data,~YearGroup,~AppSkillWork)
appskillnonwork_year <- subgroup_summary(alumni.data,~YearGroup,~AppSkillNonwork)
appapproach_year <- subgroup_summary(alumni.data,~YearGroup,~AppApproachProblem)
apptrain_year <- subgroup_summary(alumni.data,~YearGroup,~AppTrainColleagues)
appresources_year <- subgroup_summary(alumni.data,~YearGroup,~AppDevelopResources)
appadvocate_year <- subgroup_summary(alumni.data,~YearGroup,~AppAdvocateChange)
appchange_year <- subgroup_summary(alumni.data,~YearGroup,~AppMakeChange)

#Origin Region
## Skill gain
skillrestech_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillResearchTechniques)
skillresfield_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillResearchField)
skillcritical_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillCritical)
skilltechnical_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillTechnical)
skillleadership_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillLeadership)
skilldisseminate_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillDisseminate)
skillinfluence_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillInfluence)
skillethical_orireg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~OriginRegion,~SkillEthical)

## Application of skills
appskillwork_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppSkillWork)
appskillnonwork_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppSkillNonwork)
appapproach_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppApproachProblem)
apptrain_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppTrainColleagues)
appresources_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppDevelopResources)
appadvocate_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppAdvocateChange)
appchange_orireg <- subgroup_summary(alumni.data,~OriginRegion,~AppMakeChange)

#Residency Region
## Skill gain
skillrestech_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillResearchTechniques)
skillresfield_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillResearchField)
skillcritical_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillCritical)
skilltechnical_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillTechnical)
skillleadership_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillLeadership)
skilldisseminate_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillDisseminate)
skillinfluence_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillInfluence)
skillethical_resreg <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~ResidencyRegion,~SkillEthical)

## Application of skills
appskillwork_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppSkillWork)
appskillnonwork_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppSkillNonwork)
appapproach_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppApproachProblem)
apptrain_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppTrainColleagues)
appresources_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppDevelopResources)
appadvocate_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppAdvocateChange)
appchange_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~AppMakeChange)

#Subect Studied
## Skill gain
skillrestech_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillResearchTechniques) %>% filter(!JacsCat=="NA", sum(freq)>20)
skillresfield_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillResearchField) %>% filter(!JacsCat=="NA", sum(freq)>20)
skillcritical_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillCritical) %>% filter(!JacsCat=="NA", sum(freq)>20)
skilltechnical_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillTechnical) %>% filter(!JacsCat=="NA", sum(freq)>20)
skillleadership_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillLeadership) %>% filter(!JacsCat=="NA", sum(freq)>20)
skilldisseminate_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillDisseminate) %>% filter(!JacsCat=="NA", sum(freq)>20)
skillinfluence_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillInfluence) %>% filter(!JacsCat=="NA", sum(freq)>20)
skillethical_jacs <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% subgroup_summary(~JacsCat,~SkillEthical) %>% filter(!JacsCat=="NA", sum(freq)>20)

## Application of skills
appskillwork_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppSkillWork) %>% filter(!JacsCat=="NA", sum(freq)>20)
appskillnonwork_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppSkillNonwork) %>% filter(!JacsCat=="NA", sum(freq)>20)
appapproach_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppApproachProblem) %>% filter(!JacsCat=="NA", sum(freq)>20)
apptrain_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppTrainColleagues) %>% filter(!JacsCat=="NA", sum(freq)>20)
appresources_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppDevelopResources) %>% filter(!JacsCat=="NA", sum(freq)>20)
appadvocate_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppAdvocateChange) %>% filter(!JacsCat=="NA", sum(freq)>20)
appchange_jacs <- subgroup_summary(alumni.data,~JacsCat,~AppMakeChange) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
## Skill gain
skillrestech_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillResearchTechniques) %>% score_summary()
skillresfield_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillResearchField) %>% score_summary()
skillcritical_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillCritical) %>% score_summary()
skilltechnical_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillTechnical) %>% score_summary()
skillleadership_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillLeadership) %>% score_summary()
skilldisseminate_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillDisseminate) %>% score_summary()
skillinfluence_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillInfluence) %>% score_summary()
skillethical_score <- alumni.data %>% filter(grepl("_Two",SurveyID)) %>% group_by(SkillEthical) %>% score_summary()

## Application of skills
appskillwork_score <- alumni.data %>% group_by(AppSkillWork) %>% score_summary()
appskillnonwork_score <- alumni.data %>% group_by(AppSkillNonwork) %>% score_summary()
appapproach_score <- alumni.data %>% group_by(AppApproachProblem) %>% score_summary()
apptrain_score <- alumni.data %>% group_by(AppTrainColleagues) %>% score_summary()
appresources_score <- alumni.data %>% group_by(AppDevelopResources) %>% score_summary()
appadvocate_score <- alumni.data %>% group_by(AppAdvocateChange) %>% score_summary()
appchange_score <- alumni.data %>% group_by(AppMakeChange) %>% score_summary()

## Research ----

# Prefix = "res"
# Note that there are usually many NA responses for research variables because these questions are only asked to thsoe that report current involvement in research
# Filter question for research involvement is 'ResMain'
# NA for ResMain is because unemployed and studying respondents are not presented with the ResMain question

#Overall
resmain_overall <- pop_summary(alumni.data, ~ResMain) 

rescollabauthor_overall <- pop_summary(alumni.data,~ResCollabAuthor)
rescollabgrant_overall <- pop_summary(alumni.data,~ResCollabGrant)
rescollabconf_overall <- pop_summary(alumni.data,~ResCollabConf)

reseditor_overall <- pop_summary(alumni.data, ~ResEditor)
resreviewer_overall <- pop_summary(alumni.data, ~ResReviewer)
resorganiser_overall <- pop_summary(alumni.data, ~ResOrganiser)

rescomparticles_overall <- pop_summary(alumni.data, ~ResCompArticles)
rescompprojects_overall <- pop_summary(alumni.data, ~ResCompProjects)
rescompfunding_overall <- pop_summary(alumni.data, ~ResCompFunding)

#Gender
resmain_gender <- subgroup_summary(alumni.data,~Gender,~ResMain) 

rescollabauthor_gender <- subgroup_summary(alumni.data,~Gender,~ResCollabAuthor)
rescollabgrant_gender <- subgroup_summary(alumni.data,~Gender,~ResCollabGrant)
rescollabconf_gender <- subgroup_summary(alumni.data,~Gender,~ResCollabConf)

reseditor_gender <- subgroup_summary(alumni.data,~Gender,~ResEditor)
resreviewer_gender <- subgroup_summary(alumni.data,~Gender,~ResReviewer)
resorganiser_gender <- subgroup_summary(alumni.data,~Gender,~ResOrganiser)

rescomparticles_gender <- subgroup_summary(alumni.data,~Gender,~ResCompArticles)
rescompprojects_gender <- subgroup_summary(alumni.data,~Gender,~ResCompProjects)
rescompfunding_gender <- subgroup_summary(alumni.data,~Gender,~ResCompFunding)

#Scheme
resmain_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResMain) 

rescollabauthor_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResCollabAuthor)
rescollabgrant_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResCollabGrant)
rescollabconf_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResCollabConf)

reseditor_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResEditor)
resreviewer_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResReviewer)
resorganiser_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResOrganiser)

rescomparticles_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResCompArticles)
rescompprojects_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResCompProjects)
rescompfunding_sch <- subgroup_summary(alumni.data,~SchemeNom,~ResCompFunding)

#Scheme Type
resmain_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResMain) 

rescollabauthor_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResCollabAuthor)
rescollabgrant_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResCollabGrant)
rescollabconf_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResCollabConf)

reseditor_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResEditor)
resreviewer_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResReviewer)
resorganiser_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResOrganiser)

rescomparticles_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResCompArticles)
rescompprojects_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResCompProjects)
rescompfunding_schtype <- subgroup_summary(alumni.data,~SchemeType,~ResCompFunding)

#Year Group
resmain_year <- subgroup_summary(alumni.data,~YearGroup,~ResMain) 

rescollabauthor_year <- subgroup_summary(alumni.data,~YearGroup,~ResCollabAuthor)
rescollabgrant_year <- subgroup_summary(alumni.data,~YearGroup,~ResCollabGrant)
rescollabconf_year <- subgroup_summary(alumni.data,~YearGroup,~ResCollabConf)

reseditor_year <- subgroup_summary(alumni.data,~YearGroup,~ResEditor)
resreviewer_year <- subgroup_summary(alumni.data,~YearGroup,~ResReviewer)
resorganiser_year <- subgroup_summary(alumni.data,~YearGroup,~ResOrganiser)

rescomparticles_year <- subgroup_summary(alumni.data,~YearGroup,~ResCompArticles)
rescompprojects_year <- subgroup_summary(alumni.data,~YearGroup,~ResCompProjects)
rescompfunding_year <- subgroup_summary(alumni.data,~YearGroup,~ResCompFunding)

#Origin Region
resmain_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResMain) 

rescollabauthor_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResCollabAuthor)
rescollabgrant_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResCollabGrant)
rescollabconf_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResCollabConf)

reseditor_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResEditor)
resreviewer_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResReviewer)
resorganiser_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResOrganiser)

rescomparticles_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResCompArticles)
rescompprojects_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResCompProjects)
rescompfunding_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ResCompFunding)

#Residency Region
resmain_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResMain) 

rescollabauthor_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResCollabAuthor)
rescollabgrant_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResCollabGrant)
rescollabconf_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResCollabConf)

reseditor_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResEditor)
resreviewer_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResReviewer)
resorganiser_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResOrganiser)

rescomparticles_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResCompArticles)
rescompprojects_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResCompProjects)
rescompfunding_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ResCompFunding)

#Subect Studied
resmain_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResMain) %>% filter(!JacsCat=="NA", sum(freq)>20)

rescollabauthor_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResCollabAuthor) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescollabgrant_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResCollabGrant) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescollabconf_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResCollabConf) %>% filter(!JacsCat=="NA", sum(freq)>20)

reseditor_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResEditor) %>% filter(!JacsCat=="NA", sum(freq)>20)
resreviewer_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResReviewer) %>% filter(!JacsCat=="NA", sum(freq)>20)
resorganiser_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResOrganiser) %>% filter(!JacsCat=="NA", sum(freq)>20)

rescomparticles_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResCompArticles) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescompprojects_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResCompProjects) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescompfunding_jacs <- subgroup_summary(alumni.data,~JacsCat,~ResCompFunding) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
resmain_score <- alumni.data %>% group_by(ResMain) %>% score_summary()

rescollabauthor_score <- alumni.data %>% group_by(ResCollabAuthor) %>% score_summary()
rescollabgrant_score <- alumni.data %>% group_by(ResCollabGrant) %>% score_summary()
rescollabconf_score <- alumni.data %>% group_by(ResCollabConf) %>% score_summary()

reseditor_score <- alumni.data %>% group_by(ResEditor) %>% score_summary()
resreviewer_score <- alumni.data %>% group_by(ResReviewer) %>% score_summary()
resorganiser_score <- alumni.data %>% group_by(ResOrganiser) %>% score_summary()

rescomparticles_score <- alumni.data %>% group_by(ResCompArticles) %>% score_summary()
rescompprojects_score <- alumni.data %>% group_by(ResCompProjects) %>% score_summary()
rescompfunding_score <- alumni.data %>% group_by(ResCompFunding) %>% score_summary()


## Teaching ----

# prefix = "teach"

#Overall
teachmain_overall <- pop_summary(alumni.data,~TeachMain)
teachcmwskills_overall <- alumni.data %>% filter(TeachMain=="Yes") %>% pop_summary(~TeachCMWSkills)

teachschool_overall <- pop_summary(alumni.data,~TeachSchool)
teachundergrad_overall <- pop_summary(alumni.data,~TeachUndergrad)
teachpostgrad_overall <- pop_summary(alumni.data,~TeachPostgrad)
teachdoctorate_overall <- pop_summary(alumni.data,~TeachDoctorate)
teachtvet_overall <- pop_summary(alumni.data,~TeachTVET)


#Gender
teachmain_gender <- subgroup_summary(alumni.data,~Gender,~TeachMain)
teachcmwskills_gender <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~Gender,~TeachCMWSkills)

teachschool_gender <- subgroup_summary(alumni.data,~Gender,~TeachSchool)
teachundergrad_gender <- subgroup_summary(alumni.data,~Gender,~TeachUndergrad)
teachpostgrad_gender <- subgroup_summary(alumni.data,~Gender,~TeachPostgrad)
teachdoctorate_gender <- subgroup_summary(alumni.data,~Gender,~TeachDoctorate)
teachtvet_gender <- subgroup_summary(alumni.data,~Gender,~TeachTVET)

#Scheme
teachmain_sch <- subgroup_summary(alumni.data,~SchemeNom,~TeachMain)
teachcmwskills_sch <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~SchemeNom,~TeachCMWSkills)

teachschool_sch <- subgroup_summary(alumni.data,~SchemeNom,~TeachSchool)
teachundergrad_sch <- subgroup_summary(alumni.data,~SchemeNom,~TeachUndergrad)
teachpostgrad_sch <- subgroup_summary(alumni.data,~SchemeNom,~TeachPostgrad)
teachdoctorate_sch <- subgroup_summary(alumni.data,~SchemeNom,~TeachDoctorate)
teachtvet_sch <- subgroup_summary(alumni.data,~SchemeNom,~TeachTVET)

#Scheme Type
teachmain_schtype <- subgroup_summary(alumni.data,~SchemeType,~TeachMain)
teachcmwskills_schtype <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~SchemeType,~TeachCMWSkills)

teachschool_schtype <- subgroup_summary(alumni.data,~SchemeType,~TeachSchool)
teachundergrad_schtype <- subgroup_summary(alumni.data,~SchemeType,~TeachUndergrad)
teachpostgrad_schtype <- subgroup_summary(alumni.data,~SchemeType,~TeachPostgrad)
teachdoctorate_schtype <- subgroup_summary(alumni.data,~SchemeType,~TeachDoctorate)
teachtvet_schtype <- subgroup_summary(alumni.data,~SchemeType,~TeachTVET)

#Year Group
teachmain_year <- subgroup_summary(alumni.data,~YearGroup,~TeachMain)
teachcmwskills_year <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~YearGroup,~TeachCMWSkills)

teachschool_year <- subgroup_summary(alumni.data,~YearGroup,~TeachSchool)
teachundergrad_year <- subgroup_summary(alumni.data,~YearGroup,~TeachUndergrad)
teachpostgrad_year <- subgroup_summary(alumni.data,~YearGroup,~TeachPostgrad)
teachdoctorate_year <- subgroup_summary(alumni.data,~YearGroup,~TeachDoctorate)
teachtvet_year <- subgroup_summary(alumni.data,~YearGroup,~TeachTVET)

#Origin Region
teachmain_orireg <- subgroup_summary(alumni.data,~OriginRegion,~TeachMain)
teachcmwskills_orireg <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~OriginRegion,~TeachCMWSkills)

teachschool_orireg <- subgroup_summary(alumni.data,~OriginRegion,~TeachSchool)
teachundergrad_orireg <- subgroup_summary(alumni.data,~OriginRegion,~TeachUndergrad)
teachpostgrad_orireg <- subgroup_summary(alumni.data,~OriginRegion,~TeachPostgrad)
teachdoctorate_orireg <- subgroup_summary(alumni.data,~OriginRegion,~TeachDoctorate)
teachtvet_orireg <- subgroup_summary(alumni.data,~OriginRegion,~TeachTVET)

#Residency Region
teachmain_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~TeachMain)
teachcmwskills_resreg <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~ResidencyRegion,~TeachCMWSkills)

teachschool_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~TeachSchool)
teachundergrad_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~TeachUndergrad)
teachpostgrad_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~TeachPostgrad)
teachdoctorate_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~TeachDoctorate)
teachtvet_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~TeachTVET)

#Subect Studied
teachmain_jacs <- subgroup_summary(alumni.data,~JacsCat,~TeachMain) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachcmwskills_jacs <- alumni.data %>% filter(TeachMain=="Yes") %>% subgroup_summary(~JacsCat,~TeachCMWSkills) %>% filter(!JacsCat=="NA", sum(freq)>20)

teachschool_jacs <- subgroup_summary(alumni.data,~JacsCat,~TeachSchool) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachundergrad_jacs <- subgroup_summary(alumni.data,~JacsCat,~TeachUndergrad) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachpostgrad_jacs <- subgroup_summary(alumni.data,~JacsCat,~TeachPostgrad) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachdoctorate_jacs <- subgroup_summary(alumni.data,~JacsCat,~TeachDoctorate) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachtvet_jacs <- subgroup_summary(alumni.data,~JacsCat,~TeachTVET) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
teachmain_score <- alumni.data %>% group_by(TeachMain) %>% score_summary()
teachcmwskills_score <- alumni.data %>% filter(TeachMain=="Yes") %>% group_by(TeachCMWSkills) %>% score_summary()

teachschool_score <- alumni.data %>% group_by(TeachSchool) %>% score_summary()
teachundergrad_score <- alumni.data %>% group_by(TeachUndergrad) %>% score_summary()
teachpostgrad_score <- alumni.data %>% group_by(TeachPostgrad) %>% score_summary()
teachdoctorate_score <- alumni.data %>% group_by(TeachDoctorate) %>% score_summary()
teachtvet_score <- alumni.data %>% group_by(TeachTVET) %>% score_summary()


## Networks and links----

# prefix= 'Net'

#Overall
netacad_overall <- pop_summary(alumni.data,~NetAcad)
netuk_overall <- pop_summary(alumni.data,~NetUK)
nethome_overall <- pop_summary(alumni.data,~NetHome)
netother_overall <- pop_summary(alumni.data,~NetOther)
netpersonal_overall <- pop_summary(alumni.data,~NetPersonal)

netinfacad_overall <- pop_summary(alumni.data,~NetInfluenceAcad)
netinfuk_overall <- pop_summary(alumni.data,~NetInfluenceUK)
netinforigin_overall <- pop_summary(alumni.data,~NetInfluenceOrigin)
netinfother_overall <- pop_summary(alumni.data,~NetInfluenceOther)
netinfpersonal_overall <- pop_summary(alumni.data,~NetInfluencePersonal)

#Gender
netacad_gender <- subgroup_summary(alumni.data,~Gender,~NetAcad)
netuk_gender <- subgroup_summary(alumni.data,~Gender,~NetUK)
nethome_gender <- subgroup_summary(alumni.data,~Gender,~NetHome)
netother_gender <- subgroup_summary(alumni.data,~Gender,~NetOther)
netpersonal_gender <- subgroup_summary(alumni.data,~Gender,~NetPersonal)

netinfacad_gender <- subgroup_summary(alumni.data,~Gender,~NetInfluenceAcad)
netinfuk_gender <- subgroup_summary(alumni.data,~Gender,~NetInfluenceUK)
netinforigin_gender <- subgroup_summary(alumni.data,~Gender,~NetInfluenceOrigin)
netinfother_gender <- subgroup_summary(alumni.data,~Gender,~NetInfluenceOther)
netinfpersonal_gender <- subgroup_summary(alumni.data,~Gender,~NetInfluencePersonal)

#Scheme
netacad_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetAcad)
netuk_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetUK)
nethome_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetHome)
netother_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetOther)
netpersonal_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetPersonal)

netinfacad_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetInfluenceAcad)
netinfuk_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetInfluenceUK)
netinforigin_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetInfluenceOrigin)
netinfother_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetInfluenceOther)
netinfpersonal_sch <- subgroup_summary(alumni.data,~SchemeNom,~NetInfluencePersonal)

#Scheme Type
netacad_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetAcad)
netuk_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetUK)
nethome_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetHome)
netother_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetOther)
netpersonal_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetPersonal)

netinfacad_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetInfluenceAcad)
netinfuk_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetInfluenceUK)
netinforigin_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetInfluenceOrigin)
netinfother_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetInfluenceOther)
netinfpersonal_schtype <- subgroup_summary(alumni.data,~SchemeType,~NetInfluencePersonal)

#Year Group
netacad_year <- subgroup_summary(alumni.data,~YearGroup,~NetAcad)
netuk_year <- subgroup_summary(alumni.data,~YearGroup,~NetUK)
nethome_year <- subgroup_summary(alumni.data,~YearGroup,~NetHome)
netother_year <- subgroup_summary(alumni.data,~YearGroup,~NetOther)
netpersonal_year <- subgroup_summary(alumni.data,~YearGroup,~NetPersonal)

netinfacad_year <- subgroup_summary(alumni.data,~YearGroup,~NetInfluenceAcad)
netinfuk_year <- subgroup_summary(alumni.data,~YearGroup,~NetInfluenceUK)
netinforigin_year <- subgroup_summary(alumni.data,~YearGroup,~NetInfluenceOrigin)
netinfother_year <- subgroup_summary(alumni.data,~YearGroup,~NetInfluenceOther)
netinfpersonal_year <- subgroup_summary(alumni.data,~YearGroup,~NetInfluencePersonal)

#Origin Region
netacad_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetAcad)
netuk_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetUK)
nethome_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetHome)
netother_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetOther)
netpersonal_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetPersonal)

netinfacad_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetInfluenceAcad)
netinfuk_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetInfluenceUK)
netinforigin_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetInfluenceOrigin)
netinfother_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetInfluenceOther)
netinfpersonal_orireg <- subgroup_summary(alumni.data,~OriginRegion,~NetInfluencePersonal)

#Residency Region
netacad_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetAcad)
netuk_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetUK)
nethome_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetHome)
netother_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetOther)
netpersonal_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetPersonal)

netinfacad_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetInfluenceAcad)
netinfuk_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetInfluenceUK)
netinforigin_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetInfluenceOrigin)
netinfother_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetInfluenceOther)
netinfpersonal_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~NetInfluencePersonal)

#Subect Studied
netacad_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetAcad) %>% filter(!JacsCat=="NA", sum(freq)>20)
netuk_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetUK) %>% filter(!JacsCat=="NA", sum(freq)>20)
nethome_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetHome) %>% filter(!JacsCat=="NA", sum(freq)>20)
netother_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetOther) %>% filter(!JacsCat=="NA", sum(freq)>20)
netpersonal_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetPersonal) %>% filter(!JacsCat=="NA", sum(freq)>20)

netinfacad_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetInfluenceAcad) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinfuk_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetInfluenceUK) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinforigin_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetInfluenceOrigin) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinfother_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetInfluenceOther) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinfpersonal_jacs <- subgroup_summary(alumni.data,~JacsCat,~NetInfluencePersonal) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
netacad_score <- alumni.data %>% group_by(NetAcad) %>% score_summary()
netuk_score <- alumni.data %>% group_by(NetUK) %>% score_summary()
nethome_score <- alumni.data %>% group_by(NetHome) %>% score_summary()
netother_score <- alumni.data %>% group_by(NetOther) %>% score_summary()
netpersonal_score <- alumni.data %>% group_by(NetPersonal) %>% score_summary()

netinfacad_score <- alumni.data %>% group_by(NetInfluenceAcad) %>% score_summary()
netinfuk_score <- alumni.data %>% group_by(NetInfluenceUK) %>% score_summary()
netinforigin_score <- alumni.data %>% group_by(NetInfluenceOrigin) %>% score_summary()
netinfother_score <- alumni.data %>% group_by(NetInfluenceOther) %>% score_summary()
netinfpersonal_score <- alumni.data %>% group_by(NetInfluencePersonal) %>% score_summary()

## Broader impact----

# prefix = "Imp"

#Overall
impinstitutional_overall <- pop_summary(alumni.data,~ImpInstitutional)
implocal_overall <- pop_summary(alumni.data,~ImpLocal)
impnational_overall <- pop_summary(alumni.data,~ImpNational)
impinternational_overall <- pop_summary(alumni.data,~ImpInternational)

impsocial_overall <- pop_summary(alumni.data,~ImpSocial)
impcivic_overall <- pop_summary(alumni.data,~ImpCivic)
impecon_overall <- pop_summary(alumni.data,~ImpEcon)
imppolicy_overall <- pop_summary(alumni.data,~ImpPolicy)

#Gender
impinstitutional_gender <- subgroup_summary(alumni.data,~Gender,~ImpInstitutional)
implocal_gender <- subgroup_summary(alumni.data,~Gender,~ImpLocal)
impnational_gender <- subgroup_summary(alumni.data,~Gender,~ImpNational)
impinternational_gender <- subgroup_summary(alumni.data,~Gender,~ImpInternational)

impsocial_gender <- subgroup_summary(alumni.data,~Gender,~ImpSocial)
impcivic_gender <- subgroup_summary(alumni.data,~Gender,~ImpCivic)
impecon_gender <- subgroup_summary(alumni.data,~Gender,~ImpEcon)
imppolicy_gender <- subgroup_summary(alumni.data,~Gender,~ImpPolicy)

#Scheme
impinstitutional_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpInstitutional)
implocal_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpLocal)
impnational_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpNational)
impinternational_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpInternational)

impsocial_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpSocial)
impcivic_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpCivic)
impecon_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpEcon)
imppolicy_sch <- subgroup_summary(alumni.data,~SchemeNom,~ImpPolicy)

#Scheme Type
impinstitutional_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpInstitutional)
implocal_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpLocal)
impnational_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpNational)
impinternational_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpInternational)

impsocial_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpSocial)
impcivic_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpCivic)
impecon_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpEcon)
imppolicy_schtype <- subgroup_summary(alumni.data,~SchemeType,~ImpPolicy)

#Year Group
impinstitutional_year <- subgroup_summary(alumni.data,~YearGroup,~ImpInstitutional)
implocal_year <- subgroup_summary(alumni.data,~YearGroup,~ImpLocal)
impnational_year <- subgroup_summary(alumni.data,~YearGroup,~ImpNational)
impinternational_year <- subgroup_summary(alumni.data,~YearGroup,~ImpInternational)

impsocial_year <- subgroup_summary(alumni.data,~YearGroup,~ImpSocial)
impcivic_year <- subgroup_summary(alumni.data,~YearGroup,~ImpCivic)
impecon_year <- subgroup_summary(alumni.data,~YearGroup,~ImpEcon)
imppolicy_year <- subgroup_summary(alumni.data,~YearGroup,~ImpPolicy)

#Origin Region
impinstitutional_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpInstitutional)
implocal_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpLocal)
impnational_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpNational)
impinternational_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpInternational)

impsocial_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpSocial)
impcivic_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpCivic)
impecon_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpEcon)
imppolicy_orireg <- subgroup_summary(alumni.data,~OriginRegion,~ImpPolicy)

#Residency Region
impinstitutional_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpInstitutional)
implocal_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpLocal)
impnational_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpNational)
impinternational_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpInternational)

impsocial_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpSocial)
impcivic_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpCivic)
impecon_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpEcon)
imppolicy_resreg <- subgroup_summary(alumni.data,~ResidencyRegion,~ImpPolicy)

#Subect Studied
impinstitutional_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpInstitutional) %>% filter(!JacsCat=="NA", sum(freq)>20)
implocal_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpLocal) %>% filter(!JacsCat=="NA", sum(freq)>20)
impnational_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpNational) %>% filter(!JacsCat=="NA", sum(freq)>20)
impinternational_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpInternational) %>% filter(!JacsCat=="NA", sum(freq)>20)

impsocial_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpSocial) %>% filter(!JacsCat=="NA", sum(freq)>20)
impcivic_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpCivic) %>% filter(!JacsCat=="NA", sum(freq)>20)
impecon_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpEcon) %>% filter(!JacsCat=="NA", sum(freq)>20)
imppolicy_jacs <- subgroup_summary(alumni.data,~JacsCat,~ImpPolicy) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
impinstitutional_score <- alumni.data %>% group_by(ImpInstitutional) %>% score_summary()
implocal_score <- alumni.data %>% group_by(ImpLocal) %>% score_summary()
impnational_score <- alumni.data %>% group_by(ImpNational) %>% score_summary()
impinternational_score <- alumni.data %>% group_by(ImpInternational) %>% score_summary()

impsocial_score <- alumni.data %>% group_by(ImpSocial) %>% score_summary()
impcivic_score <- alumni.data %>% group_by(ImpCivic) %>% score_summary()
impecon_score <- alumni.data %>% group_by(ImpEcon) %>% score_summary()
imppolicy_score <- alumni.data %>% group_by(ImpPolicy) %>% score_summary()


## Analytic indices----

#prefix = "i."

#Overall

indices <- select(alumni.data, starts_with("i."))

index_overall <- rbind(sapply(indices, summary),SD=sapply(indices, sd)) %>% round(2)

#Gender
indexldr_gender <- alumni.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) ) 


indexcollab_gender <- 
  alumni.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexskills_gender <- 
  alumni.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) )

indexresearch_gender <- 
  alumni.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Scheme
indexldr_sch <- alumni.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_sch <- 
  alumni.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexskills_sch <- 
  alumni.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) )

indexresearch_sch <- 
  alumni.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Scheme Type
indexldr_schtype <- 
  alumni.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_schtype <- 
  alumni.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexskills_schtype <- 
  alumni.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) )

indexresearch_schtype <- 
  alumni.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Year Group
indexldr_year <- 
  alumni.data %>% 
  group_by(YearGroup) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_year <- 
  alumni.data %>% 
  group_by(YearGroup) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexskills_year <- 
  alumni.data %>% 
  group_by(YearGroup) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) )

indexresearch_year <- 
  alumni.data %>% 
  group_by(YearGroup) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Origin Region
indexldr_orireg <- alumni.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_orireg <- 
  alumni.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexskills_orireg <- 
  alumni.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) )

indexresearch_orireg <- 
  alumni.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Residency Region
indexldr_resreg <- 
  alumni.data %>% 
  group_by(ResidencyRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_resreg <- 
  alumni.data %>% 
  group_by(ResidencyRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexskills_resreg <- 
  alumni.data %>% 
  group_by(ResidencyRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) )

indexresearch_resreg <- 
  alumni.data %>% 
  group_by(ResidencyRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Subect Studied
indexldr_jacs <- 
  alumni.data %>% 
  group_by(JacsCat) %>%
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) ) %>% 
  filter(!JacsCat=="NA", Count>20)

indexcollab_jacs <- 
  alumni.data %>% 
  group_by(JacsCat) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) ) %>% 
  filter(!JacsCat=="NA", Count>20)

indexskills_jacs <- 
  alumni.data %>% 
  group_by(JacsCat) %>% 
  summarise(Count = n(),
            Median = round(median(i.skills),2),
            Mean = round(mean(i.skills),2),
            SD = round(sd(i.skills),2),
            Max = round(max(i.skills),2),
            Min = round(min(i.skills),2) ) %>% 
  filter(!JacsCat=="NA", Count>20)

indexresearch_jacs <- 
  alumni.data %>% 
  group_by(JacsCat) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) ) %>% 
  filter(!JacsCat=="NA", Count>20)

#Committee Score

#this is much harder to model in a table because both sets of variables are continuous - better to use a scatterplot

indexldr_score <- 
  ggplot(alumni.data, aes(x=ZCtteeScore, y=i.ldr)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()

indexcollab_score <- 
  ggplot(alumni.data, aes(x=ZCtteeScore, y=i.collab)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()


indexskills_score <- 
  ggplot(alumni.data, aes(x=ZCtteeScore, y=i.skills)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()

indexresearch_score <- 
  ggplot(alumni.data, aes(x=ZCtteeScore, y=i.research)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()

