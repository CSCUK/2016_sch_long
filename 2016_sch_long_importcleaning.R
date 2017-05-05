# Code information ----

# Longitudinal Scholar tracking survey: data analysis
# Matt Mawer, The Association of Commonwealth Universities
# February, 2017

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats, plotly, pander)

opar = par()

# --- Data connection and import ----

# Data from sources:
# 1. Libra
# 2. Evaluation database

## a] DB connections ----

con.evaldb <- odbcConnect("EvalDb")
con.libra <- odbcConnect("Libra") 

con.evaldb
con.libra

# DB query for population data

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
population <- select(population,-DegreeCode, -SchemeType, -PhD)
  

# DB query for survey data

## Query data and join onto admin and geodata
## Two step process required for alumni data because +2 survey has diferent variables, so needs join and bind rows, removes unneeded columns
alumni.data <- 
  bind_rows(list(
      sqlQuery(con.evaldb, "SELECT tbl_DATA_Sch_4.*,tbl_Ctrl_EvalInfo.SchemeType,tbl_Ctrl_EvalInfo.PhD FROM tbl_DATA_Sch_4 LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_DATA_Sch_4.AWDID WHERE tbl_DATA_Sch_4.YearGroup=2012"),
      sqlQuery(con.evaldb, "SELECT tbl_DATA_Sch_6.*,tbl_Ctrl_EvalInfo.SchemeType,tbl_Ctrl_EvalInfo.PhD FROM tbl_DATA_Sch_6 LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_DATA_Sch_6.AWDID WHERE tbl_DATA_Sch_6.YearGroup=2010"),
      sqlQuery(con.evaldb, "SELECT tbl_DATA_Sch_8.*,tbl_Ctrl_EvalInfo.SchemeType,tbl_Ctrl_EvalInfo.PhD FROM tbl_DATA_Sch_8 LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_DATA_Sch_8.AWDID WHERE tbl_DATA_Sch_8.YearGroup=2008"),
      sqlQuery(con.evaldb, "SELECT tbl_DATA_Sch_10.*,tbl_Ctrl_EvalInfo.SchemeType,tbl_Ctrl_EvalInfo.PhD FROM tbl_DATA_Sch_10 LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_DATA_Sch_10.AWDID WHERE tbl_DATA_Sch_10.YearGroup=2006"))) %>%
  full_join(sqlQuery(con.evaldb, "SELECT tbl_DATA_Sch_2.*,tbl_Ctrl_EvalInfo.SchemeType,tbl_Ctrl_EvalInfo.PhD FROM tbl_DATA_Sch_2 LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_DATA_Sch_2.AWDID WHERE tbl_DATA_Sch_2.YearGroup=2014"))
  
alumni.data <- 
  alumni.data %>% 
  left_join(population, by="AWDID") %>% 
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Origin"="Country")) %>%
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Residency"="Country")) %>% 
  select(-CurrentSector, -DateAdded) %>% 
  rename(OriginRegion=Region.x,ResidencyRegion = Region.y, CurrentSector=DCurrentSector) %>% 
  tbl_df()

## DB query for baseline data, joins onto admin and geodata
base.data <- 
  sqlQuery(con.evaldb, "SELECT tbl_DATA_Sch_0.* FROM tbl_DATA_Sch_0 WHERE tbl_DATA_Sch_0.SurveyID='Sch_2016_0'") %>% 
  left_join(population, by="AWDID") %>% 
  left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Origin"="Country")) %>% 
  select(-DateAdded,-StudyScholFunder,-PreSector) %>% 
  rename(OriginRegion = Region, PreSector= DPreSector, StudyScholFunder = DStudyScholFunder) %>% 
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
                                "SS"="Shared Scholar"),
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

## e] Cleanup ----

rm(list= ls()[!(ls() %in% c("opar","alumni.data","base.data"))]) #remove everything that doesn't match this list
gc() #clean the memory
