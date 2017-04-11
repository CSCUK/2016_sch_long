# Code information ----

# Longitudinal Scholar tracking survey: data analysis
# Matt Mawer, The Association of Commonwealth Universities
# February, 2017

load("2016_sch_long_core.rdata")
SurveyName <- "2016 Alumni Survey"

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats, plotly, pander)

opar = par()


# --- Functions ----

score_summary <- function(dataframe){
  summarise(
    filter(dataframe,!SchemeNom=="Shared Scholars", !SchemeNom=="Distance Learners"),
    Count = n(),
    Median = round(median(ZCtteeScore, na.rm=T),2),
    Mean = round(mean(ZCtteeScore, na.rm=T),2), 
    SD = round(sd(ZCtteeScore, na.rm=T),2), 
    Max = round(max(ZCtteeScore, na.rm=T),2),
    Min = round(min(ZCtteeScore, na.rm=T),2)
  )}


pop_summary <- function(dataframe, variable){
  dataframe %>% 
    group_by_(variable) %>% 
    summarise_(
      freq = ~n() 
    ) %>% 
    mutate(prop = round((freq / sum(freq))*100,1))
} #population level summary for a variable

subgroup_summary <- function(dataframe, group, variable){
  dataframe %>% 
    group_by_(group, variable) %>% 
    summarise_(
      freq = ~n() 
    ) %>% 
    mutate(prop = round((freq / sum(freq))*100,1))
} #subgroup summary for a variable: takes group as the second argument

# --- Dataset structure ----

str(base.data, list.len=nrow(base.data))
str(alumni.data, list.len=nrow(alumni.data))

# VARIABLE SUFFIXES:
# Gender = gender
# Scheme = sch
# Scheme type = schtype
# YearGroup = year
# Origin region = orireg
# Residency region = resreg
# JACS category = jacs

## a] Data overview ----
overview_gender <- pop_summary(alumni.data,~Gender) %>% arrange(desc(prop))
overview_sch <- pop_summary(alumni.data,~Scheme) %>% arrange(desc(prop))
overview_schtype <- pop_summary(alumni.data,~SchemeType) %>% arrange(desc(prop))
overview_year <- pop_summary(alumni.data,~YearGroup) %>% arrange(desc(prop))
overview_orireg <- pop_summary(alumni.data,~OriginRegion) %>% arrange(desc(prop))
overview_resreg <- pop_summary(alumni.data,~ResidencyRegion) %>% arrange(desc(prop))
overview_jacs <- pop_summary(alumni.data,~JacsCat) %>% arrange(desc(prop))
overview_score <- alumni.data %>% group_by(CtteeGroup) %>% score_summary()


## b] Residency ----

# prefix = 'res'

#overall
rescountry_overall <- alumni.data %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
resregion_overall <- alumni.data %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
resreason_overall <- alumni.data %>% filter(!ResidencyReason=="NA") %>% count(ResidencyReason) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))

# Residency status (home country)
rescountry_gender <- alumni.data %>% group_by(Gender) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home") %>% arrange(desc(prop))
rescountry_sch <- alumni.data %>% group_by(SchemeNom) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home") %>% arrange(desc(prop))
rescountry_schtype <- alumni.data %>% group_by(SchemeType) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home") %>% arrange(desc(prop))
rescountry_year <- alumni.data %>% group_by(YearGroup) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home") %>% arrange(desc(prop))
rescountry_orireg <- alumni.data %>% group_by(OriginRegion) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home") %>% arrange(desc(prop))
rescountry_resreg <- alumni.data %>% group_by(ResidencyRegion) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home") %>% arrange(desc(prop))
rescountry_jacs <- alumni.data %>% filter(!JacsCat=="NA") %>% group_by(JacsCat) %>% count(resStatusCountry) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusCountry=="Home",sum(n)>20) %>% arrange(desc(prop))
rescountry_score <- alumni.data %>% group_by(resStatusCountry) %>% score_summary

p.rescountry_score <- ggplot(alumni.data, aes(x=resStatusCountry, y=ZCtteeScore)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=4)


# Residency status (home region)
resregion_gender <- alumni.data %>% group_by(Gender) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home") %>% arrange(desc(prop))
resregion_sch <- alumni.data %>% group_by(SchemeNom) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home") %>% arrange(desc(prop))
resregion_schtype <- alumni.data %>% group_by(SchemeType) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home") %>% arrange(desc(prop))
resregion_year <- alumni.data %>% group_by(YearGroup) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home") %>% arrange(desc(prop))
resregion_orireg <- alumni.data %>% group_by(OriginRegion) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home") %>% arrange(desc(prop))
resregion_resreg <- alumni.data %>% group_by(ResidencyRegion) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home") %>% arrange(desc(prop))
resregion_jacs <- alumni.data %>% filter(!JacsCat=="NA") %>% group_by(JacsCat) %>% count(resStatusRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(resStatusRegion=="Home",sum(n)>20) %>% arrange(desc(prop))
resregion_score <- alumni.data %>% group_by(resStatusRegion) %>% score_summary()

p.resregion_score <- ggplot(alumni.data, aes(x=resStatusRegion, y=ZCtteeScore)) + geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=23, size=4)


## c] Employment ----

# prefix = 'emp'
# TO ADD - CONTEXT VARIABLES

# Overall
empcurrent_overall <- alumni.data %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
empsector_overall <- alumni.data %>% filter(!CurrentSector=="NA") %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
empskillmatch_overall <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
empchange_overall <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
empreturnorg_overall <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))
empreturntime_overall <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(prop))

# Current employment - limited to 'Employed' and 'Studying' for ease of viewing
empcurrent_gender <- alumni.data %>% group_by(Gender) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying") %>% arrange(CurrentEmploy, desc(prop))
empcurrent_sch <- alumni.data %>% group_by(SchemeNom) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying") %>% arrange(CurrentEmploy, desc(prop))
empcurrent_schtype <- alumni.data %>% group_by(SchemeType) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying") %>% arrange(CurrentEmploy, desc(prop))
empcurrent_year <- alumni.data %>% group_by(YearGroup) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying") %>% arrange(CurrentEmploy, desc(prop))
empcurrent_orireg <- alumni.data %>% group_by(OriginRegion) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying") %>% arrange(CurrentEmploy, desc(prop))
empcurrent_resreg <- alumni.data %>% group_by(ResidencyRegion) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying", sum(n)>20) %>% arrange(CurrentEmploy, desc(prop)) #limited to regions with n>20
empcurrent_jacs <- alumni.data %>% filter(!JacsCat=="NA") %>% group_by(JacsCat) %>% count(CurrentEmploy) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(CurrentEmploy %in% "Employed" | CurrentEmploy %in% "Studying", sum(n)>20) %>% arrange(CurrentEmploy,desc(prop))
empcurrent_score <- alumni.data %>% group_by(CurrentEmploy) %>% score_summary

# Current employment sector
empsector_gender <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(Gender) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSector, desc(prop))
empsector_sch <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(SchemeNom) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSector, desc(prop))
empsector_schtype <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(SchemeType) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSector, desc(prop))
empsector_year <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(YearGroup) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSector, YearGroup, desc(prop))
empsector_orireg <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(OriginRegion) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSector, desc(prop))
empsector_resreg <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(ResidencyRegion) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(sum(n)>20) %>% arrange(CurrentSector, desc(prop))
empsector_jacs <- alumni.data %>% filter(!JacsCat=="NA",!CurrentSector=="NA") %>% group_by(JacsCat) %>% count(CurrentSector) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(sum(n)>20) %>% arrange(CurrentSector, desc(prop))
empsector_score <- alumni.data %>% filter(!CurrentSector=="NA") %>% group_by(CurrentSector) %>% score_summary()

# Current employment skill level match
empskillmatch_gender <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(Gender) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSkillMatch, desc(prop))
empskillmatch_sch <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(SchemeNom) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSkillMatch, desc(prop))
empskillmatch_schtype <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(SchemeType) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSkillMatch, desc(prop))
empskillmatch_year <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(YearGroup) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSkillMatch, YearGroup, desc(prop))
empskillmatch_orireg <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(OriginRegion) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(CurrentSkillMatch, desc(prop))
empskillmatch_resreg <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(ResidencyRegion) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(sum(n)>20) %>% arrange(CurrentSkillMatch, desc(prop))
empskillmatch_jacs <- alumni.data %>% filter(!JacsCat=="NA",!CurrentSkillMatch=="NA") %>% group_by(JacsCat) %>% count(CurrentSkillMatch) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(sum(n)>20) %>% arrange(CurrentSkillMatch, desc(prop))
empskillmatch_score <- alumni.data %>% filter(!CurrentSkillMatch=="NA") %>% group_by(CurrentSkillMatch) %>% score_summary()

# Job changes in the last 2 years (and then tables limited to 'never', for easier comparison)
empchange_gender <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(Gender) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(Gender,CurrentJobChange)
empchange_sch <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(SchemeNom) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(SchemeNom,CurrentJobChange)
empchange_schtype <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(SchemeType) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(SchemeType,CurrentJobChange)
empchange_year <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(YearGroup) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(YearGroup,CurrentJobChange)
empchange_orireg <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(OriginRegion) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(OriginRegion,CurrentJobChange)
empchange_resreg <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(ResidencyRegion) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(ResidencyRegion,CurrentJobChange)
empchange_jacs <- alumni.data %>% filter(!JacsCat=="NA",!CurrentJobChange=="NA") %>% group_by(JacsCat) %>% count(CurrentJobChange) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(sum(n)>20) %>% arrange(JacsCat,CurrentJobChange)
empchange_score <- alumni.data %>% filter(!CurrentJobChange=="NA") %>% group_by(CurrentJobChange) %>% score_summary()

empchange_jacs %>% filter(CurrentJobChange=="Never") #example of simplified table looking at 'never' only

# Return to previous organisation for +2 year respondents only (note 'YearGroup' is missing as a variable because it is meaningless: all are +2 group)
empreturnorg_gender <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(Gender) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(Gender,desc(prop))
empreturnorg_sch <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(SchemeNom) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(SchemeNom, desc(ReturnOrganisation))
empreturnorg_schtype <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(SchemeType) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(SchemeType, desc(ReturnOrganisation))
empreturnorg_orireg <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(OriginRegion) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(OriginRegion, desc(ReturnOrganisation))
empreturnorg_resreg <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(ResidencyRegion) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(ResidencyRegion, desc(ReturnOrganisation))
empreturnorg_jacs <- alumni.data %>% filter(grepl("_Two$", SurveyID),!JacsCat=="NA") %>% group_by(JacsCat) %>% count(ReturnOrganisation) %>% mutate(prop = round((n / sum(n))*100,1)) %>% filter(sum(n)>20) %>% arrange(JacsCat, desc(ReturnOrganisation))
empreturnorg_score <- alumni.data %>% filter(grepl("_Two$", SurveyID)) %>% group_by(ReturnOrganisation) %>% score_summary()

empreturnorg_sch %>% filter(ReturnOrganisation=="Yes") #example of simplified table for looking at those who return to org only

# Time to gain employment for those +2 year respondents who did not return to their former organistion (N quite small for some groups, not very meaningful)
empreturntime_gender <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(Gender) %>%  count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1))
empreturntime_sch <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(SchemeNom) %>%  count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1))
empreturntime_schtype <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(SchemeType) %>%  count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1))
empreturntime_orireg <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(OriginRegion) %>%  count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1))
empreturntime_resreg <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(ResidencyRegion) %>%  count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1))
empreturntime_jacs <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(JacsCat) %>%  count(ReturnEmploy) %>% mutate(prop = round((n / sum(n))*100,1))
empreturntime_score <- alumni.data %>% filter(grepl("_Two$", SurveyID), !ReturnOrganisation=="Yes", CurrentEmploy=="Employed") %>% group_by(ReturnEmploy) %>% score_summary()


## d] Further qualifications----

# prefix = "Aca"

#Overall
Acapostaward_overall <- pop_summary(alumni.data, ~AcaPostaward)
Acaquallevel_overall <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% pop_summary(~AcaQualLevel) %>% arrange(desc(prop))
Acacontribution_overall <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% pop_summary(~AcaQualCMWContribution)

#Gender
Acapostaward_gender <- subgroup_summary(alumni.data, ~Gender,~AcaPostaward)
Acaquallevel_gender <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~Gender,~AcaQualLevel)
Acacontribution_gender <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~Gender, ~AcaQualCMWContribution)

#Scheme
Acapostaward_sch <- subgroup_summary(alumni.data, ~SchemeNom,~AcaPostaward)
Acaquallevel_sch <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeNom,~AcaQualLevel)
Acacontribution_sch <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeNom, ~AcaQualCMWContribution)

#Scheme Type
Acapostaward_schtype <- subgroup_summary(alumni.data, ~SchemeType,~AcaPostaward)
Acaquallevel_schtype <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeType,~AcaQualLevel)
Acacontribution_schtype <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~SchemeType, ~AcaQualCMWContribution)

#Year Group
Acapostaward_year <- subgroup_summary(alumni.data, ~YearGroup,~AcaPostaward)
Acaquallevel_year <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~YearGroup,~AcaQualLevel)
Acacontribution_year <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~YearGroup, ~AcaQualCMWContribution)

#Origin Region
Acapostaward_orireg <- subgroup_summary(alumni.data, ~OriginRegion,~AcaPostaward)
Acaquallevel_orireg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~OriginRegion,~AcaQualLevel)
Acacontribution_orireg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~OriginRegion, ~AcaQualCMWContribution)

#Residency Region
Acapostaward_resreg <- subgroup_summary(alumni.data, ~ResidencyRegion,~AcaPostaward)
Acaquallevel_resreg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~ResidencyRegion,~AcaQualLevel)
Acacontribution_resreg <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% subgroup_summary(~ResidencyRegion, ~AcaQualCMWContribution)

#Subect Studied
Acapostaward_jacs <- subgroup_summary(alumni.data, ~JacsCat,~AcaPostaward) %>% filter(!JacsCat=="NA", sum(freq)>20)
Acaquallevel_jacs <- alumni.data %>% filter(!is.na(AcaQualLevel),!JacsCat=="NA") %>% subgroup_summary(~JacsCat,~AcaQualLevel)
Acacontribution_jacs <- alumni.data %>% filter(!is.na(AcaQualLevel),!JacsCat=="NA") %>% subgroup_summary(~JacsCat, ~AcaQualCMWContribution)

#Committee Score
Acapostaward_score <- alumni.data %>% group_by(AcaPostaward) %>% score_summary()
Acaquallevel_score <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% group_by(AcaQualLevel) %>% score_summary()
Acacontribution_score <- alumni.data %>% filter(!is.na(AcaQualLevel)) %>% group_by(AcaQualCMWContribution) %>% score_summary()

## e] Leadership ----

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

ldrbudget_orireg %>% filter(LdrBudget=="Yes") %>% arrange(desc(prop)) #example of simplified table looking at proportions reporting activities

## f] Skill application----

#Overall
#Gender
#Scheme
#Scheme Type
#Year Group
#Origin Region
#Residency Region
#Subect Studied
#Committee Score

## g] Teaching and research

#Overall
#Gender
#Scheme
#Scheme Type
#Year Group
#Origin Region
#Residency Region
#Subect Studied
#Committee Score

----
## h] Networks and links----

#Overall
#Gender
#Scheme
#Scheme Type
#Year Group
#Origin Region
#Residency Region
#Subect Studied
#Committee Score

## i] Broader impact----

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


#WORK IN PROGRESS TO SIMPLIFY BIND_COLS CODES FOR TABLES 
test <- function(dataframe,variable,group,v_name){
  dataframe %>%
    group_by_(group, variable) %>% 
    summarise_(
      freq = ~n() 
    ) %>% 
    mutate_(prop = round((freq / sum(freq))*100,1)) %>% 
    filter_(variable=="Yes") %>% 
    arrange_(group) %>% 
    select_(group, v_name=prop)
}



#Example of a concise table for impact level
bind_cols(
  select(impinstitutional_year %>% filter(ImpInstitutional=="Yes") %>% arrange(YearGroup) %>% ungroup, YearGroup, Institutional=prop),
  select(implocal_year %>% filter(ImpLocal=="Yes") %>% arrange(YearGroup) %>% ungroup, Local=prop),
  select(impnational_year %>% filter(ImpNational=="Yes") %>% arrange(YearGroup) %>% ungroup, National=prop),
  select(impinternational_year %>% filter(ImpInternational=="Yes") %>% arrange(YearGroup) %>% ungroup, International=prop)
  )

#Example of a concise table for impact type
bind_cols(
  select(impsocial_year %>% filter(ImpSocial=="Yes") %>% arrange(YearGroup) %>% ungroup, YearGroup, Social=prop),
  select(impcivic_year %>% filter(ImpCivic=="Yes") %>% arrange(YearGroup) %>% ungroup, Civic=prop),
  select(impecon_year %>% filter(ImpEcon=="Yes") %>% arrange(YearGroup) %>% ungroup, Economic=prop),
  select(imppolicy_year %>% filter(ImpPolicy=="Yes") %>% arrange(YearGroup) %>% ungroup, Policy=prop)
)


## j] Analytic indices----

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

qplot(x=i.ldr,y=ZCtteeScore,data=alumni.data, geom="point")
