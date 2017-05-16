# Code information ----

# Baseline  survey: data analysis
# Matt Mawer, The Association of Commonwealth Universities
# May, 2017

# Script creates an extensive array of basic data tables for the results of the CSC's surveys.
# Many tables can be used directly in reports (e.g. in markdown, or exported to excel) or can be used as part of other analyses and visualisations
# Data structures throughout are data frames (tibbles) in long data format

# Either load the RDATA file directly, or use source() to the importcleaning.r script:
source("S:/SCHOLARSHIPS/CSC/SCHEMES/CSFP-IN/CSC-Evaluation/Data Management Crystal Snap IT/r_codebank/2016_sch_long/2016_sch_long_importcleaning.r")
# load("2016_sch_long_core.rdata")

SurveyName <- "2016 Baseline Survey"

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats, plotly, pander)

opar = par()

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

str(base.data, list.len=nrow(base.data))

# VARIABLE SUFFIXES:
# Gender = gender
# Scheme = sch
# Scheme type = schtype
# Origin region = orireg
# JACS category = jacs. Note: throughout, JACs categories included in dataframes are limited to those with 20+ cases. 
#                       JACs = NA is usually excluded.

## a] Data overview ----
overview_gender <- pop_summary(base.data,~Gender) %>% arrange(desc(prop))
overview_sch <- pop_summary(base.data,~SchemeNom) %>% arrange(desc(prop))
overview_schtype <- pop_summary(base.data,~SchemeType) %>% arrange(desc(prop))
overview_orireg <- pop_summary(base.data,~OriginRegion) %>% arrange(desc(prop))
overview_jacs <- pop_summary(base.data,~JacsCat) %>% arrange(desc(prop))
overview_score <- base.data %>% group_by(CtteeGroup) %>% score_summary()

## b] Previous scholarships

studyscholmain_overall <- pop_summary(base.data,~StudyScholMain)
studyscholtype_overall <- pop_summary(base.data,~StudyScholType)

studyscholmain_gender <- subgroup_summary(base.data,~Gender,~StudyScholMain)
studyscholmain_sch <- subgroup_summary(base.data,~SchemeNom,~StudyScholMain)
studyscholmain_schtype <- subgroup_summary(base.data,~SchemeType,~StudyScholMain)
studyscholmain_orireg <- subgroup_summary(base.data,~OriginRegion,~StudyScholMain)
studyscholmain_jacs <- subgroup_summary(base.data,~JacsCat,~StudyScholMain)
studyscholmain_score <- base.data %>% group_by(StudyScholMain) %>% score_summary()

studyscholtype_gender <- base.data %>% filter(!StudyScholMain=="No") %>% subgroup_summary(~Gender,~StudyScholType)
studyscholtype_sch <- base.data %>% filter(!StudyScholMain=="No") %>% subgroup_summary(~SchemeNom,~StudyScholType)
studyscholtype_schtype <- base.data %>% filter(!StudyScholMain=="No") %>% subgroup_summary(~SchemeType,~StudyScholType)
studyscholtype_orireg <- base.data %>% filter(!StudyScholMain=="No") %>% subgroup_summary(~OriginRegion,~StudyScholType)
studyscholtype_jacs <- base.data %>% filter(!StudyScholMain=="No") %>% subgroup_summary(~JacsCat,~StudyScholType)
studyscholtype_score <- base.data %>% filter(!StudyScholMain=="No") %>% group_by(StudyScholType) %>% score_summary()

## c] Counterfactual responses

# Overall
cfuk_overall <- base.data %>% summarise(Cases = length(CFUK),Median=median(CFUK),Mean= mean(CFUK), SD=sd(CFUK),Max=max(CFUK),Min=min(CFUK))
cfhome_overall <- base.data %>% summarise(Cases = length(CFHome),Median=median(CFHome),Mean= mean(CFHome), SD=sd(CFHome),Max=max(CFHome),Min=min(CFHome))
cfother_overall <- base.data %>% summarise(Cases = length(CFOther),Median=median(CFOther),Mean= mean(CFOther), SD=sd(CFOther),Max=max(CFOther),Min=min(CFOther))

cfukfunding_overall <- base.data %>% filter(!CFUKFunding=="NA") %>% pop_summary(~CFUKFunding) %>% arrange(desc(prop))
cfhomefunding_overall <- base.data %>% filter(!CFHomeFunding=="NA") %>% pop_summary(~CFHomeFunding) %>% arrange(desc(prop))
cfotherfunding_overall <- base.data %>% filter(!CFOtherFunding=="NA") %>% pop_summary(~CFOtherFunding) %>% arrange(desc(prop))

# Gender
cfuk_gender <- base.data %>% group_by(Gender) %>% summarise(Cases = length(CFUK),Median=median(CFUK),Mean= mean(CFUK), SD=sd(CFUK),Max=max(CFUK),Min=min(CFUK))
cfhome_gender <- base.data %>% group_by(Gender) %>% summarise(Cases = length(CFHome),Median=median(CFHome),Mean= mean(CFHome), SD=sd(CFHome),Max=max(CFHome),Min=min(CFHome))
cfother_gender <- base.data %>% group_by(Gender) %>% summarise(Cases = length(CFOther),Median=median(CFOther),Mean= mean(CFOther), SD=sd(CFOther),Max=max(CFOther),Min=min(CFOther))

cfukfunding_gender <- base.data %>% filter(!CFUKFunding=="NA") %>% subgroup_summary(~Gender,~CFUKFunding)
cfhomefunding_gender <- base.data %>% filter(!CFHomeFunding=="NA") %>% subgroup_summary(~Gender,~CFHomeFunding)
cfotherfunding_gender <- base.data %>% filter(!CFOtherFunding=="NA") %>% subgroup_summary(~Gender,~CFOtherFunding)

# Scheme
cfuk_sch <- base.data %>% group_by(SchemeNom) %>% summarise(Cases = length(CFUK),Median=median(CFUK),Mean= mean(CFUK), SD=sd(CFUK),Max=max(CFUK),Min=min(CFUK))
cfhome_sch <- base.data %>% group_by(SchemeNom) %>% summarise(Cases = length(CFHome),Median=median(CFHome),Mean= mean(CFHome), SD=sd(CFHome),Max=max(CFHome),Min=min(CFHome))
cfother_sch <- base.data %>% group_by(SchemeNom) %>% summarise(Cases = length(CFOther),Median=median(CFOther),Mean= mean(CFOther), SD=sd(CFOther),Max=max(CFOther),Min=min(CFOther))

cfukfunding_sch <- base.data %>% filter(!CFUKFunding=="NA") %>% subgroup_summary(~SchemeNom,~CFUKFunding)
cfhomefunding_sch <- base.data %>% filter(!CFHomeFunding=="NA") %>% subgroup_summary(~SchemeNom,~CFHomeFunding)
cfotherfunding_sch <- base.data %>% filter(!CFOtherFunding=="NA") %>% subgroup_summary(~SchemeNom,~CFOtherFunding)

# Scheme Type
cfuk_schtype <- base.data %>% group_by(SchemeType) %>% summarise(Cases = length(CFUK),Median=median(CFUK),Mean= mean(CFUK), SD=sd(CFUK),Max=max(CFUK),Min=min(CFUK))
cfhome_schtype <- base.data %>% group_by(SchemeType) %>% summarise(Cases = length(CFHome),Median=median(CFHome),Mean= mean(CFHome), SD=sd(CFHome),Max=max(CFHome),Min=min(CFHome))
cfother_schtype <- base.data %>% group_by(SchemeType) %>% summarise(Cases = length(CFOther),Median=median(CFOther),Mean= mean(CFOther), SD=sd(CFOther),Max=max(CFOther),Min=min(CFOther))

cfukfunding_schtype <- base.data %>% filter(!CFUKFunding=="NA") %>% subgroup_summary(~SchemeType,~CFUKFunding)
cfhomefunding_schtype <- base.data %>% filter(!CFHomeFunding=="NA") %>% subgroup_summary(~SchemeType,~CFHomeFunding)
cfotherfunding_schtype <- base.data %>% filter(!CFOtherFunding=="NA") %>% subgroup_summary(~SchemeType,~CFOtherFunding)

# Origin Region
cfuk_orireg <- base.data %>% group_by(OriginRegion) %>% summarise(Cases = length(CFUK),Median=median(CFUK),Mean= mean(CFUK), SD=sd(CFUK),Max=max(CFUK),Min=min(CFUK))
cfhome_orireg <- base.data %>% group_by(OriginRegion) %>% summarise(Cases = length(CFHome),Median=median(CFHome),Mean= mean(CFHome), SD=sd(CFHome),Max=max(CFHome),Min=min(CFHome))
cfother_orireg <- base.data %>% group_by(OriginRegion) %>% summarise(Cases = length(CFOther),Median=median(CFOther),Mean= mean(CFOther), SD=sd(CFOther),Max=max(CFOther),Min=min(CFOther))

cfukfunding_orireg <- base.data %>% filter(!CFUKFunding=="NA") %>% subgroup_summary(~OriginRegion,~CFUKFunding)
cfhomefunding_orireg <- base.data %>% filter(!CFHomeFunding=="NA") %>% subgroup_summary(~OriginRegion,~CFHomeFunding)
cfotherfunding_orireg <- base.data %>% filter(!CFOtherFunding=="NA") %>% subgroup_summary(~OriginRegion,~CFOtherFunding)

# Subject studied
cfuk_sch <- base.data %>% group_by(JacsCat) %>% summarise(Cases = length(CFUK),Median=median(CFUK),Mean= mean(CFUK), SD=sd(CFUK),Max=max(CFUK),Min=min(CFUK)) %>% filter(!JacsCat=="NA", Cases>20)
cfhome_sch <- base.data %>% group_by(JacsCat) %>% summarise(Cases = length(CFHome),Median=median(CFHome),Mean= mean(CFHome), SD=sd(CFHome),Max=max(CFHome),Min=min(CFHome)) %>% filter(!JacsCat=="NA", Cases>20)
cfother_sch <- base.data %>% group_by(JacsCat) %>% summarise(Cases = length(CFOther),Median=median(CFOther),Mean= mean(CFOther), SD=sd(CFOther),Max=max(CFOther),Min=min(CFOther)) %>% filter(!JacsCat=="NA", Cases>20)

cfukfunding_sch <- base.data %>% filter(!CFUKFunding=="NA",!JacsCat=="NA") %>% subgroup_summary(~JacsCat,~CFUKFunding)
cfhomefunding_sch <- base.data %>% filter(!CFHomeFunding=="NA",!JacsCat=="NA") %>% subgroup_summary(~JacsCat,~CFHomeFunding)
cfotherfunding_sch <- base.data %>% filter(!CFOtherFunding=="NA",!JacsCat=="NA") %>% subgroup_summary(~JacsCat,~CFOtherFunding)

# Score
# -- Scattergraphs for score - to be added


## d] Employment ----

# prefix = 'emp'
# TO ADD - CONTEXT VARIABLES

# Overall
emppreschol_overall <- pop_summary(base.data,~PreEmploy)
emppresector_overall <- pop_summary(base.data,~PreSector)
emppresupport_overall <- base.data %>% filter(!PreSupportiveness=="NA") %>% pop_summary(~PreSupportiveness)
empskillmatch_overall <- base.data %>% filter(!CurrentSkillMatch=="NA") %>% pop_summary(~CurrentSkillMatch)
empchange_overall <- base.data %>% filter(!CurrentJobChange=="NA") %>% pop_summary(~CurrentJobChange)
empcurrent_overall <- pop_summary(base.data,~CurrentStatus)
empbond_overall <- pop_summary(base.data, ~Bond)

# Pre-scholarship employment status
emppreschol_gender <- subgroup_summary(base.data,~Gender,~PreEmploy)
emppreschol_sch <- subgroup_summary(base.data,~SchemeNom,~PreEmploy)
emppreschol_schtype <- subgroup_summary(base.data,~SchemeType,~PreEmploy)
emppreschol_orireg <- subgroup_summary(base.data,~OriginRegion,~PreEmploy)
emppreschol_jacs <- subgroup_summary(base.data,~JacsCat,~PreEmploy) %>% filter(!JacsCat=="NA", sum(freq)>20)
emppreschol_score <- base.data %>% group_by(PreEmploy) %>% score_summary

# Pre-scholarship employment sector
empsector_gender <- subgroup_summary(base.data,~Gender,~PreSector)
empsector_sch <- subgroup_summary(base.data,~SchemeNom,~PreSector)
empsector_schtype <- subgroup_summary(base.data,~SchemeType,~PreSector)
empsector_orireg <- subgroup_summary(base.data,~OriginRegion,~PreSector)
empsector_jacs <- subgroup_summary(base.data,~JacsCat,~PreSector) %>% filter(!JacsCat=="NA", sum(freq)>20)
empsector_score <- base.data %>% group_by(PreSector) %>% score_summary

# Pre-scholarship employer supportiveness
emppresupport_gender <- subgroup_summary(base.data,~Gender,~PreSupportiveness)
emppresupport_sch <- subgroup_summary(base.data,~SchemeNom,~PreSupportiveness)
emppresupport_schtype <- subgroup_summary(base.data,~SchemeType,~PreSupportiveness)
emppresupport_orireg <- subgroup_summary(base.data,~OriginRegion,~PreSupportiveness)
emppresupport_jacs <- subgroup_summary(base.data,~JacsCat,~PreSupportiveness) %>% filter(!JacsCat=="NA", sum(freq)>20)
emppresupport_score <- base.data %>% group_by(PreSupportiveness) %>% score_summary

# Employment skill level match
empskillmatch_gender <- base.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~Gender,~CurrentSkillMatch)
empskillmatch_sch <- base.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~SchemeNom,~CurrentSkillMatch)
empskillmatch_schtype <- base.data %>% filter(!CurrentSkillMatch=="NA") %>% subgroup_summary(~SchemeType,~CurrentSkillMatch)
empskillmatch_orireg <-base.data %>% filter(!CurrentSkillMatch=="NA") %>%  subgroup_summary(~OriginRegion,~CurrentSkillMatch)
empskillmatch_jacs <- base.data %>% filter(!JacsCat=="NA",!CurrentSkillMatch=="NA") %>% subgroup_summary(~JacsCat,~CurrentSkillMatch) %>% filter(sum(freq)>20)
empskillmatch_score <- base.data %>% group_by(CurrentSkillMatch) %>% score_summary

# Job changes in the last 2 years 
empchange_gender <- base.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~Gender,~CurrentJobChange)
empchange_sch <- base.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~Gender,~CurrentJobChange)
empchange_schtype <- base.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~Gender,~CurrentJobChange)
empchange_orireg <- base.data %>% filter(!CurrentJobChange=="NA") %>% subgroup_summary(~Gender,~CurrentJobChange)
empchange_jacs <- base.data %>% filter(!JacsCat=="NA",!CurrentJobChange=="NA") %>% subgroup_summary(~JacsCat,~CurrentJobChange) %>% filter(sum(freq)>20)
empchange_score <- base.data %>% filter(!CurrentJobChange=="NA") %>% group_by(CurrentJobChange) %>% score_summary()

# Current employment status (while on studies)
empcurrent_gender <- subgroup_summary(base.data,~Gender,~CurrentStatus)
empcurrent_sch <- subgroup_summary(base.data,~SchemeNom,~CurrentStatus)
empcurrent_schtype <- subgroup_summary(base.data,~SchemeType,~CurrentStatus)
empcurrent_orireg <- subgroup_summary(base.data,~OriginRegion,~CurrentStatus)
empcurrent_jacs <- subgroup_summary(base.data,~JacsCat,~CurrentStatus) %>% filter(!JacsCat=="NA", sum(freq)>20)
empcurrent_score <- base.data %>% group_by(CurrentStatus) %>% score_summary

# Bond arrangements with current employers
empbond_gender <- subgroup_summary(base.data,~Gender,~Bond)
empbond_sch <- subgroup_summary(base.data,~SchemeNom,~Bond)
empbond_schtype <- subgroup_summary(base.data,~SchemeType,~Bond)
empbond_orireg <- subgroup_summary(base.data,~OriginRegion,~Bond)
empbond_jacs <- subgroup_summary(base.data,~JacsCat,~Bond) %>% filter(!JacsCat=="NA", sum(freq)>20)
empbond_score <- base.data %>% group_by(Bond) %>% score_summary


## e] Leadership ----

# prefix = "ldr" and "Inn"
# LEADERSHIP QUESTIONS, LEADERSHIP INDEX, INNOVATION QUESTIONS

# Overall
ldrbudget_overall <- pop_summary(base.data,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_overall<- pop_summary(base.data,~LdrSupervising)
ldrmanaging_overall <- pop_summary(base.data,~LdrManaging)
ldrstrategy_overall <- pop_summary(base.data,~LdrStrategy)
ldrinnproject_overall <- pop_summary(base.data,~InnLeadProject)
ldrinnfunding_overall <- pop_summary(base.data,~InnLeadFunding) 

#Gender
ldrbudget_gender <- subgroup_summary(base.data,~Gender,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_gender <- subgroup_summary(base.data,~Gender,~LdrSupervising)
ldrmanaging_gender <- subgroup_summary(base.data,~Gender, ~LdrManaging)
ldrstrategy_gender <- subgroup_summary(base.data,~Gender,~LdrStrategy)
ldrinnproject_gender <- subgroup_summary(base.data,~Gender,~InnLeadProject)
ldrinnfunding_gender <- subgroup_summary(base.data,~Gender,~InnLeadFunding) 

#Scheme
ldrbudget_sch <- subgroup_summary(base.data,~SchemeNom,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_sch <- subgroup_summary(base.data,~SchemeNom,~LdrSupervising)
ldrmanaging_sch <- subgroup_summary(base.data,~SchemeNom, ~LdrManaging)
ldrstrategy_sch <- subgroup_summary(base.data,~SchemeNom,~LdrStrategy)
ldrinnproject_sch <- subgroup_summary(base.data,~SchemeNom,~InnLeadProject)
ldrinnfunding_sch <- subgroup_summary(base.data,~SchemeNom,~InnLeadFunding) 

#Scheme Type
ldrbudget_schtype <- subgroup_summary(base.data,~SchemeType,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_schtype <- subgroup_summary(base.data,~SchemeType,~LdrSupervising)
ldrmanaging_schtype <- subgroup_summary(base.data,~SchemeType, ~LdrManaging)
ldrstrategy_schtype <- subgroup_summary(base.data,~SchemeType,~LdrStrategy)
ldrinnproject_schtype <- subgroup_summary(base.data,~SchemeType,~InnLeadProject)
ldrinnfunding_schtype <- subgroup_summary(base.data,~SchemeType,~InnLeadFunding) 

#Origin region
ldrbudget_orireg <- subgroup_summary(base.data,~OriginRegion,~LdrBudget) #NA = question not asked: not currently employed
ldrsupervise_orireg <- subgroup_summary(base.data,~OriginRegion,~LdrSupervising)
ldrmanaging_orireg <- subgroup_summary(base.data,~OriginRegion, ~LdrManaging)
ldrstrategy_orireg <- subgroup_summary(base.data,~OriginRegion,~LdrStrategy)
ldrinnproject_orireg <- subgroup_summary(base.data,~OriginRegion,~InnLeadProject)
ldrinnfunding_orireg <- subgroup_summary(base.data,~OriginRegion,~InnLeadFunding) 

#Subject studied
ldrbudget_jacs <- subgroup_summary(base.data,~JacsCat,~LdrBudget) %>% filter(!JacsCat=="NA", sum(freq)>20) #NA = question not asked: not currently employed
ldrsupervise_jacs <- subgroup_summary(base.data,~JacsCat,~LdrSupervising) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrmanaging_jacs <- subgroup_summary(base.data,~JacsCat, ~LdrManaging) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrstrategy_jacs <- subgroup_summary(base.data,~JacsCat,~LdrStrategy) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrinnproject_jacs <- subgroup_summary(base.data,~JacsCat,~InnLeadProject) %>% filter(!JacsCat=="NA", sum(freq)>20)
ldrinnfunding_jacs <- subgroup_summary(base.data,~JacsCat,~InnLeadFunding)  %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee score
ldrbudget_score <- base.data %>% group_by(LdrBudget) %>% score_summary()
ldrsupervise_score <- base.data %>% group_by(LdrSupervising) %>% score_summary()
ldrmanaging_score <- base.data %>% group_by(LdrManaging) %>% score_summary()
ldrstrategy_score <- base.data %>% group_by(LdrStrategy) %>% score_summary()
ldrinnproject_score <- base.data %>% group_by(InnLeadProject) %>% score_summary()
ldrinnfunding_score <- base.data %>% group_by(InnLeadFunding) %>% score_summary()


## f] Research ----

# Prefix = "res"
# Note that there are usually many NA responses for research variables because these questions are only asked to thsoe that report current involvement in research
# Filter question for research involvement is 'ResMain'
# NA for ResMain is because unemployed and studying respondents are not presented with the ResMain question

#Overall
resmain_overall <- pop_summary(base.data, ~ResMain) 

rescollabauthor_overall <- pop_summary(base.data,~ResCollabAuthor)
rescollabgrant_overall <- pop_summary(base.data,~ResCollabGrant)
rescollabconf_overall <- pop_summary(base.data,~ResCollabConf)

reseditor_overall <- pop_summary(base.data, ~ResEditor)
resreviewer_overall <- pop_summary(base.data, ~ResReviewer)
resorganiser_overall <- pop_summary(base.data, ~ResOrganiser)

rescomparticles_overall <- pop_summary(base.data, ~ResCompArticles)
rescompprojects_overall <- pop_summary(base.data, ~ResCompProjects)
rescompfunding_overall <- pop_summary(base.data, ~ResCompFunding)

#Gender
resmain_gender <- subgroup_summary(base.data,~Gender,~ResMain) 

rescollabauthor_gender <- subgroup_summary(base.data,~Gender,~ResCollabAuthor)
rescollabgrant_gender <- subgroup_summary(base.data,~Gender,~ResCollabGrant)
rescollabconf_gender <- subgroup_summary(base.data,~Gender,~ResCollabConf)

reseditor_gender <- subgroup_summary(base.data,~Gender,~ResEditor)
resreviewer_gender <- subgroup_summary(base.data,~Gender,~ResReviewer)
resorganiser_gender <- subgroup_summary(base.data,~Gender,~ResOrganiser)

rescomparticles_gender <- subgroup_summary(base.data,~Gender,~ResCompArticles)
rescompprojects_gender <- subgroup_summary(base.data,~Gender,~ResCompProjects)
rescompfunding_gender <- subgroup_summary(base.data,~Gender,~ResCompFunding)

#Scheme
resmain_sch <- subgroup_summary(base.data,~SchemeNom,~ResMain) 

rescollabauthor_sch <- subgroup_summary(base.data,~SchemeNom,~ResCollabAuthor)
rescollabgrant_sch <- subgroup_summary(base.data,~SchemeNom,~ResCollabGrant)
rescollabconf_sch <- subgroup_summary(base.data,~SchemeNom,~ResCollabConf)

reseditor_sch <- subgroup_summary(base.data,~SchemeNom,~ResEditor)
resreviewer_sch <- subgroup_summary(base.data,~SchemeNom,~ResReviewer)
resorganiser_sch <- subgroup_summary(base.data,~SchemeNom,~ResOrganiser)

rescomparticles_sch <- subgroup_summary(base.data,~SchemeNom,~ResCompArticles)
rescompprojects_sch <- subgroup_summary(base.data,~SchemeNom,~ResCompProjects)
rescompfunding_sch <- subgroup_summary(base.data,~SchemeNom,~ResCompFunding)

#Scheme Type
resmain_schtype <- subgroup_summary(base.data,~SchemeType,~ResMain) 

rescollabauthor_schtype <- subgroup_summary(base.data,~SchemeType,~ResCollabAuthor)
rescollabgrant_schtype <- subgroup_summary(base.data,~SchemeType,~ResCollabGrant)
rescollabconf_schtype <- subgroup_summary(base.data,~SchemeType,~ResCollabConf)

reseditor_schtype <- subgroup_summary(base.data,~SchemeType,~ResEditor)
resreviewer_schtype <- subgroup_summary(base.data,~SchemeType,~ResReviewer)
resorganiser_schtype <- subgroup_summary(base.data,~SchemeType,~ResOrganiser)

rescomparticles_schtype <- subgroup_summary(base.data,~SchemeType,~ResCompArticles)
rescompprojects_schtype <- subgroup_summary(base.data,~SchemeType,~ResCompProjects)
rescompfunding_schtype <- subgroup_summary(base.data,~SchemeType,~ResCompFunding)

#Origin Region
resmain_orireg <- subgroup_summary(base.data,~OriginRegion,~ResMain) 

rescollabauthor_orireg <- subgroup_summary(base.data,~OriginRegion,~ResCollabAuthor)
rescollabgrant_orireg <- subgroup_summary(base.data,~OriginRegion,~ResCollabGrant)
rescollabconf_orireg <- subgroup_summary(base.data,~OriginRegion,~ResCollabConf)

reseditor_orireg <- subgroup_summary(base.data,~OriginRegion,~ResEditor)
resreviewer_orireg <- subgroup_summary(base.data,~OriginRegion,~ResReviewer)
resorganiser_orireg <- subgroup_summary(base.data,~OriginRegion,~ResOrganiser)

rescomparticles_orireg <- subgroup_summary(base.data,~OriginRegion,~ResCompArticles)
rescompprojects_orireg <- subgroup_summary(base.data,~OriginRegion,~ResCompProjects)
rescompfunding_orireg <- subgroup_summary(base.data,~OriginRegion,~ResCompFunding)

#Subect Studied
resmain_jacs <- subgroup_summary(base.data,~JacsCat,~ResMain) %>% filter(!JacsCat=="NA", sum(freq)>20)

rescollabauthor_jacs <- subgroup_summary(base.data,~JacsCat,~ResCollabAuthor) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescollabgrant_jacs <- subgroup_summary(base.data,~JacsCat,~ResCollabGrant) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescollabconf_jacs <- subgroup_summary(base.data,~JacsCat,~ResCollabConf) %>% filter(!JacsCat=="NA", sum(freq)>20)

reseditor_jacs <- subgroup_summary(base.data,~JacsCat,~ResEditor) %>% filter(!JacsCat=="NA", sum(freq)>20)
resreviewer_jacs <- subgroup_summary(base.data,~JacsCat,~ResReviewer) %>% filter(!JacsCat=="NA", sum(freq)>20)
resorganiser_jacs <- subgroup_summary(base.data,~JacsCat,~ResOrganiser) %>% filter(!JacsCat=="NA", sum(freq)>20)

rescomparticles_jacs <- subgroup_summary(base.data,~JacsCat,~ResCompArticles) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescompprojects_jacs <- subgroup_summary(base.data,~JacsCat,~ResCompProjects) %>% filter(!JacsCat=="NA", sum(freq)>20)
rescompfunding_jacs <- subgroup_summary(base.data,~JacsCat,~ResCompFunding) %>% filter(!JacsCat=="NA", sum(freq)>20)

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


## g] Teaching ----

# prefix = "teach"

#Overall
teachmain_overall <- pop_summary(base.data,~TeachMain)

teachschool_overall <- pop_summary(base.data,~TeachSchool)
teachundergrad_overall <- pop_summary(base.data,~TeachUndergrad)
teachpostgrad_overall <- pop_summary(base.data,~TeachPostgrad)
teachdoctorate_overall <- pop_summary(base.data,~TeachDoctorate)
teachtvet_overall <- pop_summary(base.data,~TeachTVET)

#Gender
teachmain_gender <- subgroup_summary(base.data,~Gender,~TeachMain)

teachschool_gender <- subgroup_summary(base.data,~Gender,~TeachSchool)
teachundergrad_gender <- subgroup_summary(base.data,~Gender,~TeachUndergrad)
teachpostgrad_gender <- subgroup_summary(base.data,~Gender,~TeachPostgrad)
teachdoctorate_gender <- subgroup_summary(base.data,~Gender,~TeachDoctorate)
teachtvet_gender <- subgroup_summary(base.data,~Gender,~TeachTVET)

#Scheme
teachmain_sch <- subgroup_summary(base.data,~SchemeNom,~TeachMain)

teachschool_sch <- subgroup_summary(base.data,~SchemeNom,~TeachSchool)
teachundergrad_sch <- subgroup_summary(base.data,~SchemeNom,~TeachUndergrad)
teachpostgrad_sch <- subgroup_summary(base.data,~SchemeNom,~TeachPostgrad)
teachdoctorate_sch <- subgroup_summary(base.data,~SchemeNom,~TeachDoctorate)
teachtvet_sch <- subgroup_summary(base.data,~SchemeNom,~TeachTVET)

#Scheme Type
teachmain_schtype <- subgroup_summary(base.data,~SchemeType,~TeachMain)

teachschool_schtype <- subgroup_summary(base.data,~SchemeType,~TeachSchool)
teachundergrad_schtype <- subgroup_summary(base.data,~SchemeType,~TeachUndergrad)
teachpostgrad_schtype <- subgroup_summary(base.data,~SchemeType,~TeachPostgrad)
teachdoctorate_schtype <- subgroup_summary(base.data,~SchemeType,~TeachDoctorate)
teachtvet_schtype <- subgroup_summary(base.data,~SchemeType,~TeachTVET)

#Origin Region
teachmain_orireg <- subgroup_summary(base.data,~OriginRegion,~TeachMain)

teachschool_orireg <- subgroup_summary(base.data,~OriginRegion,~TeachSchool)
teachundergrad_orireg <- subgroup_summary(base.data,~OriginRegion,~TeachUndergrad)
teachpostgrad_orireg <- subgroup_summary(base.data,~OriginRegion,~TeachPostgrad)
teachdoctorate_orireg <- subgroup_summary(base.data,~OriginRegion,~TeachDoctorate)
teachtvet_orireg <- subgroup_summary(base.data,~OriginRegion,~TeachTVET)

#Subect Studied
teachmain_jacs <- subgroup_summary(base.data,~JacsCat,~TeachMain) %>% filter(!JacsCat=="NA", sum(freq)>20)

teachschool_jacs <- subgroup_summary(base.data,~JacsCat,~TeachSchool) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachundergrad_jacs <- subgroup_summary(base.data,~JacsCat,~TeachUndergrad) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachpostgrad_jacs <- subgroup_summary(base.data,~JacsCat,~TeachPostgrad) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachdoctorate_jacs <- subgroup_summary(base.data,~JacsCat,~TeachDoctorate) %>% filter(!JacsCat=="NA", sum(freq)>20)
teachtvet_jacs <- subgroup_summary(base.data,~JacsCat,~TeachTVET) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
teachmain_score <- base.data %>% group_by(TeachMain) %>% score_summary()

teachschool_score <- base.data %>% group_by(TeachSchool) %>% score_summary()
teachundergrad_score <- base.data %>% group_by(TeachUndergrad) %>% score_summary()
teachpostgrad_score <- base.data %>% group_by(TeachPostgrad) %>% score_summary()
teachdoctorate_score <- base.data %>% group_by(TeachDoctorate) %>% score_summary()
teachtvet_score <- base.data %>% group_by(TeachTVET) %>% score_summary()


## h] Networks and links----

# prefix= 'Net'

#Overall
netacad_overall <- pop_summary(base.data,~NetAcad)
netuk_overall <- pop_summary(base.data,~NetUK)
nethome_overall <- pop_summary(base.data,~NetHome)
netother_overall <- pop_summary(base.data,~NetOther)
netpersonal_overall <- pop_summary(base.data,~NetPersonal)

netinfacad_overall <- pop_summary(base.data,~NetInfluenceAcad)
netinfuk_overall <- pop_summary(base.data,~NetInfluenceUK)
netinforigin_overall <- pop_summary(base.data,~NetInfluenceOrigin)
netinfother_overall <- pop_summary(base.data,~NetInfluenceOther)
netinfpersonal_overall <- pop_summary(base.data,~NetInfluencePersonal)

#Gender
netacad_gender <- subgroup_summary(base.data,~Gender,~NetAcad)
netuk_gender <- subgroup_summary(base.data,~Gender,~NetUK)
nethome_gender <- subgroup_summary(base.data,~Gender,~NetHome)
netother_gender <- subgroup_summary(base.data,~Gender,~NetOther)
netpersonal_gender <- subgroup_summary(base.data,~Gender,~NetPersonal)

netinfacad_gender <- subgroup_summary(base.data,~Gender,~NetInfluenceAcad)
netinfuk_gender <- subgroup_summary(base.data,~Gender,~NetInfluenceUK)
netinforigin_gender <- subgroup_summary(base.data,~Gender,~NetInfluenceOrigin)
netinfother_gender <- subgroup_summary(base.data,~Gender,~NetInfluenceOther)
netinfpersonal_gender <- subgroup_summary(base.data,~Gender,~NetInfluencePersonal)

#Scheme
netacad_sch <- subgroup_summary(base.data,~SchemeNom,~NetAcad)
netuk_sch <- subgroup_summary(base.data,~SchemeNom,~NetUK)
nethome_sch <- subgroup_summary(base.data,~SchemeNom,~NetHome)
netother_sch <- subgroup_summary(base.data,~SchemeNom,~NetOther)
netpersonal_sch <- subgroup_summary(base.data,~SchemeNom,~NetPersonal)

netinfacad_sch <- subgroup_summary(base.data,~SchemeNom,~NetInfluenceAcad)
netinfuk_sch <- subgroup_summary(base.data,~SchemeNom,~NetInfluenceUK)
netinforigin_sch <- subgroup_summary(base.data,~SchemeNom,~NetInfluenceOrigin)
netinfother_sch <- subgroup_summary(base.data,~SchemeNom,~NetInfluenceOther)
netinfpersonal_sch <- subgroup_summary(base.data,~SchemeNom,~NetInfluencePersonal)

#Scheme Type
netacad_schtype <- subgroup_summary(base.data,~SchemeType,~NetAcad)
netuk_schtype <- subgroup_summary(base.data,~SchemeType,~NetUK)
nethome_schtype <- subgroup_summary(base.data,~SchemeType,~NetHome)
netother_schtype <- subgroup_summary(base.data,~SchemeType,~NetOther)
netpersonal_schtype <- subgroup_summary(base.data,~SchemeType,~NetPersonal)

netinfacad_schtype <- subgroup_summary(base.data,~SchemeType,~NetInfluenceAcad)
netinfuk_schtype <- subgroup_summary(base.data,~SchemeType,~NetInfluenceUK)
netinforigin_schtype <- subgroup_summary(base.data,~SchemeType,~NetInfluenceOrigin)
netinfother_schtype <- subgroup_summary(base.data,~SchemeType,~NetInfluenceOther)
netinfpersonal_schtype <- subgroup_summary(base.data,~SchemeType,~NetInfluencePersonal)

#Origin Region
netacad_orireg <- subgroup_summary(base.data,~OriginRegion,~NetAcad)
netuk_orireg <- subgroup_summary(base.data,~OriginRegion,~NetUK)
nethome_orireg <- subgroup_summary(base.data,~OriginRegion,~NetHome)
netother_orireg <- subgroup_summary(base.data,~OriginRegion,~NetOther)
netpersonal_orireg <- subgroup_summary(base.data,~OriginRegion,~NetPersonal)

netinfacad_orireg <- subgroup_summary(base.data,~OriginRegion,~NetInfluenceAcad)
netinfuk_orireg <- subgroup_summary(base.data,~OriginRegion,~NetInfluenceUK)
netinforigin_orireg <- subgroup_summary(base.data,~OriginRegion,~NetInfluenceOrigin)
netinfother_orireg <- subgroup_summary(base.data,~OriginRegion,~NetInfluenceOther)
netinfpersonal_orireg <- subgroup_summary(base.data,~OriginRegion,~NetInfluencePersonal)

#Subect Studied
netacad_jacs <- subgroup_summary(base.data,~JacsCat,~NetAcad) %>% filter(!JacsCat=="NA", sum(freq)>20)
netuk_jacs <- subgroup_summary(base.data,~JacsCat,~NetUK) %>% filter(!JacsCat=="NA", sum(freq)>20)
nethome_jacs <- subgroup_summary(base.data,~JacsCat,~NetHome) %>% filter(!JacsCat=="NA", sum(freq)>20)
netother_jacs <- subgroup_summary(base.data,~JacsCat,~NetOther) %>% filter(!JacsCat=="NA", sum(freq)>20)
netpersonal_jacs <- subgroup_summary(base.data,~JacsCat,~NetPersonal) %>% filter(!JacsCat=="NA", sum(freq)>20)

netinfacad_jacs <- subgroup_summary(base.data,~JacsCat,~NetInfluenceAcad) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinfuk_jacs <- subgroup_summary(base.data,~JacsCat,~NetInfluenceUK) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinforigin_jacs <- subgroup_summary(base.data,~JacsCat,~NetInfluenceOrigin) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinfother_jacs <- subgroup_summary(base.data,~JacsCat,~NetInfluenceOther) %>% filter(!JacsCat=="NA", sum(freq)>20)
netinfpersonal_jacs <- subgroup_summary(base.data,~JacsCat,~NetInfluencePersonal) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
netacad_score <- base.data %>% group_by(NetAcad) %>% score_summary()
netuk_score <- base.data %>% group_by(NetUK) %>% score_summary()
nethome_score <- base.data %>% group_by(NetHome) %>% score_summary()
netother_score <- base.data %>% group_by(NetOther) %>% score_summary()
netpersonal_score <- base.data %>% group_by(NetPersonal) %>% score_summary()

netinfacad_score <- base.data %>% group_by(NetInfluenceAcad) %>% score_summary()
netinfuk_score <- base.data %>% group_by(NetInfluenceUK) %>% score_summary()
netinforigin_score <- base.data %>% group_by(NetInfluenceOrigin) %>% score_summary()
netinfother_score <- base.data %>% group_by(NetInfluenceOther) %>% score_summary()
netinfpersonal_score <- base.data %>% group_by(NetInfluencePersonal) %>% score_summary()

## i] Broader impact----

# prefix = "Imp"

#Overall
impinstitutional_overall <- pop_summary(base.data,~ImpInstitutional)
implocal_overall <- pop_summary(base.data,~ImpLocal)
impnational_overall <- pop_summary(base.data,~ImpNational)
impinternational_overall <- pop_summary(base.data,~ImpInternational)

impsocial_overall <- pop_summary(base.data,~ImpSocial)
impcivic_overall <- pop_summary(base.data,~ImpCivic)
impecon_overall <- pop_summary(base.data,~ImpEcon)
imppolicy_overall <- pop_summary(base.data,~ImpPolicy)

#Gender
impinstitutional_gender <- subgroup_summary(base.data,~Gender,~ImpInstitutional)
implocal_gender <- subgroup_summary(base.data,~Gender,~ImpLocal)
impnational_gender <- subgroup_summary(base.data,~Gender,~ImpNational)
impinternational_gender <- subgroup_summary(base.data,~Gender,~ImpInternational)

impsocial_gender <- subgroup_summary(base.data,~Gender,~ImpSocial)
impcivic_gender <- subgroup_summary(base.data,~Gender,~ImpCivic)
impecon_gender <- subgroup_summary(base.data,~Gender,~ImpEcon)
imppolicy_gender <- subgroup_summary(base.data,~Gender,~ImpPolicy)

#Scheme
impinstitutional_sch <- subgroup_summary(base.data,~SchemeNom,~ImpInstitutional)
implocal_sch <- subgroup_summary(base.data,~SchemeNom,~ImpLocal)
impnational_sch <- subgroup_summary(base.data,~SchemeNom,~ImpNational)
impinternational_sch <- subgroup_summary(base.data,~SchemeNom,~ImpInternational)

impsocial_sch <- subgroup_summary(base.data,~SchemeNom,~ImpSocial)
impcivic_sch <- subgroup_summary(base.data,~SchemeNom,~ImpCivic)
impecon_sch <- subgroup_summary(base.data,~SchemeNom,~ImpEcon)
imppolicy_sch <- subgroup_summary(base.data,~SchemeNom,~ImpPolicy)

#Scheme Type
impinstitutional_schtype <- subgroup_summary(base.data,~SchemeType,~ImpInstitutional)
implocal_schtype <- subgroup_summary(base.data,~SchemeType,~ImpLocal)
impnational_schtype <- subgroup_summary(base.data,~SchemeType,~ImpNational)
impinternational_schtype <- subgroup_summary(base.data,~SchemeType,~ImpInternational)

impsocial_schtype <- subgroup_summary(base.data,~SchemeType,~ImpSocial)
impcivic_schtype <- subgroup_summary(base.data,~SchemeType,~ImpCivic)
impecon_schtype <- subgroup_summary(base.data,~SchemeType,~ImpEcon)
imppolicy_schtype <- subgroup_summary(base.data,~SchemeType,~ImpPolicy)

#Origin Region
impinstitutional_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpInstitutional)
implocal_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpLocal)
impnational_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpNational)
impinternational_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpInternational)

impsocial_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpSocial)
impcivic_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpCivic)
impecon_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpEcon)
imppolicy_orireg <- subgroup_summary(base.data,~OriginRegion,~ImpPolicy)

#Subect Studied
impinstitutional_jacs <- subgroup_summary(base.data,~JacsCat,~ImpInstitutional) %>% filter(!JacsCat=="NA", sum(freq)>20)
implocal_jacs <- subgroup_summary(base.data,~JacsCat,~ImpLocal) %>% filter(!JacsCat=="NA", sum(freq)>20)
impnational_jacs <- subgroup_summary(base.data,~JacsCat,~ImpNational) %>% filter(!JacsCat=="NA", sum(freq)>20)
impinternational_jacs <- subgroup_summary(base.data,~JacsCat,~ImpInternational) %>% filter(!JacsCat=="NA", sum(freq)>20)

impsocial_jacs <- subgroup_summary(base.data,~JacsCat,~ImpSocial) %>% filter(!JacsCat=="NA", sum(freq)>20)
impcivic_jacs <- subgroup_summary(base.data,~JacsCat,~ImpCivic) %>% filter(!JacsCat=="NA", sum(freq)>20)
impecon_jacs <- subgroup_summary(base.data,~JacsCat,~ImpEcon) %>% filter(!JacsCat=="NA", sum(freq)>20)
imppolicy_jacs <- subgroup_summary(base.data,~JacsCat,~ImpPolicy) %>% filter(!JacsCat=="NA", sum(freq)>20)

#Committee Score
impinstitutional_score <- base.data %>% group_by(ImpInstitutional) %>% score_summary()
implocal_score <- base.data %>% group_by(ImpLocal) %>% score_summary()
impnational_score <- base.data %>% group_by(ImpNational) %>% score_summary()
impinternational_score <- base.data %>% group_by(ImpInternational) %>% score_summary()

impsocial_score <- base.data %>% group_by(ImpSocial) %>% score_summary()
impcivic_score <- base.data %>% group_by(ImpCivic) %>% score_summary()
impecon_score <- base.data %>% group_by(ImpEcon) %>% score_summary()
imppolicy_score <- base.data %>% group_by(ImpPolicy) %>% score_summary()

## j] Analytic indices----

#prefix = "i."

#Overall

indices <- select(base.data, starts_with("i."))

index_overall <- rbind(sapply(indices, summary),SD=sapply(indices, sd)) %>% round(2)

#Gender
indexldr_gender <- base.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) ) 

indexcollab_gender <- 
  base.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexresearch_gender <- 
  base.data %>% 
  group_by(Gender) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Scheme
indexldr_sch <- base.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_sch <- 
  base.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexresearch_sch <- 
  base.data %>% 
  group_by(SchemeNom) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Scheme Type
indexldr_schtype <- 
  base.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_schtype <- 
  base.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexresearch_schtype <- 
  base.data %>% 
  group_by(SchemeType) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Origin Region
indexldr_orireg <- base.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) )

indexcollab_orireg <- 
  base.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) )

indexresearch_orireg <- 
  base.data %>% 
  group_by(OriginRegion) %>% 
  summarise(Count = n(),
            Median = round(median(i.research),2),
            Mean = round(mean(i.research),2),
            SD = round(sd(i.research),2),
            Max = round(max(i.research),2),
            Min = round(min(i.research),2) )

#Subect Studied
indexldr_jacs <- 
  base.data %>% 
  group_by(JacsCat) %>%
  summarise(Count = n(),
            Median = round(median(i.ldr),2),
            Mean = round(mean(i.ldr),2),
            SD = round(sd(i.ldr),2),
            Max = round(max(i.ldr),2),
            Min = round(min(i.ldr),2) ) %>% 
  filter(!JacsCat=="NA", Count>20)

indexcollab_jacs <- 
  base.data %>% 
  group_by(JacsCat) %>% 
  summarise(Count = n(),
            Median = round(median(i.collab),2),
            Mean = round(mean(i.collab),2),
            SD = round(sd(i.collab),2),
            Max = round(max(i.collab),2),
            Min = round(min(i.collab),2) ) %>% 
  filter(!JacsCat=="NA", Count>20)

indexresearch_jacs <- 
  base.data %>% 
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
  ggplot(base.data, aes(x=ZCtteeScore, y=i.ldr)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()

indexcollab_score <- 
  ggplot(base.data, aes(x=ZCtteeScore, y=i.collab)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()

indexresearch_score <- 
  ggplot(base.data, aes(x=ZCtteeScore, y=i.research)) + 
  geom_jitter() +
  coord_cartesian(ylim=c(0,1.0)) +
  ylab("Leadership index") +
  xlab("Committee Z-score (SD)") +
  theme_bw()
