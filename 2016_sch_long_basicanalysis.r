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

score_summary <- function(x){
  summarise(
    filter(x,!SchemeNom=="Shared Scholars", !SchemeNom=="Distance Learners"),
    Count = n(),
    Median = round(median(ZCtteeScore, na.rm=T),2),
    Mean = round(mean(ZCtteeScore, na.rm=T),2), 
    SD = round(sd(ZCtteeScore, na.rm=T),2), 
    Max = round(max(ZCtteeScore, na.rm=T),2),
    Min = round(min(ZCtteeScore, na.rm=T),2)
  )}

#This one in progress - to do the grouping, counting and mutation operations in a single function
test <- 
  function(x){
      count(x) %>% 
      mutate(prop = round((n / sum(n))*100,1))
  }

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
overview_gender <- alumni.data %>% count(Gender) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_sch <- alumni.data %>% count(SchemeNom) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_schtype <- alumni.data %>% count(SchemeType) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_year <- alumni.data %>% count(YearGroup) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_orireg <- alumni.data %>% count(OriginRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_resreg <- alumni.data %>% count(ResidencyRegion) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_jacs <- alumni.data %>% count(JacsCat) %>% mutate(prop = round((n / sum(n))*100,1)) %>% arrange(desc(n))
overview_score <- rbind(alumni.data %>% group_by(SchemeType) %>% filter(!Scheme=="SS", !Scheme=="CD")%>% score_summary %>% rename(Competition=SchemeType),
                       (alumni.data %>% group_by(SchemeNom) %>% filter(SchemeNom=="Agency: Developed")%>% score_summary %>% rename(Competition=SchemeNom)))
## IF CTTEEGROUP VARIABLE IN importcleaning.r WORKED CORRECTLY, CAN USE THIS INSTEAD:
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

