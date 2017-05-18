# Code information ----

# Alumni (long) and baseline (base) surveys: infographic data export
# Matt Mawer, The Association of Commonwealth Universities
# May, 2017

# Script designed to export data for standard infographics on alumni follow-up and baseline surveys

# Either load the RDATA file directly, or use source() to the importcleaning.r script:
# source("S:/SCHOLARSHIPS/CSC/SCHEMES/CSFP-IN/CSC-Evaluation/Data Management Crystal Snap IT/r_codebank/2016_sch_long/2016_sch_long_basicanalysis.r")
# load("2016_sch_long_core.rdata")

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats, plotly, pander)

opar = par()

# --- Dataset structure ----

str(alumni.data, list.len=nrow(alumni.data))
str(base.data, list.len=nrow(base.data))

## --- ALUMNI INFOGRAPHIC ----

# --- 1] Responses ----

# Response rate
source("S:/SCHOLARSHIPS/CSC/SCHEMES/CSFP-IN/CSC-Evaluation/Data Management Crystal Snap IT/r_codebank/2016_sch_long/2016_sch_long_response_alumni.r")
response.rate <- resp.overall %>% filter(Response=="Completed")
response.countries <- rbind(head(filter(resp.country,sum(n)>30, Response=="Completed"),3),
                            tail(filter(resp.country,sum(n)>30, Response=="Completed"),3))

rm(list= ls()[!(ls() %in% c("opar","response.rate","response.countries"))]) #remove everything that doesn't match this list
gc() #clean the memory

# Employment sectors of respondents
source("S:/SCHOLARSHIPS/CSC/SCHEMES/CSFP-IN/CSC-Evaluation/Data Management Crystal Snap IT/r_codebank/2016_sch_long/2016_sch_long_importcleaning.r")
employment.sectors <- select(empsector_overall,Sector=CurrentSector,prop)

# --- 2] Impact ----

Impact.reach <- 
  bind_rows(impinstitutional_overall,implocal_overall,impnational_overall,impinternational_overall) %>% 
  filter(Response=="Yes") %>% 
  mutate(Variable = recode(Variable, "ImpInstitutional"="Institutional","ImpLocal"="Local","ImpNational"="National","ImpInternational"="International")) %>% 
  select(-Response,"Impact reach"=Variable,prop)


impact.type <- 
  bind_rows(impsocial_overall,impcivic_overall,impecon_overall,imppolicy_overall) %>% 
  filter(Response=="Yes") %>% 
  mutate(Variable = recode(Variable, "ImpCivic" = "Civic Engagement", "ImpEcon"="Economic Development",
                           "ImpPolicy"="Policymaking","ImpSocial"="Social Development")) %>% 
  select(-Response,"Impact type"=Variable, prop)
  

# --- 3] Skills ----

skills.applying <- 
  bind_rows(appskillwork_overall, appskillnonwork_overall,appapproach_overall) %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1))

skills.transfering <- 
  bind_rows(apptrain_overall,appresources_overall) %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1))

skills.advocating <- 
  bind_rows(appadvocate_overall,appchange_overall) %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1))

# --- 4] Leadership ----

leadership <- 
  bind_rows(ldrbudget_overall, ldrmanaging_overall, ldrsupervise_overall,ldrstrategy_overall) %>% 
  filter(Response=="Yes") %>% 
  select(-Response) %>% 
  mutate(Variable = fct_recode(Variable,"Overseeing budgets"="LdrBudget","Managing Company / department" = "LdrManaging",
                               "Setting strategy"="LdrStrategy","Supervising others"="LdrSupervising"))

# --- 5] Academic research ----

research.involvement <- 
  resmain_overall %>% filter(Response=="Yes") %>% 
  select(-Response) %>% 
  mutate(Variable = fct_recode(Variable,"Research Involvement" = "ResMain"))

research.collaboration <- 
  bind_rows(rescollabauthor_overall,rescollabgrant_overall,rescollabconf_overall) %>%
  filter(!Response=="NA") %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1))
  
# --- 6] Teaching ----

teaching.involvement <- 
  teachmain_overall %>% filter(Response=="Yes") %>% 
  select(-Response) %>% 
  mutate(Variable = fct_recode(Variable,"Teaching Involvement" = "TeachMain"))

teaching.cmw <- 
  teachcmwskills_overall

# --- Write data to excel ----

write.xlsx(dataframe1, file="filename.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(dataframe2, file="filename.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)