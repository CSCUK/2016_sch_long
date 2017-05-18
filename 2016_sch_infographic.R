# Code information ----

# Alumni (long) and baseline (base) surveys: infographic data export
# Matt Mawer, The Association of Commonwealth Universities
# May, 2017

# Script designed to export data for standard infographics on alumni follow-up and baseline surveys

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats)

opar = par()

# --- Dataset structure ----

str(alumni.data, list.len=nrow(alumni.data))

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

teaching.cmw <- select(teachcmwskills_overall,-Variable)

# --- 7] Professional contacts ----

networks.host <- select(netacad_overall,-Variable)

networks.ukprof <- select(netuk_overall,-Variable)

networks.otherprof <- select(netother_overall,-Variable)

# --- Write data to excel ----

# Listed by section, but all will export to a single workbook. First line creates workbook: first line should not have append=TRUE.

write.xlsx(response.rate, file="data_infographic.xlsx", sheetName="response.rate", row.names=FALSE)
write.xlsx(response.countries, file="data_infographic.xlsx", sheetName="response.countries", row.names=FALSE, append=TRUE)
write.xlsx(employment.sectors, file="data_infographic.xlsx", sheetName="employment.sectors", row.names=FALSE, append=TRUE)

write.xlsx(impact.reach, file="data_infographic.xlsx", sheetName="impact.reach", row.names=FALSE, append=TRUE)
write.xlsx(impact.type, file="data_infographic.xlsx", sheetName="impact.type", row.names=FALSE, append=TRUE)

write.xlsx(skills.applying, file="data_infographic.xlsx", sheetName="skills.applying", row.names=FALSE, append=TRUE)
write.xlsx(skills.transfering, file="data_infographic.xlsx", sheetName="skills.transfering", row.names=FALSE, append=TRUE)
write.xlsx(skills.advocating, file="data_infographic.xlsx", sheetName="skills.advocating", row.names=FALSE, append=TRUE)

write.xlsx(leadership, file="data_infographic.xlsx", sheetName="leadership", row.names=FALSE, append=TRUE)

write.xlsx(research.involvement, file="data_infographic.xlsx", sheetName="res.involvement", row.names=FALSE, append=TRUE)
write.xlsx(research.collaboration, file="data_infographic.xlsx", sheetName="res.collaboration", row.names=FALSE, append=TRUE)

write.xlsx(teaching.involvement, file="data_infographic.xlsx", sheetName="teach.involvement", row.names=FALSE, append=TRUE)
write.xlsx(teaching.cmw, file="data_infographic.xlsx", sheetName="teach.CMW", row.names=FALSE, append=TRUE)

write.xlsx(networks.host, file="data_infographic.xlsx", sheetName="employment.sectors", row.names=FALSE, append=TRUE)
write.xlsx(networks.ukprof, file="data_infographic.xlsx", sheetName="employment.sectors", row.names=FALSE, append=TRUE)
write.xlsx(networks.otherprof, file="data_infographic.xlsx", sheetName="employment.sectors", row.names=FALSE, append=TRUE)