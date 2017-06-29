# Code information ----

# Alumni (long) and baseline (base) surveys: infographic data export
# Matt Mawer, The Association of Commonwealth Universities
# May, 2017

# Script designed to export data for standard infographics on alumni follow-up and baseline surveys
# If problems occur with the write.xlsx functions (e.g. error about zipping workbook and use of rtools), check this SO solution:
# http://stackoverflow.com/questions/27952451/error-zipping-up-workbook-failed-when-trying-to-write-xlsx

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats)

opar = par()

## --- Data access ----

source("S:/SCHOLARSHIPS/CSC/SCHEMES/CSFP-IN/CSC-Evaluation/Data Management Crystal Snap IT/r_codebank/2016_sch_long/2016_sch_long_basicanalysis.r")

## --- ALUMNI INFOGRAPHIC ----

# --- 1] Responses ----

# Response rate
response_rate <- resp_overall %>% filter(Response=="Complete")
response_countries <- resp_ctry %>% arrange(desc(Rate)) %>% filter(sum(freq)>30, Response=="Complete")
response_countries_toptail <- bind_rows(resp_ctry %>% arrange(desc(Rate)) %>% filter(sum(freq)>30, Response=="Complete") %>% head(3) %>% mutate(Place="Top 3"),
                                resp_ctry %>% arrange(desc(Rate)) %>% filter(sum(freq)>30, Response=="Complete") %>% tail(3) %>% mutate(Place="Bottom 3"))

# Employment sectors of respondents
employment_sectors <- select(empsector_overall,Sector=Response,Proportion=prop) %>% filter(!Sector=="NA") %>% arrange(desc(Proportion))

# --- 2] Impact ----

impact_reach <- 
  bind_rows(impinstitutional_overall,implocal_overall,impnational_overall,impinternational_overall) %>% 
  filter(Response=="Yes") %>% 
  mutate(Variable = recode(Variable, "ImpInstitutional"="Institutional","ImpLocal"="Local","ImpNational"="National","ImpInternational"="International")) %>% 
  select(-Response,"Impact reach"=Variable,Proportion=prop)

impact_type <- 
  bind_rows(impsocial_overall,impcivic_overall,impecon_overall,imppolicy_overall) %>% 
  filter(Response=="Yes") %>% 
  mutate(Variable = recode(Variable, "ImpCivic" = "Civic Engagement", "ImpEcon"="Economic Development",
                           "ImpPolicy"="Policymaking","ImpSocial"="Social Development")) %>% 
  select(-Response,"Impact type"=Variable, Proportion=prop)
  

# --- 3] Skills ----

skills_applying <- 
  bind_rows(appskillwork_overall, appskillnonwork_overall,appapproach_overall) %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1)) %>% 
  select("Applying Skills"=Response, everything())

skills_transfering <- 
  bind_rows(apptrain_overall,appresources_overall) %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1)) %>% 
  select("Transferring Skills"=Response, everything())

skills_advocating <- 
  bind_rows(appadvocate_overall,appchange_overall) %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1)) %>% 
  select("Advocating change"=Response, everything())

# --- 4] Leadership ----

leadership <- 
  bind_rows(ldrbudget_overall, ldrmanaging_overall, ldrsupervise_overall,ldrstrategy_overall) %>% 
  filter(Response=="Yes") %>% 
  mutate(Variable = fct_recode(Variable,"Overseeing budgets"="LdrBudget","Managing Company / department" = "LdrManaging",
                               "Setting strategy"="LdrStrategy","Supervising others"="LdrSupervising")) %>% 
  select(-Response,Activity=Variable)

# --- 5] Academic research ----

research_involvement <- 
  resmain_overall %>% filter(Response=="Yes") %>% 
  select(-Response) %>% 
  mutate(Variable = fct_recode(Variable,"Research Involvement" = "ResMain"))

research_collaboration <- 
  bind_rows(rescollabauthor_overall,rescollabgrant_overall,rescollabconf_overall) %>%
  filter(!Response=="NA") %>% 
  group_by(Response) %>% 
  summarise(Frequency = sum(freq)) %>% 
  mutate(prop = round((Frequency / sum(Frequency))*100,1)) %>% 
  select(Int.collaboration=Response,everything())
  
# --- 6] Teaching ----

teaching_involvement <- 
  teachmain_overall %>% filter(Response=="Yes") %>% 
  select(-Response) %>% 
  mutate(Variable = fct_recode(Variable,"Teaching Involvement" = "TeachMain"))

teaching_cmw <- select(teachcmwskills_overall,-Variable,"Use CMW skills"=Response)

# --- 7] Professional contacts ----

networks_host <- select(netacad_overall,-Variable,"UK host academics"=Response)

networks_ukprof <- select(netuk_overall,-Variable,"UK professional networks"=Response)

networks_otherprof <- select(netother_overall,-Variable,"Other int.prof. networks"=Response)

# --- Write data to excel ----

# Will export to a single workbook. First line creates workbook: Final line saves the workbook.

wb <- createWorkbook("data_infographic.xlsx")

addWorksheet(wb,"response_rate") 
writeData(wb,"response_rate",response_rate)

addWorksheet(wb,"response_countries")
writeData(wb,"response_countries",response_countries)

addWorksheet(wb,"response_countries_toptail")
writeData(wb,"response_countries_toptail",response_countries_toptail)

addWorksheet(wb,"employment_sectors")
writeData(wb,"employment_sectors",employment_sectors)

addWorksheet(wb,"impact_reach")
writeData(wb,"impact_reach",impact_reach)

addWorksheet(wb,"impact_type")
writeData(wb,"impact_type",impact_type)

addWorksheet(wb,"skills_applying")
writeData(wb,"skills_applying",skills_applying)

addWorksheet(wb,"skills_transfering")
writeData(wb,"skills_transfering",skills_transfering)

addWorksheet(wb,"skills_advocating")
writeData(wb,"skills_advocating",skills_advocating)

addWorksheet(wb,"leadership")
writeData(wb,"leadership",leadership)

addWorksheet(wb,"research_involvement")
writeData(wb,"research_involvement",research_involvement)

addWorksheet(wb,"research_collaboration")
writeData(wb,"research_collaboration",research_collaboration)

addWorksheet(wb,"teaching_involvement")
writeData(wb,"teaching_involvement",teaching_involvement)

addWorksheet(wb,"teaching_cmw")
writeData(wb,"teaching_cmw",teaching_cmw)

addWorksheet(wb,"networks_host")
writeData(wb,"networks_host",networks_host)

addWorksheet(wb,"networks_ukprof")
writeData(wb,"networks_ukprof",networks_ukprof)

addWorksheet(wb,"networks_otherprof")
writeData(wb,"networks_otherprof",networks_otherprof)

saveWorkbook(wb, file = paste("data_infographic.xlsx","_",format(Sys.Date(),"%d%b%Y"),".xlsx",sep=""), overwrite = TRUE)
