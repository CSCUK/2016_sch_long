# Code information ----

# Longitudinal Scholar tracking survey: response analysis
# Matt Mawer, The Association of Commonwealth Universities
# February, 2017

# update the SurveyName and FilePath variables to the current values
SurveyName = "2016 Alumni Survey"
FilePath = "S:/SCHOLARSHIPS/CSC/SNAP/Evaluation/Scholar surveys/Data management & seeding/Sch Alumni seeding.xlsx"

## --- Packages --- ##

library(pacman)
pacman:: p_load(openxlsx, dplyr, knitr, pander, plotly)
  # Add new packages to be loaded to p_load args

opar = par()
  # Save old graphcis parameters as a backup

## --- Data import --- ##

res.data <- 
    read.xlsx(FilePath, sheet = "CURRENT SEED", colNames = T, na.strings = "") %>% 
    left_join(sqlQuery(con.evaldb,"SELECT tbl_LKUP_Geodata.CTRYNAME AS Country,tbl_LKUP_Geodata.CSCRegion AS Region FROM tbl_LKUP_Geodata"), by=c("Origin"="Country")) %>%   
    select(AWDID, Year.Group = YearGroup, PhD, Gender, Scheme, Award.year, Origin, Region,"Response" = FinalResponseStatus, Alumni.status) %>% #select relevant columns
    mutate(Response = recode(Response, "1" = "Completed", "2" = "Not.completed", "3"="Email.failed", "4"="Not.included")) %>% # label responses
    tbl_df

## --- Analysis --- ##

# overall, gender, scheme, scheme type, yeargroup, origin region, jacs, score

# 1. Response rate data frames #

resp.overall <- 
  res.data %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,0)) %>% 
  arrange(desc(Rate))

resp.YearGroup <- 
  res.data %>% 
  group_by(Year.Group) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) %>% 
  arrange(desc(Rate))

resp.phd <- 
  res.data %>%
  mutate(PhD = recode(PhD, "0" = "Other", "1" = "PhD")) %>% 
  group_by(PhD) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) %>% 
  arrange(desc(Rate))

resp.scheme <- 
  res.data %>% 
  mutate(Scheme = recode(Scheme, "CD"="Distance Learners","CR"="Agency: Developed","CA"="University Staff", "CS"="Agency: Developing","CN"="Split Site","SS"="Shared Scholars")) %>% 
  group_by(Scheme) %>%
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) %>% 
  arrange(desc(Rate))

resp.region <- 
  res.data %>% 
  group_by(Region) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) %>% 
  arrange(desc(Rate))
  
resp.country <- 
  res.data %>% 
  group_by(Origin) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) %>% 
  arrange(desc(Rate))
