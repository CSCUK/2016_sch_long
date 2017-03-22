# Code information ----

# Longitudinal Scholar tracking survey: Response rates
# Matt Mawer, The Association of Commonwealth Universities
# February, 2017

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats, plotly, pander)

opar = par()

# update the SurveyName and FilePath variables to the current values
SurveyName = "2016 Alumni Survey"
FilePath = "S:/SCHOLARSHIPS/CSC/SNAP/Evaluation/Scholar surveys/Data management & seeding/Sch Alumni seeding.xlsx"


# --- Data connection and import ----

## a] DB connections ----

con.evaldb <- odbcConnect("EvalDb")
con.evaldb

## b] Data import ----

# Retrieve geodata from evaluation database
geoData <- sqlQuery(con.evaldb,"
                    SELECT
                      tbl_LKUP_Geodata.CTRYNAME AS Country,
                      tbl_LKUP_Geodata.CSCRegion AS Region
                    FROM
                      tbl_LKUP_Geodata
                    ")

## THIS NEEDS UPDATING TO USE DATABASE LINKS - NO NEED FOR EXCEL IMPORT
res.data <- 
    read.xlsx(FilePath, sheet = "CURRENT SEED", colNames = T, na.strings = "") %>% #get survey data
    left_join(read.xlsx(geoData,sheet = "CSC regions", colNames = T,na.strings = ""), by = c("Origin" = "Country")) %>% #join region data
    select(AWDID, Year.Group = YearGroup, PhD, Gender, Scheme, Award.year, Origin, Region,"Response" = FinalResponseStatus, Alumni.status) %>% #select relevant columns
    mutate(Simple.response = recode(Response, "1"="Completed", .default = "Non-response"), #new variable: binary response
           Response = recode(Response, "1" = "Completed", "2" = "Not.completed", "3"="Email.failed", "4"="Not.included")) %>% # label responses
    tbl_df

#xxx
# --- Analysis ----

## --- a] Basic RR ----

resp.overall <- 
  res.data %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,0))

resp.YearGroup <- 
  res.data %>% 
  group_by(Year.Group) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1))

resp.phd <- 
  res.data %>%
  mutate(PhD = recode(PhD, "0" = "Other", "1" = "PhD")) %>% 
  group_by(PhD) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) 

resp.scheme <- 
  res.data %>% 
  mutate(Scheme = recode(Scheme, "CD"="Distance Learners","CR"="Agency: Developed","CA"="University Staff", "CS"="Agency: Developing","CN"="Split Site","SS"="Shared Scholars")) %>% 
  group_by(Scheme) %>%
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) 

resp.region <- 
  res.data %>% 
  group_by(Region) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1))
  
resp.country <- 
  res.data %>% 
  group_by(Origin) %>% 
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1))


## --- Save output --- ##

save.image("2016_sch_long_responserates.rdata")
  