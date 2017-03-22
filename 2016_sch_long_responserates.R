# Code information ----

# Longitudinal Scholar tracking survey: Response rates
# Matt Mawer, The Association of Commonwealth Universities
# February, 2017

## NOTE - Update years in 'WHERE' statement of database query ('Data connection and import, section [b]) to appropriate values for survey

SurveyName <- "2016 Alumni Survey"

# --- Library calls ----

library(pacman)
p_load(RODBC, openxlsx, tidyverse,forcats, plotly, pander)

opar = par()

# --- Data connection and import ----

## a] DB connections ----

con.evaldb <- odbcConnect("EvalDb")
con.evaldb

## b] Data import ----

res.data <- sqlQuery(con.evaldb,"
                    SELECT
                      tbl_Ctrl_EvalInfo.AWDID AS AWDID
                      tbl_Ctrl_EvalInfo.AWDSCH AS Scheme
                      tbl_Ctrl_EvalInfo.SchemeType AS SchemeType
                      tbl_Ctrl_EvalInfo.PhD AS PhD
                      tbl_Ctrl_EvalInfo.YearGroup AS YearGroup
                      tbl_Ctrl_EvalInfo.Origin AS Country
                      tbl_LKUP_Geodata.CSCRegion AS Region
                      tbl_LKUP_ResponseStatus.ResponseName AS Response
                    From
                      tbl_Ctrl_Respondents
                      LEFT JOIN tbl_LKUP_ResponseStatus ON tbl_LKUP_ResponseStatus.StatusCode = tbl_Ctrl_Respondents.ResponseStatus  
                      LEFT JOIN tbl_Ctrl_EvalInfo ON tbl_Ctrl_EvalInfo.AWDID = tbl_Ctrl_Respondents.AWDID
                      LEFT JOIN tbl_LKUP_Geodata ON tbl_LKUP_Geodata.CTRYNAME = tbl_Ctrl_EvalInfo.Origin
                    Where
                      tbl_Ctrl_EvalInfo.YearGroup IN ('2014','2012','2010','2008','2006')
                    ")
#might need to check WHERE statement

res.data <- res.data %>% 
            mutate(SchemeNom = recode(Scheme, "CD"="Distance Learners",
                                              "CR"="Agency: Developed",
                                              "CA"="University Staff", 
                                              "CS"="Agency: Developing",
                                              "CN"="Split Site",
                                              "SS"="Shared Scholars")
                   )

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

resp.sch <- 
  res.data %>% 
  group_by(SchemeNom) %>%
  count(Response) %>% 
  mutate(Rate = round((n / sum(n))*100,1)) 

resp.schtype <- 
  res.data %>%  
  group_by(SchemeType) %>%
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
  