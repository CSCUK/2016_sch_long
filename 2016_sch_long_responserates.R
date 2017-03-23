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

#Currently this has to be down via importing responses then joining to evalInfo table: could not make a single query version work

res.data <- sqlQuery(con.evaldb, "
                    SELECT 
                      tbl_Ctrl_Respondents.AWDID,
                      tbl_LKUP_ResponseStatus.ResponseName AS Response,
                      tbl_Ctrl_Respondents.SurveyID
                    FROM
                      tbl_Ctrl_Respondents
                      LEFT JOIN tbl_LKUP_ResponseStatus ON tbl_Ctrl_Respondents.ResponseStatus = tbl_LKUP_ResponseStatus.StatusCode
                    WHERE
                      tbl_Ctrl_Respondents.SurveyID Like '%2014_Two' Or
                      tbl_Ctrl_Respondents.SurveyID Like '%2012_Four' Or 
                      tbl_Ctrl_Respondents.SurveyID Like '%2010_Six' Or 
                      tbl_Ctrl_Respondents.SurveyID Like '%2008_Eight' Or 
                      tbl_Ctrl_Respondents.SurveyID Like '%2006_Ten';                
                    ") %>% 
            left_join(
            sqlQuery(con.evaldb,"
                    SELECT
                      tbl_Ctrl_EvalInfo.AWDID AS AWDID,
                      tbl_Ctrl_EvalInfo.AWDSCH AS Scheme,
                      tbl_Ctrl_EvalInfo.SchemeType AS SchemeType,
                      tbl_Ctrl_EvalInfo.PhD AS PhD,
                      tbl_Ctrl_EvalInfo.YearGroup AS YearGroup,
                      tbl_Ctrl_EvalInfo.Origin AS Origin,
                      tbl_LKUP_Geodata.CSCRegion AS Region
                    FROM
                      tbl_Ctrl_EvalInfo
                      LEFT JOIN tbl_LKUP_Geodata ON tbl_LKUP_Geodata.CTRYNAME = tbl_Ctrl_EvalInfo.Origin
                             "),  
                    by= "AWDID") %>% 
            tbl_df

odbcCloseAll()

res.data <- res.data %>% 
            mutate(SchemeNom = recode(Scheme, "CD"="Distance Learners",
                                              "CR"="Agency: Developed",
                                              "CA"="University Staff", 
                                              "CS"="Agency: Developing",
                                              "CN"="Split Site",
                                              "SS"="Shared Scholars") )

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
  