
library(knitr)
library(kableExtra)
library(tidyverse)
library(lubridate)
library(janitor)

# Read in data
Survey <- readr::read_csv("data/results-survey411257-n.csv", 
                          col_types = cols(submitdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                           startdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                           datestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 

# Data preprocess
Survey <- Survey %>%
  mutate(Datum = date(datestamp)) %>%
  select(Datum, C1, C2) %>%
  na.omit() %>%
  mutate(C1.Correct = ifelse(C1=="do not know", TRUE, FALSE),
         C2.Correct = ifelse(C2=="y will increase", TRUE, FALSE)) %>%
  rename("Without DAG" = C1, "With DAG" = C2) 


SurveyTable <- Survey %>%
  tabyl(`Without DAG`, `With DAG`) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_title("combined")

SurveyTable %>%
  kbl() %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(1, background = "lightgreen") %>%
  column_spec(4, background = "lightgreen") %>%
  row_spec(4, italic = TRUE) %>%
  column_spec(5, italic = TRUE)
