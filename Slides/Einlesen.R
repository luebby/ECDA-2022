library(tidyverse)
library(lubridate)


# Read in data
Survey <- readr::read_csv("results-survey411257.csv", 
                          col_types = cols(submitdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                           startdate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                           datestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) 

# Data preprocess
Survey <- Survey %>%
  mutate(date = date(datestamp)) %>%
  select(date, C1, C2) %>%
  na.omit() %>%
  mutate(C1.Correct = ifelse(C1=="do not know", TRUE, FALSE),
         C2.Correct = ifelse(C2=="y will increase", TRUE, FALSE)) 

Surveyn10 <- Survey %>%
  group_by(date) %>%
  summarise(n=n()) %>%
  filter(n>=10) %>%
  ungroup() %>%
  mutate(Event=paste0("e",1:n())) %>%
  mutate(Type = case_when(Event == "e1" ~ "Instructors",
                          Event == "e2" ~ "Practioners",
                          Event == "e3" ~ "Students",
                          Event == "e4" ~ "Students",
                          Event == "e5" ~ "Instructors",
                          Event == "e6" ~ "Students",
                          Event == "e7" ~ "Causal Meeting",
                          Event == "e8" ~ "Practioners",
                          Event == "e9" ~ "Practioners",
                          Event == "e10" ~ "Students"))

Survey <- Survey %>%
  inner_join(Surveyn10, by = "date") %>%
  select(Event, Type, C1, C2, C1.Correct, C2.Correct)

write.csv2(Survey, "Inference.csv", row.names = FALSE)
