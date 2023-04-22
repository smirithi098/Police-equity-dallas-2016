setwd("S:/Data visualization")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(parsedate)

dallas_data <- read.csv("37-00049_UOF-P_2016_prepped.csv")

glimpse(dallas_data)

head(dallas_data)

# format the date columns
date_format = "%d-%m-%y"

dallas_data <- dallas_data %>%
  mutate(INCIDENT_DATE = parse_date(INCIDENT_DATE),
         OFFICER_HIRE_DATE = parse_date(OFFICER_HIRE_DATE))

# separate the rows with mutiple columns into individual rows
column_to_separate <- dallas_data %>%
  select(c("SUBJECT_ID", "UOF_NUMBER", "SUBJECT_OFFENSE", "FORCE_EFFECTIVE"))
    
dallas_data <- dallas_data %>% 
  separate_longer_delim(c(UOF_NUMBER, FORCE_EFFECTIVE),
                        delim = ",")


convert_to_factor <- dallas_data %>%
  select(c(5,6,9,11,13:15,17,18,23,34,35,47))