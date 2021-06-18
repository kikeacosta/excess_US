library(here)
source(here("Code/00_functions.R"))

db_d <- 
  read_rds("Output/weekly_deaths_by_sex_2015_2019.rds")

db_p <- 
  read_rds(here("Output", "pop_interpol_us_week_age5.rds"))

db <- 
  db_d %>% 
  left_join(db_p)
