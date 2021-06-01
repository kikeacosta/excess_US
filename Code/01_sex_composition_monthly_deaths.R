library(lubridate)
library(tidyverse)

db <- read_tsv("Data/pop by state.txt",
               col_types = cols(.default = "c"))

db_deaths <- tibble()
for(y in 2015:2019){
  temp <- read_tsv(paste0("Data/Underlying Cause of Death, ", y, ".txt"),
                      col_types = cols(.default = "c"))
  
  db_deaths <- 
    db_deaths %>% 
    bind_rows(temp)
  
}

db2 <- 
  db_deaths %>% 
  select(State, 
         Age = `Five-Year Age Groups Code`,
         Sex = 'Gender',
         Year,
         Month = 'Month Code',
         Deaths) %>% 
  drop_na(State)
  
db3 <- 
  db2 %>% 
  mutate(Age = recode(Age,
                      "100+" = "95-99"),
         Age = str_sub(Age, 1, 2),
         Age = recode(Age,
                      "1" = "0",
                      "1-" = "1",
                      "5-" = "5"),
         Age = as.integer(Age),
         Age = case_when(Age < 25 ~ 0,
                         Age %in% 25:44 ~ 25,
                         Age %in% 45:64 ~ 45,
                         Age %in% 65:74 ~ 65,
                         Age %in% 75:84 ~ 75,
                         Age >= 85 ~ 85),
         Deaths = as.integer(Deaths),
         Year = as.double(Year),
         Month = str_sub(Month, 6, 7) %>% as.integer()) %>% 
  group_by(State, Age, Sex, Year, Month) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db4 <- 
  db3 %>% 
  spread(Sex, Deaths) %>% 
  replace_na(list(Female = 0, Male = 0)) %>% 
  mutate(pr_f = Female / (Male + Female),
         pr_m = Male / (Male + Female)) %>% 
  select(-Female, -Male) %>% 
  complete(State, Age, Year, Month, fill = list(pr_f = NA, pr_m = NA)) %>% 
  group_by(Age, Year, Month) %>% 
  mutate(av_pr_f = mean(pr_f, na.rm = T),
         av_pr_m = mean(pr_m, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pr_f = ifelse(is.na(pr_f), av_pr_f, pr_f),
         pr_m = ifelse(is.na(pr_m), av_pr_m, pr_m)) %>% 
  select(-av_pr_f, -av_pr_m)


# ~~~~~~~~~~~~~~~~
# weekly mortality
# ~~~~~~~~~~~~~~~~

# load weekly mortality data
db_w <- read_csv("Data/Weekly_Counts_of_Deaths_by_Jurisdiction_and_Age.csv")
       
db_w2 <- 
  db_w %>% 
  select(State = 1,
         Date = 2,
         Year,
         Week,
         Age = 6,
         Deaths = 7) %>% 
  mutate(Month = month(mdy(Date)),
         Age = str_sub(Age, 1, 2),
         Age = ifelse(Age == "Un", 0, as.integer(Age)))

# test same age groups in the two datasets
unique(db_w2$Age) %>% sort()
unique(db5$Age) %>% sort()

# identify the states in the monthly mortality database
states <- unique(db4$State)

# merge with sex distribution by state, age, year, and month
# and estimate sex partition of weekly deaths
db_w3 <- 
  db_w2 %>% 
  left_join(db4) %>% 
  filter(Year < 2020,
         State %in% states) %>% 
  mutate(Female = Deaths * pr_f,
         Male = Deaths * pr_m)


db_w4 <- 
  db_w3 %>% 
  select(-c("Deaths", "pr_f", "pr_m")) %>% 
  gather(Female, Male, key = Sex, value = Deaths) %>% 
  mutate(Deaths = round(Deaths))

write_rds(db_w4, "Output/weekly_deaths_by_sex_2015_2019.rds")


