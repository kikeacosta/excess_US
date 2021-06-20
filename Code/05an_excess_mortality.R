# Description:
# Summarize EXCESS deaths since week 8, 2020, in all countries by sex and age 
library(here)
source(here("Code/00_functions.R"))

# detach(package:MASS)

# mortality baseline estimates
baseline_files <- fs::dir_ls(here("Output", "baseline_by_state"))
db_all <- vroom(baseline_files)

db <- 
  db_all %>% 
  rename(State = PopCode) %>% 
  select(State, Year, Month, Date, Sex, Age, Deaths, Baseline, lp, up, Exposure) %>% 
  filter(Year >= 2020 & Date <= "2021-04-15")

unique(db$State)

write_rds(db, here("Output", "baseline_mortality.rds"))
db <- read_rds(here("Output", "baseline_mortality.rds"))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Three different excess constructions:
# 1) all excess
# 2) positive excess
# 3) epidemic excess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db2 <- 
  db %>% 
  mutate(Epi_per = ifelse(Deaths >= up, 1, 0),
         Excess_epi = ifelse(Epi_per == 1, Deaths - Baseline, 0),
         Excess_pos = ifelse(Deaths > Baseline, Deaths - Baseline, 0),
         Excess = Deaths - Baseline)

# Excess mortality since week 8 in 2020 (March 1) 
db_sum <- 
  db2 %>% 
  filter(Date >= "2020-03-01") %>% 
  group_by(State, Sex, Date) %>% 
  summarise(Baseline = sum(Baseline),
            Excess_epi = sum(Excess_epi),
            Excess_pos = sum(Excess_pos),
            Excess = sum(Excess),
            pscore = (Baseline + Excess_epi) / Baseline) %>% 
  ungroup()

# cumulative excess deaths starting in Week 8
cum_age <- 
  db2 %>% 
  arrange(Date) %>%
  filter(Date >= "2020-03-01") %>% 
  group_by(State, Sex, Age) %>% 
  mutate(CumEpi = cumsum(Excess_epi),
         CumExc = cumsum(Excess),
         CumPos = cumsum(Excess_pos),
         Exposure = cumsum(Exposure)) %>% 
  arrange(State, Sex, Age, Date) %>% 
  select(State, Sex, Age, Date, CumEpi, CumExc, CumPos, Exposure) %>% 
  ungroup() %>% 
  mutate()

write_csv(cum_age, "Output/cumulative_us_excess_sex_age_2020_2021.csv")
# write_csv(cum, "Output/cumulative_excess_all_ages_2020_2021.csv")

