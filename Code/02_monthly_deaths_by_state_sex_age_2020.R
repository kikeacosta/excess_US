library(here)
source(here("Code/00_functions.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weekly mortality data by age, sex, and state since 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# loading data 
db_20 <- read_csv("Data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv",
                  col_types = cols(.default = "c"))

unique(db_20$State)

# Age groups
unique(db_20$`Age Group`)
ages <- c("All Ages", "Under 1 year", 
          "1-4 years", "5-14 years", "15-24 years",
          "25-34 years", "35-44 years", "45-54 years", 
          "55-64 years", "65-74 years", 
          "75-84 years", "85 years and over")

ages <- c("All Ages", "0-17 years", 
          "18-29 years", "30-39 years", "40-49 years",
          "50-64 years", "65-74 years", 
          "75-84 years", "85 years and over")

# monthly deaths by state
db_20m <- 
  db_20 %>% 
  filter(!is.na(Month)) %>% 
  select(Year, Month, State, Sex, Age = `Age Group`, Deaths = `Total Deaths`) %>% 
  mutate(Sex = recode(Sex,
                      "Female" = "f",
                      "Male" = "m",
                      "All Sexes" = "t")) %>% 
  filter(Age %in% ages) %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = recode(Age,
                      "Al" = "TOT",
                      "0-" = "0"),
         Deaths = Deaths %>% as.double(),
         Month = Month %>% as.double())

unique(db_20m$Age)

db_20_all_age <- 
  db_20m %>% 
  filter(Age == "TOT") %>% 
  rename(Deaths_all = Deaths) %>% 
  select(-Age)

db_20m2 <- 
  db_20m %>%
  filter(Age != "TOT") %>% 
  mutate(Age = Age %>% as.double())

imput_one_age <- 
  db_20m2 %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(age_grs = sum(!is.na(Deaths)),
         Deaths_knw = sum(Deaths, na.rm = T)) %>% 
  ungroup() %>% 
  filter(age_grs == 7) %>% 
  left_join(db_20_all_age) %>% 
  mutate(Deaths = ifelse(is.na(Deaths), Deaths_all - Deaths_knw, Deaths)) %>% 
  select(-Deaths_all, -Deaths_knw)


db_20m3 <- 
  db_20m2 %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(age_grs = sum(!is.na(Deaths))) %>% 
  ungroup() %>% 
  filter(age_grs != 7) %>% 
  bind_rows(imput_one_age) %>% 
  arrange(State, Year, Month, Sex, Age) %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(age_grs = sum(!is.na(Deaths))) %>% 
  ungroup()

# imputation based on total sex
imput2 <- 
  db_20m3  %>% 
  group_by(Year, Month, State) %>% 
  mutate(prop = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  filter(is.na(prop)) %>% 
  select(-prop, -age_grs) %>% 
  spread(Sex, Deaths) %>% 
  mutate(m = ifelse(is.na(m), t - f, m),
         f = ifelse(is.na(f), t - m, f),
         t = ifelse(is.na(t), f + m, t)) %>% 
  gather(t, m, f, key = Sex, value = Deaths)

db_20m4 <- 
  db_20m3  %>% 
  group_by(Year, Month, State) %>% 
  mutate(prop = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  filter(!is.na(prop)) %>% 
  select(-prop, -age_grs) %>% 
  bind_rows(imput2) %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(age_grs = sum(!is.na(Deaths))) %>% 
  ungroup()


# looking at the death share among missing ages in all other periods,
# within the same State and Sex

still_miss <- 
  db_20m4 %>% 
  filter(age_grs < 8) %>% 
  group_by(State, Year, Month, Sex) %>% 
  mutate(id = group_indices())

# i <- 1
imputs <- tibble()
for(i in 1:max(still_miss$id)){
  chunk <- 
    still_miss %>% 
    filter(id == i)
  
  st <- chunk %>% dplyr::pull(State) %>% unique()
  sx <- chunk %>% dplyr::pull(Sex) %>% unique()
  # yr <- chunk %>% dplyr::pull(Year) %>% unique()
  # mt <- chunk %>% dplyr::pull(Month) %>% unique()
  ages <- chunk %>% dplyr::filter(is.na(Deaths)) %>% dplyr::pull(Age) 
  
  av_prop <- 
    db_20m4 %>% 
    dplyr::filter(State == st,
                  Sex == sx,
                  Age %in% ages,
                  age_grs == 8) %>% 
    group_by(State, Sex, Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    group_by(State, Sex) %>% 
    mutate(prop_miss = Deaths / sum(Deaths)) %>% 
    ungroup() %>% 
    select(-Deaths)
  
  temp <- 
    chunk %>% 
    left_join(av_prop) %>% 
    select(Year, Month, State, Sex, Age, prop_miss)
  
  imputs <- 
    imputs %>% 
    bind_rows(temp)
  
}

db_20m5 <- 
  db_20m4 %>% 
  left_join(imputs) %>% 
  left_join(db_20_all_age) %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(Deaths_knw = sum(Deaths, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Deaths_unk = Deaths_all - Deaths_knw,
         Deaths2 = ifelse(is.na(Deaths), 
                          round(Deaths_unk * prop_miss),
                          Deaths)) %>% 
  drop_na(Deaths_all)

db_20m6 <- 
  db_20m5 %>% 
  select(Year, Month, State, Sex, Age, Deaths2) %>% 
  rename(Deaths = Deaths2)


# ~~~~~~~~~~~~~~~~
# consistency test
# ~~~~~~~~~~~~~~~~
t1 <- 
  db_20m2 %>% 
  left_join(db_20m6 %>% 
              rename(Deaths_est = Deaths)) %>% 
  mutate(Imp = ifelse(is.na(Deaths), 1, 0),
         Diff = Deaths - Deaths_est) %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(all_D_est = sum(Deaths_est)) %>% 
  ungroup() %>% 
  left_join(db_20_all_age) %>% 
  mutate(Eq_tots = ifelse(all_D_est == Deaths_all, 1, 0),
         Dif_tots = all_D_est - Deaths_all)

# saving output
write_rds(db_20m6, "Output/monthly_deaths_by_state_sex_age_2020_2021.rds")

