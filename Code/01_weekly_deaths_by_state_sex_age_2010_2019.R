library(here)
source(here("Code/00_functions.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mortality data by year, month, state, sex, and age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# all known deaths
d_known <- 
  read_tsv("Data/monthly_deaths_2010_2019_ages_all_known.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths_knw = Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths_knw = Deaths_knw %>% as.double())

# deaths in ages 40+
d_40plus <- 
  read_tsv("Data/monthly_deaths_2010_2019_ages_40plus.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths = Deaths %>% as.double()) %>% 
  rename(Deaths_40plus = Deaths)

# deaths in ages 0-39
d_0_39_est <- 
  d_40plus %>% 
  left_join(d_known) %>% 
  mutate(Deaths = Deaths_knw - Deaths_40plus,
         Age = 0) %>% 
  select(-Deaths_knw, -Deaths_40plus)

# deaths in ages 40-64
d_40_64 <- 
  read_tsv("Data/monthly_deaths_2010_2019_ages_40_64.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths) %>% 
  drop_na() %>% 
  mutate(Age = 40,
         Deaths = Deaths %>% as.double())


# deaths ages 65-74, 75-84, 85+
d_65_85plus <- 
  read_tsv("Data/monthly_deaths_2010_2019_ages_65_85plus.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Age = 'Ten-Year Age Groups', Deaths = Deaths) %>% 
  drop_na() %>% 
  mutate(Age = str_sub(Age, 1, 2) %>% as.double(),
         Deaths = Deaths %>% as.double())


# all deaths (known and unknown age)
d_all <- 
  read_tsv("Data/monthly_deaths_2010_2019_ages_all.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths = Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths = Deaths %>% as.double())

d_unk <- 
  d_all %>% 
  left_join(d_known) %>% 
  mutate(Deaths_unk = Deaths - Deaths_knw,
         prop_unk = Deaths_unk / Deaths)


# deaths with all age groups
db_age <- 
  bind_rows(d_0_39_est,
            d_40_64,
            d_65_85plus) %>% 
  mutate(Month = str_sub(Month, 1, 3),
         Sex = str_to_lower(Sex))

unique(db_age$Age)

# states included
cts <- 
  d_45_64 %>% 
  pull(State) %>% 
  unique()

write_rds(db_age, "Output/weekly_deaths_by_state_sex_age_2010_2019.rds")

