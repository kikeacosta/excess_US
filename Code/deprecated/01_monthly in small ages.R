library(here)
source(here("Code/00_functions.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total Population by state in 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_p <- read_tsv("Data/pop by state.txt",
                 col_types = cols(.default = "c"))

unique(db_p$Gender)
unique(db_p$State)

pop_state_2020 <- 
  db_p %>% 
  rename(Pop = `Projected Populations`) %>% 
  mutate(Pop = Pop %>% as.double()) %>% 
  filter(Year == "2020") %>% 
  group_by(State) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()


pop_state_2020 %>% 
  group_by() %>% 
  summarise(Pop = sum(Pop))

# selecting states with more than 4 million persons in 2020
sts_over_mill <- 
  pop_state_2020 %>% 
  filter(Pop > 4000000) %>% 
  pull(State)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mortality data by year, month, state, sex, and age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# all known deaths
d_known <- 
  read_tsv("Data/Underlying Cause of Death, 2015-2019_all_known.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths_knw = Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths_knw = Deaths_knw %>% as.double())

# deaths in ages 25+
d_25plus <- 
  read_tsv("Data/Underlying Cause of Death, 2015-2019_ages_25plus.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths_25plus = Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths_25plus = Deaths_25plus %>% as.double())

# deaths in ages 0-24
d_0_24_est <- 
  d_25plus %>% 
  left_join(d_known) %>% 
  mutate(Deaths = Deaths_knw - Deaths_25plus,
         Age = 0) %>% 
  select(-Deaths_knw, -Deaths_25plus)

# # test comparting imputation with original
# d_0_24 <-
#   read_tsv("Data/Underlying Cause of Death, 2015-2019_ages_0_24.txt",
#            col_types = cols(.default = "c")) %>%
#   select(State, Year, Month, Sex = 'Gender Code', Deaths) %>%
#   drop_na()
# 
# test_0_24 <-
#   d_0_24 %>%
#   left_join(d_0_24_est)

# deaths in ages 45+
d_45plus <- 
  read_tsv("Data/Underlying Cause of Death, 2015-2019_ages_45plus.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths_45plus = Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths_45plus = Deaths_45plus %>% as.double())

# deaths in ages 25-44
d_25_44_est <- 
  d_known %>% 
  left_join(d_0_24_est %>% 
              rename(Deaths_0_24 = Deaths) %>% 
              select(-Age)) %>% 
  left_join(d_45plus) %>% 
  mutate(Deaths = Deaths_knw - Deaths_0_24 - Deaths_45plus,
         Age = 25) %>% 
  select(-Deaths_knw, -Deaths_0_24, -Deaths_45plus)

# deaths ages 45-64
d_45_64 <- 
  read_tsv("Data/Underlying Cause of Death, 2015-2019_ages_45_64.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths) %>% 
  drop_na() %>% 
  mutate(Age = 45,
         Deaths = Deaths %>% as.double())
  
# deaths ages 65-74, 75-84, 85+
d_65_85plus <- 
  read_tsv("Data/Underlying Cause of Death, 2015-2019_ages_65_85plus.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Age = 'Ten-Year Age Groups', Deaths = Deaths) %>% 
  drop_na() %>% 
  mutate(Age = str_sub(Age, 1, 2) %>% as.double(),
         Deaths = Deaths %>% as.double())


# all deaths (known and unknown age)
d_all <- 
  read_tsv("Data/Underlying Cause of Death, 2015-2019_all.txt", 
           col_types = cols(.default = "c")) %>% 
  select(State, Year, Month, Sex = 'Gender Code', Deaths = Deaths) %>% 
  drop_na() %>% 
  mutate(Deaths = Deaths %>% as.double())

d_unk <- 
  d_all %>% 
  left_join(d_known) %>% 
  mutate(Deaths_unk = Deaths - Deaths_knw)


# deaths with all age groups
db_age <- 
  bind_rows(d_0_24_est,
            d_25_44_est,
            d_45_64,
            d_65_85plus) %>% 
  mutate(Month = str_sub(Month, 1, 3),
         Sex = str_to_lower(Sex))

# states included
cts <- 
  d_45_64 %>% 
  pull(State) %>% 
  unique()



# imput <- 
#   db_age %>% 
#   filter(Deaths < 10) %>% 
#   summarise(Deaths = mean(Deaths))
  




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Weekly mortality data by age, sex, and state since 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# loading data 
db_20 <- 
  read_csv("Data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv",
           col_types = cols(.default = "c"))

unique(db_20$`Age Group`)

# states to select
cts <- unique(db_w4$State)

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
  filter(!is.na(Month),
         State %in% cts) %>% 
  select(Year, Month, State, Sex, Age = `Age Group`, Deaths = `Total Deaths`) %>% 
  mutate(Sex = recode(Sex,
               "Female" = "f",
               "Male" = "m",
               "All Sexes" = "t")) %>% 
  filter(Age %in% ages) %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = recode(Age,
                      "Al" = "TOT",
                      "Un" = "0",
                      "1-" = "1",
                      "5-"  = "5"),
         Deaths = Deaths %>% as.double())

unique(db_20m$Age)

db_20_all_age <- 
  db_20m %>% 
  filter(Age == "TOT")

db_20m2 <- 
  db_20m %>%
  filter(Age != "TOT") %>% 
  mutate(Age = Age %>% as.double(),
         Age = case_when(Age < 25 ~ 0,
                         Age >= 25 & Age < 45 ~ 25,
                         Age >= 45 & Age < 65 ~ 45,
                         Age >= 65 & Age < 75 ~ 65,
                         Age >= 75 & Age < 85 ~ 75,
                         Age >= 85 ~ 85)) %>% 
  group_by(Year, Month, State, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

db_20m3 <- 
  db_20m2 %>% 
  filter(State %in% sts_over_mill)


chunk <- 
  db_20m2 %>% 
  filter(State == "Alaska",
         Month == "1",
         Year == "2020")

chunk <- 
  db_20m2 %>% 
  filter(State == "Alaska",
         !is.na(Deaths)) %>% 
  group_by(Year, Month, State, Sex) %>% 
  mutate(n = n())



db_20m2 %>% 
  group_by(Year, Month, State, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths))









