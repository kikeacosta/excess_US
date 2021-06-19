library(here)
source(here("Code/00_functions.R"))

db_20 <- read_rds("Output/weekly_deaths_by_state_sex_age_2020_2021.rds")
db_19 <- read_rds("Output/weekly_deaths_by_state_sex_age_2010_2019.rds")

db_19_2 <- 
  db_19 %>% 
  mutate(Month = match(Month, month.abb)) 

db_19_t <- 
  db_19_2 %>% 
  group_by(State, Year, Month, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Sex = "t")
  
db_d <- 
  bind_rows(db_19_2, db_19_t, db_20) %>% 
  arrange(State, Year, Month, Age, Sex)


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
