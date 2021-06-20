library(here)
source(here("Code/00_functions.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging mortality data 2010 to 2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_19 <- read_rds("Output/monthly_deaths_by_state_sex_age_2010_2019.rds")
db_20 <- read_rds("Output/monthly_deaths_by_state_sex_age_2020_2021.rds")

cts_19 <- unique(db_19$State) %>% sort()
cts_20 <- unique(db_20$State) %>% sort()

unique(db_19$Age)
unique(db_20$Age)
unique(db_20$State)

db_20_2 <- 
  db_20 %>% 
  mutate(Age = case_when(Age < 40 ~ 0,
                         Age >= 40 & Age < 65 ~ 40,
                         Age >= 65 & Age < 75 ~ 65,
                         Age >= 75 & Age < 85 ~ 75,
                         Age >= 85 ~ 85),
         State = ifelse(State == "New York City", "New York", State)) %>% 
  group_by(State, Sex, Age, Year, Month) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  filter(State %in% cts_19)

db_d <- 
  bind_rows(db_19, db_20_2) %>% 
  arrange(State, Sex, Age, Year, Month) %>% 
  mutate(Date = make_date(y = Year, m = Month, d = 15)) %>% 
  group_by(State, Sex, Age) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  select(State, Sex, Age, Year, Month, Date, t, Deaths) %>% 
  mutate(Year = Year %>% as.double())
  
unique(db_d$State)

db_d %>% 
  filter(State == "New York",
         Sex == "f", 
         Age == 85) %>% 
  ggplot()+
  geom_line(aes(Date, Deaths))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Population by state in 2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_p <- read_tsv("Data/pop by state.txt",
                 col_types = cols(.default = "c"))

pop_all <- 
  db_p %>% 
  select(`Age Group`, Sex = `Gender Code`, 
         State, Year, 
         Pop = `Projected Populations`) %>% 
  drop_na() %>% 
  mutate(Age = str_sub(`Age Group`, 1, 2),
         Age = case_when(Age == "0-" ~ "0", 
                         Age == "5-" ~ "5", 
                         TRUE ~ Age),
         Age = Age %>% as.double(),
         Sex = tolower(Sex),
         Pop = Pop %>% as.double(),
         Year = Year %>% as.double(),
         Month = 7) %>% 
  select(-'Age Group') %>% 
  filter(Year >= 2005 & Year <= 2022) %>% 
  mutate(Age = case_when(Age < 40 ~ 0,
                         Age >= 40 & Age < 65 ~ 40,
                         Age >= 65 & Age < 75 ~ 65,
                         Age >= 75 & Age < 85 ~ 75,
                         Age >= 85 ~ 85)) %>% 
  group_by(State, Sex, Age, Year, Month) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()

db_m <- expand_grid(Year = 2005:2022, Month = 1:12) %>% 
  mutate(t = 1:n())

ages <- unique(pop_all$Age)
ctrs <- unique(pop_all$State)
sexs <- unique(pop_all$Sex)

c <- ctrs[1]
s <- sexs[1]
a <- ages[1]

inters_pop <- NULL
for(c in ctrs){
  pop_temp1 <- pop_all %>% 
    filter(State == c)
  for(s in sexs){
    pop_temp2 <- pop_temp1 %>% 
      filter(Sex == s)
    for(a in ages){
      
      db_m_temp <- db_m %>% 
        mutate(State = c,
               Sex = s,
               Age = a) %>% 
        left_join(pop_temp2)
      
      inters_pop <- inters_pop %>% 
        bind_rows(db_m_temp %>% 
                    left_join(interpop(db_m_temp)))
      
    }
  }
}

unique(inters_pop$State)

# Visual test
#############
c <- "Wyoming"
a <- 40
s <- "f"

inters_pop %>% 
  filter(State == c,
         Age == a,
         Sex == s) %>% 
  ggplot()+
  geom_line(aes(t, Pop2), col = "black")+
  geom_point(aes(t, Pop), col = "red")


inters_pop2 <- 
  inters_pop %>% 
  select(-Pop, -t) %>% 
  rename(Pop = Pop2)

# population estimates for all sex
inters_popt <- 
  inters_pop2 %>% 
  group_by(State, Age, Year, Month) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Sex = "t")

inters_pop3 <- 
  bind_rows(inters_pop2,
            inters_popt) %>% 
  arrange(State, Sex, Age, Year, Month)

write_rds(inters_pop3, here("Output", "pop_interpol_us_month_ages.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging deaths and population
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db_out <- 
  db_d %>% 
  left_join(inters_pop3)

write_rds(db_out, here("Output", "master_db_monthly_deaths_population_state_sex_age_2010_2022.rds"))

