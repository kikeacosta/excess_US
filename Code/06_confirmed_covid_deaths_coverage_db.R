
db_usa <- read_rds("Data/USA_deaths_states_coverage_db.rds")

db_usa2 <- 
  db_usa %>% 
  mutate(Date = dmy(Date)) %>% 
  select(-Code, -AgeInt, -Measure, -Metric) %>% 
  rename(State = Region,
         Deaths = Value)

write_csv(db_usa2, "output/us_weekly_covid_deaths_by_state.csv")

Northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
               "Rhode Island", "Vermont", "New Jersey", "New York", 
               "New York City", "Pennsylvania")
Midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", 
             "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", 
             "South Dakota")
South <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", 
           "South Carolina", "Virginia", "District of Columbia", "West Virginia", 
           "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", 
           "Louisiana", "Oklahoma", "Texas")
West <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", 
          "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", 
          "Washington")

db_usa3 <- 
  db_usa2 %>% 
  mutate(Region = case_when(State %in% Northeast ~ "Northeast",
                            State %in% Midwest ~ "Midwest",
                            State %in% South ~ "South",
                            State %in% West ~ "West",
                            TRUE ~ "Other")) %>% 
  filter(Region != "Other") %>% 
  group_by(Region, Date, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup()

write_csv(db_usa3, "output/us_weekly_covid_deaths_by_region.csv")
