library(here)
source(here("Code/00_functions.R"))

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
  filter(Year >= 2005 & Year <= 2022)

# unique(db_p2$Age)
# unique(db_p2$Year) %>% sort()

db_m <- expand_grid(Year = 2005:2022, Month = 1:12) %>% 
  mutate(t = 1:n())

ages <- unique(pop_all$Age)
ctrs <- unique(pop_all$State)
sexs <- unique(pop_all$Sex)
inters_pop <- NULL


c <- ctrs[1]
s <- sexs[1]
a <- ages[1]
for(c in ctrs){
  pop_temp1 <- pop_all %>% 
    filter(State == c)
  for(s in sexs){
    pop_temp2 <- pop_temp1 %>% 
      filter(Sex == s)
    for(a in ages){
      
      db_w_temp <- db_w %>% 
        mutate(State = c,
               Sex = s,
               Age = a) %>% 
        left_join(pop_temp2)
      
      db_w_temp2 <- db_w_temp %>% 
        left_join(interpop(db_w_temp)) %>% 
        mutate(Country = c,
               Age = a,
               Sex = s)
      
      inters_pop <- inters_pop %>% 
        bind_rows(db_w_temp2)
      
    }
  }
}

unique(inters_pop$State)

# Visual test
#############
c <- "California"
a <- 70
s <- "m"

inters_pop %>% 
  filter(State == c,
         Age == a,
         Sex == s) %>% 
  ggplot()+
  geom_line(aes(t, Pop2), col = "black")+
  geom_point(aes(t, Pop), col = "red")

# closing age at 85
inters_pop2 <- inters_pop %>% 
  mutate(Age = ifelse(Age > 85, 85, Age)) %>% 
  group_by(State, Year, Week, t, Sex, Age) %>% 
  summarise(Pop = sum(Pop2)) %>% 
  ungroup()

# population estimates for all sex
inters_popt <- inters_pop2 %>% 
  group_by(Year, Week, t, State, Age) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Sex = "t")

inters_pop3 <- bind_rows(inters_pop2,
                         inters_popt) %>% 
  arrange(State, Sex, Age, t) %>% 
  select(-t)

write_rds(inters_pop3, here("Output", "pop_interpol_us_week_age5.rds"))
unique(inters_pop3$State) %>% sort()
