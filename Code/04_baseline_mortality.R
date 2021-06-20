library(here)
source(here("Code/00_functions.R"))

# loading data
# ~~~~~~~~~~~~
db_dp <- read_rds(here("Output", "master_db_monthly_deaths_population_state_sex_age_2010_2022.rds"))

unique(db_p$State) %>% sort()

# definition of flu seasons and heat waves
# For northern countries
flu_season <- c(seq(1, 3, 1), seq(10, 12, 1))
flu_season <- c(12, 1, 2)
# Initial year for baseline estimation
ym <- 2010

# Formating data for baseline estimation
db_de <- db_dp %>% 
  # estimating exposures in person-weeks and rounding deaths to min 1
  mutate(Exposure = Pop / 12,
         Deaths = round(Deaths, 0) + 1) %>% 
  select(State, Sex, Age, Year, Month, t, Deaths, Exposure) %>% 
  # adding sinusoidal terms for seasonality
  mutate(sn12 = sin((2*pi*t)/(12)),
         cs12 = cos((2*pi*t)/(12)),
         # excluding winter and summer weeks, as well as 2009 and COVID-19 pandemics
         include = case_when(Age == 0 ~ 1,
                             Age > 0 & !(Month %in% flu_season) & Year < 2020 ~ 1, 
                             TRUE ~ 0),
         include = factor(include)) %>% 
  drop_na()

gc()

unique(db_de$State)

# visual inspection of weeks to include and exclude
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_de %>% 
  filter(State == "New York",
         Age == 75,
         Sex == "t") %>% 
  ggplot()+
  geom_point(aes(t, Deaths, col = include))

db_de %>% 
  filter(State == "California",
         Age == 85,
         Sex == "f") %>% 
  ggplot()+
  geom_point(aes(t, Deaths, col = include))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimating baseline for each state, sex, and age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# loading packages for baseline estimation
p_load(pkgs_bsl, character.only = TRUE)
select <- dplyr::select
# setting cores use for parallel processing
registerDoParallel(cores = 6)

# creating directories to locally store partial results that do not sync with github
if (!dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}

if (!dir.exists(here("Figures","baseline_by_state"))){
  dir.create(here("Figures","baseline_by_state"))
}

if (!dir.exists(here("Output","baseline_by_state"))){
  dir.create(here("Output","baseline_by_state"))
}

# starting year of observation
ym <- 2010
cts <- unique(db_de$State) %>% sort()
sxs <- unique(db_de$Sex)
ags <- unique(db_de$Age)
# cts <- c("CHL", "CZE", "DEUTNP", "USA", "KOR")
c <- "New York"
a <- 65
s <- "t"
db_blns_all <- tibble()
for (c in cts) {
  db_blns <- tibble()
  temp <- 
    db_de %>% 
    filter(State == c)
  sxs <- unique(temp$Sex)
  
  for (s in sxs) {
    temp2 <- 
      temp %>% 
      filter(Sex == s)
    ags <- unique(temp2$Age)
    
    for (a in ags) {
      
      temp3 <- 
        temp2 %>% 
        filter(Age == a) %>% 
        select(Year, Month, t, Deaths, Exposure, sn12, cs12, include)
      
      cat(paste(c, s, a, "\n", sep = "_"))
      
      temp4 <- fit_baseline(temp3) %>% 
        mutate(PopCode = c,
               Sex = s,
               Age = a,
               Date = make_date(y = Year, m = Month, d = 15),
               mx_b = 100000 * Baseline / Exposure,
               mx_b_u = 100000 * up / Exposure,
               mx_b_l = 100000 * lp / Exposure,
               mx_d = 100000 * Deaths / Exposure) 
      
      db_blns <- db_blns %>% 
        bind_rows(temp4)
      
      ## plots of estimates
      ## ~~~~~~~~~~~~~~~~~~
      plot_name <- paste0(c, "_", s, "_", a, ".png")
      temp4 %>%
        ggplot()+
        # geom_vline(xintercept = ymd("2020-04-03"), col = "#b80c09", alpha = 0.1, size = 5)+
        geom_line(aes(Date, mx_d), size = 0.4)+
        geom_ribbon(aes(Date, ymin = mx_b_l, ymax = mx_b_u), fill = "#01BAEF", alpha = 0.25)+
        geom_line(aes(Date, mx_b), col = "#01BAEF", alpha = 0.9, size = 0.6)+
        scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
        labs(title=paste0(c, "_", s, "_", a))+
        theme_bw()+
        theme(
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=11),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10))+
        ggsave(here("Figures", "baseline_by_state", plot_name), dpi = 300, width = 6, height = 4)
    }
  }
  db_blns <- db_blns %>% 
    mutate(PopCode = c)
  write_csv(db_blns, path = here("Output", "baseline_by_state", paste0(c, "_baseline.csv")))
  db_blns_all <- bind_rows(db_blns_all, db_blns)
}

write_rds(db_blns, here("Output", "baseline_mortality.rds"))

detach(package:MASS)


