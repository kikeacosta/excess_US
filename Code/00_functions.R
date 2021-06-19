library(lubridate)
library(tidyverse)

# Weekly mortality interpolation 
interpop <- function(db){
  ys <- db %>% drop_na() %>% pull(t)
  ps <- db %>% drop_na() %>% pull(Pop)
  # smoothing using cubic splines
  ws <- db %>% pull(t)
  md2 <- smooth.spline(x = ys, y = ps)
  inter_pop <- tibble(t = ws,
                      Pop2 = predict(md2, ws)$y)
  return(inter_pop)
}

interprop <- function(chunk){
  xs <- chunk %>% drop_na() %>% pull(t)
  ys <- chunk %>% drop_na() %>% pull(prop) 
  ws <- chunk %>% pull(t)
  # smoothing using cubic splines
  md <- smooth.spline(x = xs, y = ys)
  chunk %>%  
    mutate(prop2 = predict(md, ws)$y)
}

chunk <- 
  db_20m5 %>% 
  filter(Year == 2020,
         Month == 6,
         State == "Vermont",
         Sex == "t")


imput_props <- function(chunk){
  st <- chunk %>% dplyr::pull(State) %>% unique()
  sx <- chunk %>% dplyr::pull(Sex) %>% unique()
  # yr <- chunk %>% dplyr::pull(Year) %>% unique()
  # mt <- chunk %>% dplyr::pull(Month) %>% unique()
  ages <- chunk %>% dplyr::filter(is.na(Deaths)) %>% dplyr::pull(Age) 
  
  av_prop <- 
    db_20m5 %>% 
    dplyr::filter(State == st,
           Sex == sx,
           Age %in% ages,
           age_grs == 6) %>% 
    group_by(State, Sex, Age) %>% 
    summarise(Deaths = sum(Deaths)) %>% 
    ungroup() %>% 
    group_by(State, Sex) %>% 
    mutate(prop_miss = Deaths / sum(Deaths)) %>% 
    ungroup() %>% 
    select(-Deaths)
      
  chunk %>% 
    left_join(av_prop)

}
