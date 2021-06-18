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

