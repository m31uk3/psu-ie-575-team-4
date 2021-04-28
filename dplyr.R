#dplyr
library(psych)
library(tidyverse)
library(lubridate)


str(dfSoilTst)


tst <- dfSoilTst %>% 
  group_by(yr_mon = paste(year(as.Date(date)), month(as.Date(date)), sep = "-"), fips) %>%
  summarise(across(where(is.numeric), mean))
  #tally() %>%
  #summarise(score = mean(score), PRECTOT = mean(PRECTOT), n = n())

view(tst)


df %>% 
  group_by(g)
  


