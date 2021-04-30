#dplyr
library(psych)
library(tidyverse)
library(lubridate)


str(dfSoilTst)
names(dfSoilTs)

set.seed(43423)
split <- createDataPartition(dfSoilTs$date, p= 0.01, list = FALSE)
soil_train <- dfSoilTs[split, ]
soil_test <- dfSoilTs[-split,]

soil_train[1:50,]
names(soil_train)
ulst <- lapply(soil_train, max)
str(ulst)





dfSoilVaSum <- dfSoilVa %>% 
  group_by(yr_mon = paste(year(as.Date(date)), month(as.Date(date)), sep = "-"), fips) %>%
  summarise(across(where(is.numeric) & !score, mean), score = max(na.omit(score)), .groups = "keep")
  #tally() %>%
  #summarise(score = mean(score), PRECTOT = mean(PRECTOT), n = n())

#view(tst)

ulst <- lapply(dfSoilTrSum, unique)
str(ulst)

SoilScores <- round(dfSoilTsSum$score, 1)
unique(SoilScores)

dfSoilTsSum <- dfSoilTs[1:50,] %>% 
  group_by(yr_mon = paste(year(as.Date(date)), month(as.Date(date)), sep = "-"), fips) %>%
  summarise(across(where(is.numeric), mean), .groups = "keep")

df %>% 
  group_by(g)
  

dfSoilTr <- dfSoilTrSum
dfSoilTs <- dfSoilTsSum
dfSoilVa <- dfSoilVaSum

