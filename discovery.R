library(C50)
library(Tmisc)
library(MASS)
library(psych)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(mlbench)
library(visdat)
library(caret)
#library(broom)
library(ROCR)
library(readxl)
library(scales)
library(rpart)
library(rpart.plot)
library(tree)
library(e1071)

### Extract, Transform and Load (ETL) Data source ###
dfGenre <- read.csv("/Users/lujackso/Downloads/Kaggle/spotify_century_data/data_w_genres.csv", header = TRUE, stringsAsFactors = F)

dfSoil <- read.csv("/Users/lujackso/Downloads/Kaggle/weather_soil_data/train_timeseries/train_timeseries.csv", header = TRUE, stringsAsFactors = F)

#dfHeart <- read_excel("/Users/lujackso/Downloads/Kaggle/spotify_century_data/data.csv")

df_wine <- df_wine[df_wine$winetype=="red", ] # Red only

length(dfSoil[is.na(dfSoil$score),])
dfSoilClean <- na.omit(dfSoil) # Show rows with missing data

describe(dfSoilClean)

# Skewness, transformation, and removing outliers
# skewness < -1 or > 1; distribution is highly skewed
# skewness between -0.5 and 0.5; distribution is approximately symmetric
describe(head(dfSoilClean, 100000))
describe(dfSoilClean)[-c(2,21)]
#describe(dfSoilClean)[order(describe(dfSoilClean[-c(2,21)])$skew, decreasing = TRUE),c("vars","skew")]
dT_pim <- df_pim # Skip transforming/scaling 
pp_df_pim <- preProcess(df_pim[, -c(9)], method = c("BoxCox", "center", "scale")) # Transform values
dT_pim <- data.frame(t = predict(pp_df_pim, df_pim))
describe(dT_pim)[order(describe(dT_pim)$skew, decreasing = TRUE),c("vars","skew")]

# Remove Outliers greater than 3 standard deviations (assumes scaled data; mean of zero)
describe(dT_pim[-c(9)])
for (i in names(dT_pim[-c(9)])) {
  dT_pim <- dT_pim[!abs(dT_pim[[i]]) > 3 ,]
}
describe(dT_pim[-c(9)])


dfSoilSub <- dfSoil[format.Date(dfSoil$date, "%Y")=="2009" &
                    !is.na(dfSoil$date),]

dfSoilSub <- dfSoil[format.Date(dfSoil$date, "%Y")=="2009" &
                      !is.na(dfSoil$date),]

dfSoilSub <- dfSoil[format.Date(dfSoil$date, "%m") >="5" &
                      format.Date(dfSoil$date, "%m") <="8" &
                      format.Date(dfSoil$date, "%Y")=="2019" &
                      !is.na(dfSoil$date),]

#dfSoilTidy <- dfSoil %>%
#  filter(date >= as.Date('2019-05-01') & DATE <= as.Date('2013-10-15'))

hat <- head(dfSoil, 400)
describe(hat)
blah <- hat %>% group_by(week = week(date), year = year(date)) %>% summarise(score = max(score), .groups = 'keep')


dfSoil<- dfSoil[format.Date(dfSoil$date, "%Y")=="2019"]
dfT <- dfSoilSub %>% group_by(year = year(date), week = week(date)) %>% summarise_if(is.numeric, list(mean = mean))
dfT <- dfSoilSub %>% group_by(week = week(date), year = year(date)) %>% summarise(score = mean(score), .groups = 'keep')
describe(dfT)
table(dfT)
view(dfT)

unique(paste(week(dfSoil$date), "-", year(dfSoil$date)))

describe(dfSoilSub)
dfSoilSub$date <- as.Date(dfSoilSub$date)
dfSoilSub$YearWeek <- as.yearmon(dfSoilSub$date)
str(dfSoilSub)
unique(format.Date(dfSoil$date, "%m"))
str(dfSoil)
ulst <- lapply(round(dfSoilClean[-c(2)]), unique)
str(ulst)
describe(head(dfSoilClean))
str(dfSoil)


# Plot Summary Analysis
#gg_data <- df_pim
gg_data <- dfSoilClean[-c(2,21)]
# plot histogram of each feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(gg_data)) {
  hist(gg_data[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(gg_data[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Features (", length(names(gg_data)), ")", sep = ""), outer=TRUE,  cex=1.2)

# plot boxplots of each feature for output values
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(gg_data)) {
  boxplot(gg_data[[i]] ~ gg_data$score, col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(gg_data[i]), cex=0.8, side=1, line=2)
}
mtext(paste("Boxplots of Features for Output Values (", length(names(gg_data)), ")", sep = ""), outer=TRUE,  cex=1.2)

#TODO: check memory usage of large vars
dfSoilClean

str(head(dfSoil))
names(dfSoil)
# Feature transformations
# Missing Data, and Duplicates
vis_miss(dfSoil)
sapply(dfSoil, function(x) sum(is.na(x))) # check for NAs
which(is.na(df_pim)) # Show rows with missing data
df_pim <- na.omit(df_pim) # remove records with missing values
#PimaIndiansDiabetes2 <- PimaIndiansDiabetes2[is.na(PimaIndiansDiabetes2)] # remove records with missing values
which(duplicated(dfSoil)) # Show rows with duplicate data
df_pim <- df_pim[!duplicated(df_pim), ] # remove duplicate records
describe(df_pim)