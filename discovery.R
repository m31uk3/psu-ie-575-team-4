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

dfSoil <- read.csv("/Users/lujackso/Downloads/Kaggle/weather_soil_data/train_timeseries/train_timeseries.csv", 
                    header = TRUE, 
                    stringsAsFactors = F,
                    comment.char="")

#dfHeart <- read_excel("/Users/lujackso/Downloads/Kaggle/spotify_century_data/data.csv")

df_wine <- df_wine[df_wine$winetype=="red", ] # Red only

length(dfSoil[is.na(dfSoil$score),])
dfSoilClean <- na.omit(dfSoil) # Show rows with missing data
rm(dfSoil)

## free up space
rm(dfSoil)

## sub set ###
str(head(dfSoilClean))
dfSoilNot0 <- dfSoilClean[dfSoilClean$score!=0, ] # Red only
bk <- dfSoilClean
dfSoilClean <- dfSoilNot0



describe(dfSoilClean)

# Skewness, transformation, and removing outliers
# skewness < -1 or > 1; distribution is highly skewed
# skewness between -0.5 and 0.5; distribution is approximately symmetric
describe(head(dfSoilClean, 100000))
describe(dfSoilClean)[-c(2,21)]
#describe(dfSoilClean)[order(describe(dfSoilClean[-c(2,21)])$skew, decreasing = TRUE),c("vars","skew")]
# dT_pim <- df_pim # Skip transforming/scaling 
pp_df_pim <- preProcess(dfSoilClean[, -c(2,21)], method = c("BoxCox", "center", "scale")) # Transform values
dTSoil <- data.frame(t = predict(pp_df_pim, dfSoilClean))
describe(dT_pim)[order(describe(dT_pim)$skew, decreasing = TRUE),c("vars","skew")]

# Remove Outliers greater than 3 standard deviations (assumes scaled data; mean of zero)
describe(head(dTSoil[-c(2,21)]))
for (i in names(dTSoil[-c(2,21)])) {
  dTSoil <- dTSoil[!abs(dTSoil[[i]]) > 3 ,]
}
describe(dTSoil[-c(2,21)])


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
ulst <- lapply(round(dTSoil[-c(2)]), unique)
ulst <- lapply(round(dfSoilClean[-c(2)]), unique)
str(ulst)
describe(head(dfSoilClean))
str(dfSoil)


# Plot Summary Analysis
#gg_data <- df_pim
describe(head(dTSoil[-c(2,9,10,11,14,15,16,18,19,20)]))
gg_data <- dTSoil[-c(2,9,10,11,14,15,16,18,19,20)]
gg_data <- head(gg_data, 57911)
gg_data <- dfSoilClean[-c(2)]
gg_data <- dTSoil[-c(2,21)]
# plot histogram of each feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(gg_data)) {
  hist(gg_data[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(gg_data[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Features (", length(names(gg_data)), ")", sep = ""), outer=TRUE,  cex=1.2)

# ggplot2 code 
ggplot(dfSoilClean, aes(score)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~score,scales="free")

# plot boxplots of each feature for output values
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(gg_data[-c(11)])) {
  boxplot(gg_data[[i]] ~ gg_data$score, col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(gg_data[i]), cex=0.8, side=1, line=2)
}
mtext(paste("Boxplots of Features for Output Values (", length(names(gg_data)), ")", sep = ""), outer=TRUE,  cex=1.2)

# test one boxplot
boxplot(gg_data$T2M_RANGE ~ gg_data$score, col="wheat2", ylab = "", xlab = "", main = "")


#TODO: check memory usage of large vars
dfSoilClean

# Evaluate features for correlation
dC_pim <- dT_pim
dC_pim$t.diabetes <- as.numeric(dT_pim$t.diabetes) # covert to numeric output factor
str(dfSoilClean[-c(2)])
pim_cor <- cor(dfSoilClean[-c(2,7,6,9,5,13,11,17,12,14)])
pim_cor <- cor(dfSoilClean[-c(2)])
# Plot correlation matrix
#par(mfrow=c(1,1), oma = c(1,1,1,1) + 0.1,  mar = c(1,1,1,1) + 0.1)
corrplot(pim_cor, type = "lower", main = paste("Correlation of Features (", 19, ")", sep = ""), mar=c(0,0,3,0), oma = c(1,1,1,1))
# Search for problematic features
findCorrelation(pim_cor, cutoff = 0.90)
findCorrelation(pim_cor, cutoff = 0.70)


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