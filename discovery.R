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
# dfGenre <- read.csv("/Users/lujackso/Downloads/Kaggle/spotify_century_data/data_w_genres.csv", header = TRUE, stringsAsFactors = F)

#dfSoil <- read.csv("D:/Users/lujackso/Downloads/soil/train_timeseries/train_timeseries.csv", 
dfSoilTr <- read.csv("/Users/lujackso/Downloads/DAAN-570-302-scratch/psu-ie-575-team-4/data/train_soil_by_month.csv", 
                    header = TRUE, 
                    stringsAsFactors = F,
                    comment.char="")

dfSoilTs <- read.csv("/Users/lujackso/Downloads/DAAN-570-302-scratch/psu-ie-575-team-4/data/test_soil_by_month.csv", 
                   header = TRUE, 
                   stringsAsFactors = F,
                   comment.char="")

dfSoilVa <- read.csv("/Users/lujackso/Downloads/DAAN-570-302-scratch/psu-ie-575-team-4/data/validation_soil_by_month.csv", 
                   header = TRUE, 
                   stringsAsFactors = F,
                   comment.char="")

#dfHeart <- read_excel("/Users/lujackso/Downloads/Kaggle/spotify_century_data/data.csv")

df_wine <- df_wine[df_wine$winetype=="red", ] # Red only

length(dfSoil[is.na(dfSoil$score),])
dfSoilClean <- na.omit(dfSoil) # Show rows with missing data
rm(dfSoil)

dfTest <- bk[1:50000,]

dfTest$week_num <- week(dfTest$date)
dfTest$year <- year(dfTest$date)
str(dfTest)
unique(dfTest$year)

names(bk)

vignette("colwise")

dfSoilSumVal <- dfSoilClean %>% 
        group_by(yr_mon = paste(year(as.Date(date)), month(as.Date(date)), sep = "-"), fips) %>%
        summarise(across(where(is.numeric), mean), .groups = "keep")
          #tally() %>%
          #summarise(score = mean(score), PRECTOT = mean(PRECTOT), n = n())

  #group_by(qtr = paste(quarters(as.Date(date)), year(as.Date(date)), sep = "-")) %>%
  #select(-date) %>%
  #summarise_each(funs(count))
  #group_by(year, week_num) %>% 
  #select(-date) %>%
  #mutate(score_mean = mean(score)) %>%
  #head(30)
  #summarise(count = count(score))


ulst <- lapply(round(dfSoilSum[-c(1)]), unique)
str(ulst)
view(head(dfSoilSumVal))
head(dfSoilSumVal)
names(dfSoilSum2)
write.csv(dfSoilSum2, "D:/Users/lujackso/Downloads/soil/train_timeseries/train_soil_by_month.csv", row.names = FALSE)
write.csv(dfSoilSumTst, "D:/Users/lujackso/Downloads/soil/test_timeseries/test_soil_by_month.csv", row.names = FALSE)
write.csv(dfSoilSumVal, "D:/Users/lujackso/Downloads/soil/validation_timeseries/validation_soil_by_month.csv", row.names = FALSE)




## free up space
rm(dfSoil)
rm(list = setdiff(ls(), ls(pattern="bk*")))

## sub set ###
str(head(dfSoilClean))
dfSoilNot0 <- dfSoilClean[dfSoilClean$score!=0, ] # Non Zero only
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
pp_df_pim <- preProcess(dfSoilClean[, -c(2)], method = c("BoxCox", "center", "scale")) # Transform values
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

# round score in order to make manageable predictions
# $ score      : num [1:63812] 1 2 1.49 1.5 1.28 ... # before round
#  $ t.score      : num [1:6] 1 2 3 4 5 0 # after round
dTSoil$t.score = round(dTSoil$t.score) 


ulst <- lapply(round(dTSoil[-c(2)]), unique)
ulst <- lapply(dTSoil[-c(2)], unique)
str(ulst)
describe(head(dfSoilClean))
str(dfSoilTr)


# Plot Summary Analysis
#gg_data <- df_pim
gg_note <- " [Excl. Score = 0]"
describe(head(dTSoil[-c(2,9,10,11,14,15,16,18,19,20)]))
gg_data <- dTSoil[-c(2,9,10,11,14,15,16,18,19,20)]
gg_data <- head(gg_data, 57911)
gg_data <- dfSoilClean[-c(2)]
gg_data <- dfSoilTr[-c(1,21)]
# plot histogram of each feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(gg_data)) {
  hist(gg_data[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(gg_data[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Features (", length(names(gg_data)), ")", gg_note, sep = ""), outer=TRUE,  cex=1.2)


str(dTSoil)
# ggplot2 code 
ggplot(dfSoilClean, aes(score)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~score,scales="free")

# plot boxplots of each feature for output values
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(gg_data[-c(1)])) {
  boxplot(gg_data[[i]] ~ round(gg_data$t.fips), col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(gg_data[i]), cex=0.8, side=1, line=2)
}
mtext(paste("Boxplots of Features for Output Values (", length(names(gg_data[-c(1)])), ")", gg_note, sep = ""), outer=TRUE,  cex=1.2)

# test one boxplot
boxplot(gg_data$T2M_RANGE ~ gg_data$score, col="wheat2", ylab = "", xlab = "", main = "")


#TODO: check memory usage of large vars
dfSoilClean

# Evaluate features for correlation
dC_pim <- dT_pim
dC_pim$t.diabetes <- as.numeric(dT_pim$t.diabetes) # covert to numeric output factor

cor_input <- dTSoil[-c(2)] # no filter
cor_input <- dfSoilClean[-c(2)] # no filter on non stnd data
cor_input <- dTSoil[-c(2,7,6,4,9,5,13,8,12,17,14)] # non zero scores cor filter
cor_input <- dTSoil[-c(2,7,6,9,5,13,11,17,12,14)] # full data set cor filter

pim_cor <- cor(cor_input) 

names(cor_input)
pim_len <- length(names(cor_input))
print(pim_len)

# Plot correlation matrix
#par(mfrow=c(1,1), oma = c(1,1,1,1) + 0.1,  mar = c(1,1,1,1) + 0.1)
corrplot(pim_cor, type = "lower", main = paste("Correlation of Features (", pim_len, ")", sep = ""), mar=c(0,0,3,0), oma = c(1,1,1,1))
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