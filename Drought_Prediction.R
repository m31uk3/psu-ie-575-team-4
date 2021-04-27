library(C50)
library(MASS)
library(psych)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(mlbench)
library(caret)
library(ROCR)
library(scales)
library(rpart)
library(e1071)


soil_train <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/train_timeseries/train_pp.csv')

soil_test <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/test_timeseries/test_pp.csv')

soil_validation <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/validation_timeseries/validation_pp.csv')

soil_original <- soil_train

unique(soil_train$date)
unique(soil_test$date)
unique(soil_validation$date)

#DATASET'S TOO LARGE TO PROCESS ANYTHING SO WE REDUCE THE OBSERVATIONS AND TAKE 5 YEARS OF DATA (2011-2016)

soil_subset <- subset(x = soil_train[soil_train$date >= "2011-01-04",])

dim(soil_subset)

head(soil_train)

head(soil_test)


soil_subset[,'score'] <- round(soil_subset$score)
unique(soil_subset$score)

soil_test$score <- round(soil_test$score)
unique(soil_test$score)

soil_validation$score <- round(soil_validation$score)
unique(soil_validation$score)

data.frame(names(soil_subset))

soil_train<- na.omit(soil_train)

soil_test <- na.omit(soil_test)

soil_validation <- na.omit(soil_validation)

write.csv(soil_train,"C:/Users/Nidhi/OneDrive/Desktop/train_timeseries/train_pp.csv", row.names = FALSE)

train <-  read.csv("C:/Users/Nidhi/OneDrive/Desktop/train_timeseries/train_pp.csv")

write.csv(soil_test,"C:/Users/Nidhi/OneDrive/Desktop/test_timeseries/test_pp.csv", row.names = FALSE)

write.csv(soil_validation,"C:/Users/Nidhi/OneDrive/Desktop/validation_timeseries/validation_pp.csv", row.names = FALSE)

head(soil_train)

soil_train <- soil_train[, -2]
soil_test <- soil_test[, -2]
soil_validation <- soil_validation[, -2]

soil_subset$score <- factor(soil_subset$score)
soil_test$score <- factor(soil_test$score)
soil_validation$score <- factor(soil_validation$score)

soil_subset$fips <- as.numeric(soil_subset$fips)
soil_test$fips <- as.numeric(soil_test$fips)
soil_validation$fips <- as.numeric(soil_validation$fips)


#EXPLORATORY DATA ANALYSIS

corr <- cor(soil_subset[, -2])
corr

str(soil_subset[, -2])

soil_subset$score <- as.numeric(soil_subset$score)

findCorrelation(corr, cutoff = 0.95, names=T)

findLinearCombos(soil_subset[, -2])

corrplot::corrplot(corr, method = "square") # Correlation among "T2MWET"    "T2MDEW"    "T2M"       "T2M_MIN"   "WS10M_MAX" "TS"   
# "WS10M"

(names(soil_subset))

soil_nodate <- soil_subset[, -2]

# plot histogram of each feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(soil_nodate)) {
  hist(soil_nodate[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(soil_nodate[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Features (", length(names(soil_nodate)), ")", sep = ""), outer=TRUE,  cex=1.2)

# plot boxplots of each feature for output values
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(soil_nodate)) {
  boxplot(soil_nodate[[i]] ~ soil_nodate$score, col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(soil_nodate[i]), cex=0.8, side=1, line=2)
}
mtext(paste("Boxplots of Features for Output Values (", length(names(soil_nodate)), ")", sep = ""), outer=TRUE,  cex=1.2)


# plot scatter of each feature for output values
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(soil_train)) {
  scatter.smooth(soil_train[[i]], soil_train$score, col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(soil_train[i]), cex=0.8, side=1, line=2)
}
mtext(paste("Scatter of Features for Output Values (", length(names(soil_train)), ")", sep = ""), outer=TRUE,  cex=1.2)

par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(soil_nodate)) {
  ggplot(data = soil_nodate, aes(soil_nodate[[i]], "score"))+
    geom_point(aes(colour="score"))
  
  scatter.smooth(soil_train[[i]], soil_train$score, col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(soil_train[i]), cex=0.8, side=1, line=2)
}
mtext(paste("Scatter of Features for Output Values (", length(names(soil_train)), ")", sep = ""), outer=TRUE,  cex=1.2)

ggplot(data= soil_nodate, aes(x='score')) +
  geom_bar()

#preprocessing

pp_df_pim <- preProcess(soil_train[, -20], method = c("BoxCox", "center", "scale")) # Transform values

pp_soil <- data.frame(predict(pp_df_pim, soil_train))

head(pp_soil)

# plot histogram of preprocessed feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(pp_soil)) {
  hist(pp_soil[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(pp_soil[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Scaled Features (", length(names(pp_soil)), ")", sep = ""), outer=TRUE,  cex=1.2)


#downsampling
downsampled <- downSample(soil_train[, -20], y = soil_train$score, list = FALSE)
hist(x = as.numeric(downsampled$Class))
table(downsampled$Class)

colnames(downsampled)[which(names(downsampled) == "Class")] <- "score"  

model1 <- rpart(score~., soil_train, method = "class")

summary(model1)

predict_v <- predict(model1, soil_validation, "class")

unique(predict_v)

unique(soil_train$score)

(mean(soil_validation$score==predict_v)*100)

predict_t <- predict(model1, soil_test, "class")

(mean(soil_test$score==predict_t)*100)


model2 <- C50::C5.0(score~., soil_train, method = "class")

predict2_t <- predict(model2, soil_test)

(mean(soil_test$score==predict2_t)*100)

predict2_v <- predict(model2, soil_validation)

(mean(soil_validation$score==predict2_v)*100)

model3 <- nnet::multinom( score~. , data= soil_train )

summary(model3)

predict3_t <- predict(model3, soil_test)

(mean(soil_test$score==predict3_t)*100)

predict3_v <- predict(model3, soil_validation)

(mean(soil_validation$score==predict2)*100)
