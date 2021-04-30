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
library(DMwR)
library(nnet)

set.seed(100)

soil_train <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/train_timeseries/train_pp.csv')

#soil_test <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/test_timeseries/test_pp.csv')

#soil_validation <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/validation_timeseries/validation_pp.csv')

soil_train <- dfSoilTr
soil_test <- dfSoilTs
soil_validation <- dfSoilVa

soil_original <- soil_train

describe(soil_train)

unique(soil_train$date)
unique(soil_test$date)
unique(soil_validation$date)
ulst <- lapply(soil_train, unique)
str(ulst)
ulst$yr_mon

#DATASET'S TOO LARGE TO PROCESS ANYTHING SO WE REDUCE THE OBSERVATIONS AND TAKE 5 YEARS OF DATA (2011-2016)

soil_subset <- subset(x = soil_train[soil_train$date >= "2011-01-04",])
soil_subset <- soil_train

dim(soil_subset)

str(soil_subset)

#Converting the columns to numeric for data analysis
#soil_subset <- soil_downsampling #TODO hack remove this

soil_subset$fips = as.numeric(soil_subset$fips)

soil_subset$date = as.Date(soil_subset$date)

soil_subset$score <- as.numeric(soil_subset$score)

str(soil_subset)

#EXPLORATORY DATA ANALYSIS

soil_cor <- cor(soil_subset[, -c(1)])
#soil_cor <- cor(soil_subset)

cor_with_score <- data.frame(soil_cor[20,])

cor_with_score

#The correlation matrix shows that, only 
#PS, T2M_MAX, T2M_RANGE, TS, WS10M_RANGE ARE CORRELATED TO OUR DEPENDENT VARIABLE~ SCORE.

corrplot(soil_cor, type = "lower")


sort(findCorrelation(soil_cor, cutoff = 0.75, names=T)) # High Correlation among 
#"QV2M"        "T2M"         "T2M_MAX"     "T2M_MIN"     "T2MDEW"      "T2MWET"     
#"WS10M"       "WS10M_MAX"   "WS10M_MIN"   "WS10M_RANGE" "WS50M"       "WS50M_MAX"  

soil_nodate <- soil_subset[, -2]
soil_nodate <- soil_subset[, -1] #month data
soil_nodate <- soil_subset
str(soil_nodate)

# plot histogram of each feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(soil_nodate)) {
  hist(soil_nodate[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(soil_nodate[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Features (", length(names(soil_nodate)), ")", sep = ""), outer=TRUE,  cex=1.2)

#THE HISTOGRAM PLOT SHOWS THAT THE FEATURES ARE NOT NORMALLY DISTRIBUTED
pp_df_pim <- preProcess(soil_nodate[, -c(20)], method = c("range"))
pp_df_pim <- preProcess(soil_nodate[, -c(20)], method = c("BoxCox", "center", "scale")) # Transform values

pp_soil <- data.frame(predict(pp_df_pim, soil_nodate))


# Remove outliers
describe(pp_soil)
for (i in names(pp_soil[-c(20)])) {
  pp_soil <- pp_soil[!abs(pp_soil[[i]]) > 3 ,]
}

pp_soil$score = round(pp_soil$score) 



summary(pp_soil)

# plot histogram of preprocessed feature
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(pp_soil)) {
  hist(pp_soil[[i]], col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(pp_soil[i]), cex=0.8, side=1, line=2)
  
}
mtext(paste("Histograms of Scaled Features (", length(names(pp_soil)), ")", sep = ""), outer=TRUE,  cex=1.2)

#The data has been scaled and normalized

summary(pp_soil)

# plot boxplots of each feature for output values
par(mfrow=c(5,4), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)
for (i in names(pp_soil)) {
  boxplot(pp_soil[[i]] ~ pp_soil$score, col="wheat2", ylab = "", xlab = "", main = "")
  mtext(names(pp_soil[i]), cex=0.8, side=1, line=2)
}
mtext(paste("BoxPlots of Normalized Features (", length(names(pp_soil)), ")", sep = ""), outer=TRUE,  cex=1.2)



#MODEL BUILDING

#DOWNSAMPLING TO IMPROVE PREDICTION

str(pp_soil)

pp_soil$score = round(pp_soil$score, 0) 
pp_soil <- pp_soil[pp_soil$score!=0, ] # Non Zero only, 
pp_soil$score <- as.factor(pp_soil$score)
soil_downsampling <- pp_soil # and skip downsample

ulst <- lapply(soil_downsampling, unique)
str(ulst)


table(pp_soil$score)

soil_downsampling <- downSample(x = pp_soil[,-20], y = pp_soil$score, list = FALSE, yname = 'score')

str(soil_downsampling)

table(soil_downsampling$score)

#preprocessing

split <- createDataPartition(soil_downsampling$score, p= 0.8, list = FALSE)

soil_train <- soil_downsampling[split, ]

soil_test <- soil_downsampling[-split,]

#According to the correlation matrix, only PS, T2M_MAX, T2M_RANGE, TS, WS10M_RANGE ARE CORRELATED TO OUR DEPENDENT VARIABLE SCORE.

#BUILDING A MODEL USING ONLY THESE COLUMNS

feats <- names(soil_downsampling[c(1,  3, 8, 10, 15)])

feats

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('score ~',f)

# Convert to formula
f <- as.formula(f)
f

#using naive bayes for classification


Naive_Bayes_Model =naiveBayes(f, data=soil_train)

Naive_Bayes_Model

#Prediction on the dataset
prediction_test =predict(Naive_Bayes_Model,soil_test)

prediction_train =predict(Naive_Bayes_Model,soil_train)

(mean(prediction_test==soil_test$score))

(mean(prediction_train==soil_train$score))

#BUILDING AN ORDINAL MODEL


ordinal_model <- polr(f, data= soil_train, Hess = TRUE)

summary(ordinal_model)

summary_table <- coef(summary(ordinal_model))

logLik(ordinal_model)

pval <- pnorm(abs(summary_table[,'t value']), lower.tail = FALSE)*2

summary_table <- cbind(summary_table, "p value" = round(pval,3))

summary_table

prediction_polr <- predict(ordinal_model, newdata = soil_test)

#p VALUE FOR TS IS LARGE AND THUS THIS COLUMN CAN BE REMOVED

y_actual <- soil_test[,20]

mean(prediction_polr == y_actual)


prediction_polr_train <- predict(ordinal_model, newdata = soil_train)

#p VALUE FOR TS IS LARGE AND THUS THIS COLUMN CAN BE REMOVED

y_actual <- soil_train[,20]

mean(prediction_polr_train == y_actual)


#C5.0 

model_C50 <- C50::C5.0(f, soil_train, method = "class")

prediction_test =predict(model_C50,soil_test)

prediction_train =predict(model_C50,soil_train)

(mean(prediction_test==soil_test$score))

(mean(prediction_train==soil_train$score))

#CART

model_RPART <- rpart(f, soil_train, method = "class")

summary(model_RPART)
plot(model_RPART)
text(model_RPART)

prediction_test =predict(model_RPART,soil_test, "class")

prediction_train =predict(model_RPART,soil_train, "class")

(mean(prediction_test==soil_test$score))

(mean(prediction_train==soil_train$score))

cm <- confusionMatrix(data= prediction_train, reference = soil_train$score)

#NEURAL NETWORK

model_nn <- nnet(f,soil_downsampling, size=80, linout = FALSE, maxit = 10000)

#model_nn <- nnet(f,soil_train, size=80, linout = FALSE, maxit = 10000)


prediction_test =predict(model_nn,soil_test, 'class')

prediction_train =predict(model_nn,soil_train, 'class')

(mean(prediction_test==soil_test$score))

(mean(prediction_train==soil_train$score))

#SVM

classifierR = svm(formula = f,
                  data = soil_train,
                  x = soil_train[, -20],
                  y= soil_train$score,
                  type = 'C-classification',
                  kernel = 'radial')


?svm

