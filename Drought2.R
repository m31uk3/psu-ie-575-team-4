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
install.packages('DMwR')
library(DMwR)
library(nnet)

set.seed(100)

soil_train <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/train_timeseries/train_pp.csv')

data.frame(names(soil_train))

soil_test <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/test_timeseries/test_pp.csv')

soil_validation <- read.csv('C:/Users/Nidhi/OneDrive/Desktop/validation_timeseries/validation_pp.csv')

soil_original <- soil_train

unique(soil_train$date)
unique(soil_test$date)
unique(soil_validation$date)

#DATASET'S TOO LARGE TO PROCESS ANYTHING SO WE REDUCE THE OBSERVATIONS AND TAKE 5 YEARS OF DATA (2011-2016)

soil_subset <- subset(x = soil_train[soil_train$date >= "2011-01-04",])

dim(soil_subset)

str(soil_subset)

soil_subset$fips = as.numeric(soil_subset$fips)

soil_subset$date = as.Date(soil_subset$date)

soil_subset$score <- as.numeric(soil_subset$score)

str(soil_subset)

soil_cor <- cor(soil_subset[, -2])

cor_with_score <- data.frame(soil_cor[20,])

cor_with_score

corrplot(soil_cor)

sort(findCorrelation(soil_cor, cutoff = 0.7))

#MODEL BUILDING

#DOWNSAMPLING TO IMPROVE PREDICTION

str(soil_subset)

soil_subset$score <- as.factor(soil_subset$score)

table(soil_subset$score)

soil_downsampling <- downSample(x = soil_subset[, -2], y = soil_subset$score, list = FALSE)

table(soil_downsampling$score)

str(soil_downsampling)

#preprocessing

pp_df_pim <- preProcess(soil_downsampling[, -c(20,21)], method = c("BoxCox", "center", "scale")) # Transform values

pp_soil <- data.frame(predict(pp_df_pim, soil_downsampling))

table(pp_soil$score)

soil <- pp_soil[, -21]
                          
split <- createDataPartition(soil$score, p= 0.8, list = FALSE)

soil_train <- soil[split, ]

soil_test <- soil[-split,]

#According to the correlation matrix, only PS, T2M_MAX, T2M_RANGE, TS, WS10M_RANGE ARE CORRELATED TO OUR DEPENDENT VARIABLE SCORE.

#BUILDING A MODEL USING ONLY THESE COLUMNS

feats <- names(soil[c(1, 2, 3, 8, 10, 15, 19)])

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

#NEURAL NETWORK

model_nn <- nnet(f,soil_train, size=80, linout = FALSE, 
                 maxit = 10000)


prediction_test =predict(model_nn,soil_test, 'class')

prediction_train =predict(model_nn,soil_train, 'class')

(mean(prediction_test==soil_test$score))

(mean(prediction_train==soil_train$score))
