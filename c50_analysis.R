#C.50 Models for Soil Data
library(C50)
library(nnet)
library(corrplot)
library(caret)



### Extract, Transform and Load (ETL) Data source ###
# dfGenre <- read.csv("/Users/lujackso/Downloads/Kaggle/spotify_century_data/data_w_genres.csv", header = TRUE, stringsAsFactors = F)

#dfSoil <- read.csv("D:/Users/lujackso/Downloads/soil/train_timeseries/train_timeseries.csv", 


#dfSoilTr <- read.csv("C:/Users/gvmds/Downloads/psu-ie-575-team-4/data/train_soil_by_month.csv", 
dfSoilTr <- read.csv("C:/Users/gvmds/Downloads/train_timeseries/train_timeseries.csv",
                     #dfSoilTr <- read.csv("/Users/lujackso/Downloads/DAAN-570-302-scratch/psu-ie-575-team-4/data/train_soil_by_month.csv", 
                     header = TRUE, 
                     stringsAsFactors = F,
                     comment.char="")

#dfSoilTs <- read.csv("C:/Users/gvmds/Downloads/psu-ie-575-team-4/data/test_soil_by_month.csv", 
dfSoilTs <- read.csv("C:/Users/gvmds/Downloads/test_timeseries/test_timeseries.csv",
#dfSoilTs <- read.csv("/Users/lujackso/Downloads/DAAN-570-302-scratch/psu-ie-575-team-4/data/test_soil_by_month.csv", 
                     header = TRUE, 
                     stringsAsFactors = F,
                     comment.char="")



#dfSoilVa <- read.csv("C:/Users/gvmds/Downloads/psu-ie-575-team-4/data/validation_soil_by_month.csv", 
dfSoilVa <- read.csv("C:/Users/gvmds/Downloads/validation_timeseries/validation_timeseries.csv",
#dfSoilVa <- read.csv("/Users/lujackso/Downloads/DAAN-570-302-scratch/psu-ie-575-team-4/data/validation_soil_by_month.csv", 
                     header = TRUE, 
                     stringsAsFactors = F,
                     comment.char="")

rm(dfSoilsr)

str(dfSoilTr)

cor_input <- dfSoilTr[-c(1)] # no filter
cor_input <- dfSoilClean[-c(2)] # no filter on non stnd data
cor_input <- dTSoil[-c(2,7,6,4,9,5,13,8,12,17,14)] # non zero scores cor filter
cor_input <- dTSoil[-c(2,7,6,9,5,13,11,17,12,14)] # full data set cor filter

pim_cor <- cor(cor_input) 

describe(cor_input)

names(cor_input)
pim_len <- length(names(cor_input))
print(pim_len)

# Plot correlation matrix
#par(mfrow=c(1,1), oma = c(1,1,1,1) + 0.1,  mar = c(1,1,1,1) + 0.1)
corrplot(pim_cor, type = "lower", main = paste("Correlation of Features (", pim_len, ")", sep = ""), mar=c(0,0,3,0), oma = c(1,1,1,1))
# Search for problematic features
findCorrelation(pim_cor, cutoff = 0.90)
findCorrelation(pim_cor, cutoff = 0.70)

#table cor
cor_with_score <- data.frame(pim_cor[20,])
names(cor_with_score)
ts <- as.data.frame(cor_with_score[order(cor_with_score, decreasing = TRUE),])
ts
str(pim_cor)




# normalize our input data
describe(dfHeart)
str(dfHeart)
str(dfSoilTr)
dfSoilPP <- preProcess(dfSoilTr[-c(1,21)], method = c("range"))
dfSoilPP_train <- cbind(predict(dfSoilPP, dfSoilTr[-c(1)]), Type = dfSoilTr$score)
dfSoilPP_test <- cbind(predict(dfSoilPP, dfSoilTs[-c(1)]), Type = dfSoilTs$score)

ulst <- lapply(round(dfSoilPP_train[-c(1)]), unique)
str(ulst)

# run the nnet model
nn_model_0 <- nnet(score ~ ., data = dfSoilPP_train, size=10, maxit = 1000)
train_predictions <- predict(nn_model_0, dfSoilPP_train, type = "class")
mean(train_predictions == dfSoilTr$score)

nn_model_1 <- nnet(score ~ ., data = dfSoilTr, size=10, maxit = 2000, decay = 0.01) # Queston C
train_predictions <- predict(nn_model_1, dfSoilPP_train, type = "class")
mean(train_predictions == dfSoilTr$score)

test_predictions <- predict(nn_model_1, dfSoilPP_test, type = "class")
mean(test_predictions == dfSoilTs$score)


# build regression trees
# Decision tree classification with C50
# Decision [regression] tree classification with rpart [R implementation of CART] 

dfSoilPP_train$score <- as.factor(dfSoilPP_train$score) # convert to factor
dfSoilPP_test$score <- as.factor(dfSoilPP_test$score) # convert to factor


dfHeart_c50 <- C5.0(score ~ ., data=dfSoilPP_train, trials=1)
dfHeart_c50
dfHeart_c50T <- C5.0(score ~ ., data=dfSoilPP_train, trials=50)
dfHeart_c50T

# Compare results
# C5.0
dfHeartPred_0 <- predict(dfHeart_c50, dfSoilTs)
dfHeartPred_0.acc <- mean(dfSoilTs$score == dfHeartPred_0)*100
sprintf("C5.0 accuracy: %1.2f%%", dfHeartPred_0.acc)
summary(dfHeartPred_0)
confusionMatrix(table(dfHeartPred_0, dfSoilTs$score))

# C5.0 [Tuned]
dfHeartPred_1 <- predict(dfHeart_c50T, dfSoilTs)
dfHeartPred_1.acc <- mean(dfSoilTs$score == dfHeartPred_1)*100
sprintf("C5.0 [Tuned] accuracy: %1.2f%%", dfHeartPred_1.acc)
summary(dfHeartPred_1)
confusionMatrix(table(dfHeartPred_1, dfSoilTs$score))

# Visualize/plot results
# plot trees
par(mfrow=c(1,2), oma = c(0,0,2,0) + 0.1,  mar = c(3,3,1,1) + 0.1)#, xpd = TRUE) # define graphic parameter 
par(xpd = TRUE) # define graphic parameter 
#plot(dfHeart_c50, main = 'Rpart (CART) tree', subtree = 9) # plot the tree , type="simple"
plot(dfHeart_c50, main = 'C5.0 (CART) tree') # plot the tree , type="simple"
text(dfHeart_c50, use.n = TRUE, cex=0.7) # add text labels to tree 
par(xpd = TRUE) # define graphic parameter 
plot(dfHeart_c50T, main = 'C5.0 (CART) [Tune] tree') # plot the tree 
text(dfHeart_c50T, use.n = TRUE, cex=0.5) # add text labels to tree 