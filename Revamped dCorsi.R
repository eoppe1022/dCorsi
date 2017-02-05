# dCorsi Using kNN Regression and Repeated 10-Fold CV
# Recreation of Steve Burtch's dCorsi, which used OLS regression
# ...to get an Expected Corsi (Number of Shot Attempts For/Against)
# Using kNN, we can get slightly better estimates for Expected Corsi

library(dplyr)
library(caret)
set.seed(1738)


dCorsi <- read.csv("Updated xD 08-16.csv")

# For some reason, 2010 data is especially noisy
dCorsi %>%
  filter(Season >= 2011) -> dCorsi

ScaledDCorsi <- scale(dCorsi[,46:55])

dCorsiCombined <- cbind(dCorsi, ScaledDCorsi)

# Creates training set of years 2011, 2012, and 2014; testing sets of 2015 and 2016
trainbool <- (dCorsiCombined$Season <= 2014)
dCorsi_test <- dCorsiCombined[!trainbool,]
dCorsi_train <- dCorsiCombined[trainbool,]


# Using Repeated 10-Fold CV to find Optimal K for xCF60
set.seed(1985)

knnFit1 <- train(dCorsi_train[,56:64], dCorsi_train[,7],
                 method = "knn",
                 tuneLength = 25,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10, repeats = 3))

# KNN Regression for xCF60
knncf60 <- knnreg(dCorsi_train[,56:64], dCorsi_train[,7], k = knnFit1$bestTune)
CF60pred <- predict(knncf60, dCorsi_test[,56:64])
FinalDCorsi <- cbind(dCorsi_test, CF60pred)
# summary(CF60reg <- lm(CF60~CF60pred, data = FinalDCorsi))


# Using Repeated 10-Fold CV to find Optimal K for xCA60
set.seed(1211)

knnFit2 <- train(dCorsi_train[,57:65], dCorsi_train[,8],
                 method = "knn",
                 tuneLength = 25,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10, repeats = 3))

# KNN Regression for xCA60
knnca60 <- knnreg(dCorsi_train[,57:65], dCorsi_train[,8], k = knnFit2$bestTune)
CA60pred <- predict(knnca60, dCorsi_test[,57:65])
FinalDCorsi <- cbind(FinalDCorsi, CA60pred)
# summary(CA60reg <- lm(CA60~CA60pred, data = FinalDCorsi))


# Creates variables for dCorsi For, dCorsi Against, and Total dCorsi (as rates)
DCF60 <- (FinalDCorsi$CF60 - FinalDCorsi$CF60pred)
FinalDCorsi <- cbind(FinalDCorsi, DCF60)
DCA60 <- (FinalDCorsi$CA60pred - FinalDCorsi$CA60)
FinalDCorsi <- cbind(FinalDCorsi, DCA60)
DC60 <- FinalDCorsi$DCF60+FinalDCorsi$DCA60
FinalDCorsi <- cbind(FinalDCorsi, DC60)

# Transforms the rate stats to impact stats (multiplying by total time on ice)
DCF_Impact <- (FinalDCorsi$DCF60*FinalDCorsi$TOI)/60
FinalDCorsi <- cbind(FinalDCorsi, DCF_Impact)
DCA_Impact <- (FinalDCorsi$DCA60*FinalDCorsi$TOI)/60
FinalDCorsi <- cbind(FinalDCorsi, DCA_Impact)
DC_Impact <- (FinalDCorsi$DC60*FinalDCorsi$TOI)/60
FinalDCorsi <- cbind(FinalDCorsi, DC_Impact)
