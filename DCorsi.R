# dCorsi Using kNN Regression and Repeated 10-Fold CV
# Recreation of Steve Burtch's dCorsi, which used OLS regression
# ...to get an Expected Corsi (Number of Shot Attempts For/Against)
# Using kNN, we can get slightly better estimates for Expected Corsi


library(caret)
set.seed(1738)

xD <- read.csv("Updated xD 08-16.csv")
xD2 <- xD[sample(nrow(xD)),]

scaledxD <- scale(xD2[,46:55])

xDcombined <- cbind(xD2, scaledxD)

# Creates a test and training set
xD_train <- xDcombined[1:3000,]
xD_test <- xDcombined[3001:3658,]

# Easier variables to remember for kNN regression
TrainDV1 <- xD_train[,7]
TrainIV <- xD_train[,56:64]

# Using Repeated 10-Fold CV to find Optimal K for xCF60
set.seed(1985)

knnFit1 <- train(TrainIV, TrainDV1,
                 method = "knn",
                 tuneLength = 25,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10, repeats = 3))


# KNN Regression for xCF60
knncf60 <- knnreg(TrainIV, TrainDV1, k = knnFit1$bestTune)
CF60pred <- predict(knncf60, xD_test[,56:64])
combinedtest <- cbind(xD_test, CF60pred)
# summary(CF60reg <- lm(CF60~CF60pred, data = combinedtest))


# Using Repeated 10-Fold CV to find Optimal K for xCA60
set.seed(1211)

knnFit2 <- train(xD_train[,57:65], xD_train[,8],
                 method = "knn",
                 tuneLength = 25,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10, repeats = 3))


# KNN Regression for xCA60
knnca60 <- knnreg(xD_train[,57:65], xD_train[,8], k = knnFit2$bestTune)
CA60pred <- predict(knnca60, xD_test[,57:65])
combinedtest <- cbind(combinedtest, CA60pred)
# summary(CA60reg <- lm(CA60~CA60pred, data = combinedtest))


# Creates variables for dCorsi For, dCorsi Against, and Total dCorsi (as rates)
DCF60 <- (combinedtest$CF60 - combinedtest$CF60pred)
combinedtest <- cbind(combinedtest, DCF60)
DCA60 <- (combinedtest$CA60pred - combinedtest$CA60)
combinedtest <- cbind(combinedtest, DCA60)
DC60 <- combinedtest$DCF60+combinedtest$DCA60
combinedtest <- cbind(combinedtest, DC60)

# Transforms the rate stats to impact stats (multiplying by total time on ice)
DCF_Impact <- (combinedtest$DCF60*combinedtest$TOI)/60
combinedtest <- cbind(combinedtest, DCF_Impact)
DCA_Impact <- (combinedtest$DCA60*combinedtest$TOI)/60
combinedtest <- cbind(combinedtest, DCA_Impact)
DC_Impact <- (combinedtest$DC60*combinedtest$TOI)/60
combinedtest <- cbind(combinedtest, DC_Impact)


