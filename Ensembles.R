library(doMC)
registerDoMC(cores = 2)
library(plyr)
library(dplyr)

load("~/ds-capstone/D.RData") # load data set
D <- tbl_df(D)

dim(D)
glimpse(D)
label_name <- 'Stars'
predictors <- names(D)[names(D) != label_name]

library(caret)

# Data 3-way split {training, brewer, testing}
split1 <- createDataPartition(D$Stars, times = 1, p = .6, list = F)
training_ <- D[split1, ]
brewer_testing <- D[- split1, ] 
split2 <- createDataPartition(brewer_testing$Stars, p = .5, list = F)
brewer_ <- brewer_testing[split2, ]
testing_ <- brewer_testing[- split2, ]

## Add a preprocessing step on the numerical variables; then cbind with response variable from original data to complete
preObj <- preProcess(training_[, predictors], method = c("center", "scale"))
training <- cbind(predict(preObj, training_[, predictors]), training_[, label_name])
brewer <- cbind(predict(preObj, brewer_[, predictors]), brewer_[, label_name])
testing <- cbind(predict(preObj, testing_[, predictors]), testing_[, label_name])
#---------

myControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# predict with 3 models
m1 <- train(x = training[, predictors], y = training[, label_name], method = "rpart", trControl = myControl)
m2 <- train(x = training[, predictors], y = training[, label_name], method = "treebag", trControl = myControl)
m3 <- train(x = training[, predictors], y = training[, label_name], method = "rf", trControl = myControl)

# use trained models to predict on the brewer data and testing data
brewer$m1_pr <- predict(object = m1, brewer)
brewer$m2_pr <- predict(object = m2, brewer)
brewer$m3_pr <- predict(object = m3, brewer)
#---
testing$m1_pr <- predict(object = m1, testing)
testing$m2_pr <- predict(object = m2, testing)
testing$m3_pr <- predict(object = m3, testing)

# new predictors
predictors <- names(brewer)[names(brewer) != label_name]
predictors

# fit ensemble model using "gradient boosting machine"
fit <- train(x = brewer[, predictors], y = brewer[, label_name], method = "gbm", trControl = myControl)
# predict on testing
pp <- predict(object = fit, testing[, predictors])
confusionMatrix(testing[, label_name], pp)



