library(doMC)
registerDoMC(cores = 2)
library(plyr)
library(dplyr)

load("~/ds-capstone/D.RData") # load data set
D <- tbl_df(D)

### Prediction models ###
library(caret, warn.conflicts = F, quietly = T, verbose = F)
library(party, warn.conflicts = F, quietly = T, verbose = F)
library(partykit, warn.conflicts = F, quietly = T, verbose = F)
library(randomForest)

### Data Sclicing
# Response variable "Stars" in D$Stars
inTrain <- createDataPartition(D$Stars, p = .75, list = FALSE)
training <- D[inTrain, ]
testing <- D[-inTrain, ]
####
labelName <- "Stars"   # response variable to predict
predictors <- names(D)[names(D) != labelName] # the names of predictor variables

## Preprocessing
preObj <- preProcess(training[, predictors], method = c("center", "scale"))
#preObj <- preProcess(training[, -14], method = "pca", pcaComp = 8, thresh = 0.99)
training_proc <- predict(preObj, training[, predictors])
testing_proc <- predict(preObj, testing[, predictors])


# Use this data for training

# train_data <- training_proc[, -c(1, 5, 10, 11, 12)] 
#train_data <- training_proc[, - c(5)]
train_data <- training_proc

#####################
# Identifying correlated predictors
descrCor <-  cor(train_data)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .95)
highCorr
# Find linear combos
comboInfo <- findLinearCombos(train_data)
comboInfo

######################
# Use 10-folds Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

###### Train a CART
cart <- train(training$Stars ~., data = train_data, method = 'rpart', trControl = ctrl, tuneLength = 9)
print.train(cart)
# Best result
subset(cart$results, cp == cart$bestTune$cp)

# plot decision tree
cart_model <- as.party(cart$finalModel) # convert to party object for better vizualization
plot(cart_model) # visualize tree
plot.train(cart, metric = "Accuracy") # plot ressampling profiles (Accuracy vs Complexity Parameter)

model1 <- predict(cart, testing_proc)
confusionMatrix(testing$Stars, model1)

###### Random Forest
rf <- randomForest(training$Stars ~., data = train_data, importance = T, mtry = 2)
print(rf)
plot(rf)

model2 <- predict(rf, testing_proc)
confusionMatrix(testing$Stars, model2)



