require(data.table)
require(tidyverse)

TitanTrain <- fread("Week 2/train.csv")
TitanTest <- fread("Week 2/test.csv")


TitanTest$Pclass=ordered(TitanTest$Pclass)
TitanTrain$Pclass=ordered(TitanTrain$Pclass)
TitanTrain<-TitanTrain %>%  mutate_if(is.character, as.factor)
TitanTest <- TitanTest %>% mutate_if(is.character, as.factor)
TitanTrain$Survived <- as.factor(TitanTrain$Survived)

library(RWeka)
MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues")
trainset_nomissing <- MS(data = TitanTrain, na.action = NULL)
testset_nomissing <- MS(data = TitanTest, na.action = NULL)

myVars <- c("Pclass", "Sex", "Age", "SibSp", "Fare", "Survived")
newTrain <- trainset_nomissing[myVars]
newTest <- testset_nomissing[myVars[1:5]]

require(caret)

set.seed(123)
train(Survived~., data = newTrain, method ="knn", preProcess = c("center","scale"), tuneLength = 50)

require(e1071)

Basic <- svm(Survived~., data = newTrain, kernel = "sigmoid")
Radial <- svm(Survived~., data = newTrain, kernel = "radial")

BPred <- predict(Basic, newTrain) 
confusionMatrix(BPred, newTrain$Survived)

RPred <- predict(Radial, newTrain)
confusionMatrix(RPred, newTrain$Survived)

#Cost .8
Basic <- svm(Survived~., data = newTrain, kernel = "sigmoid", cost = .8)
Radial <- svm(Survived~., data = newTrain, kernel = "radial", cost = .8)

BPred <- predict(Basic, newTrain) 
confusionMatrix(BPred, newTrain$Survived)

RPred <- predict(Radial, newTrain)
confusionMatrix(RPred, newTrain$Survived)

#Cost .5
Basic <- svm(Survived~., data = newTrain, kernel = "sigmoid", cost = .5)
Radial <- svm(Survived~., data = newTrain, kernel = "radial", cost = .5)

BPred <- predict(Basic, newTrain) 
confusionMatrix(BPred, newTrain$Survived)

RPred <- predict(Radial, newTrain)
confusionMatrix(RPred, newTrain$Survived)

#Cost .3
Basic <- svm(Survived~., data = newTrain, kernel = "sigmoid", cost = .5)
Radial <- svm(Survived~., data = newTrain, kernel = "radial", cost = .5)

BPred <- predict(Basic, newTrain) 
confusionMatrix(BPred, newTrain$Survived)

RPred <- predict(Radial, newTrain)
confusionMatrix(RPred, newTrain$Survived)

#Cost .1
Basic <- svm(Survived~., data = newTrain, kernel = "sigmoid", cost = .1)
Radial <- svm(Survived~., data = newTrain, kernel = "radial", cost = .1)

BPred <- predict(Basic, newTrain) 
confusionMatrix(BPred, newTrain$Survived)

RPred <- predict(Radial, newTrain)
confusionMatrix(RPred, newTrain$Survived)
