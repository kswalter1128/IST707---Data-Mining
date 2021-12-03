require(data.table)
require(tidyverse)
require(caret)
require(RWeka)

#Prior to Import converted the 1/0 columns to Factors

#Import the data
Train <- read.arff("train.arff") %>% as.data.table()
Test <- read.arff("test.arff") %>% as.data.table()

#Set Statistics
TrainSet <- read.csv("train.csv")
ggplot(Train, aes(x=price_range))+geom_histogram(stat = "count")

#Stripped off the id column so the columns between test and train are the same
Test <- Test[,-1]

InfoGainAttributeEval(price_range~.,data = Train)



#Training Controls - setting up cross validation rules.
Controls <- trainControl(method = 'cv', number = 10)

set.seed(9)
dtModel <- train(price_range~.,data = Train, method = 'J48', trControl = Controls)
plot(dtModel$finalModel)
dtModel

set.seed(9)
rfModel <- train(price_range~.,data = Train, method = 'rf', trControl = Controls)
rfModel
plot(rfModel$finalModel)

set.seed(9)
svmModel <- train(price_range~.,data = Train, method = 'svmRadial', trControl = Controls)
svmModel$modelInfo

set.seed(9)
nbModel <- train(price_range~.,data = Train, method = 'svmRadial', trControl = Controls)
nbModel

Test$Prediction <- predict(rfModel, Test)
Test <- as.data.table(Test)

Test[, .(NumberofPhones = .N), .(Prediction)]
