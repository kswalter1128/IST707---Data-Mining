require(readxl)
require(readr)
require(data.table)

TitanicDF <- fread("test.csv")
TitanicDF$PassengerId <- as.character(TitanicDF$PassengerId)
TitanicDF$Pclass <- factor(TitanicDF$Pclass, levels = c("1","2","3"), ordered = T)
TitanicDF$Sex <- factor(TitanicDF$Sex)
TitanicDF$SibSp <- factor(TitanicDF$SibSp)
TitanicDF$Parch <- as.character(TitanicDF$Parch)
summary(TitanicDF)
TitanicDF$Age[is.na(TitanicDF$Age)] <- mean(TitanicDF$Age, na.rm = T)
TitanicDF$Fare[is.na(TitanicDF$Fare)] <- mean(TitanicDF$Fare, na.rm = T)

ThirdClass <- TitanicDF[Pclass==3]
hist(ThirdClass$Fare)
boxplot(ThirdClass$Fare)

Survived <- fread("gender_submission.csv")
Survived$PassengerId <- as.character(Survived$PassengerId)
TitanicDF <- merge(TitanicDF, Survived, by = "PassengerId")
table(TitanicDF$Embarked, TitanicDF$Survived)

TitanicDF[,.(AverageFare=mean(Fare)),.(Sex)]

OrdinalAge <- cut(TitanicDF$Age, breaks = c(0,10,20,30,40,50,60,Inf), labels = c("Child","Teens","Twenties","Thirties","Fourties","Fifties","Old"))
OrdinalAge
plot(TitanicDF$Age, log(TitanicDF$Age))


min_max <- function(field) {
  (field-min(field))/(max(field)-min(field))
}
plot(min_max(TitanicDF$Age))


plot(scale(TitanicDF$Age))

plot(log(TitanicDF$Age))

TitanicRandome <- TitanicDF [ sample( 1:nrow( TitanicDF ), 100, replace = F ) ]
TitanicSystemSample <- TitanicDF[seq(1, nrow(TitanicDF),10)]
