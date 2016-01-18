setwd("F:/1 - NCSU/Projects/Titanic Machine Learning from Disaster")
train<-read.csv("train.csv",header=TRUE)#,stringsAsFactors = FALSE)
test<-read.csv("test.csv",header=TRUE)#,stringsAsFactors = FALSE)
head(train)
table(train$Survived)
prop.table(table(train$Survived))
test$Survived<-rep(0,418)
submit<-data.frame(PassengerID=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="AllPerish.csv",row.names = FALSE)
summary(train$Sex)
prop.table(table(train$Sex,train$Survived),1)
table(train$Sex,train$Survived)
test$Survived<-0

test$Survived[test$Sex=="female"]<-1
submit<-data.frame(PassengerID=test$PassengerId, Survived=test$Survived)
write.csv(submit,file="MenPerish.csv",row.names=FALSE)
summary(train$Age)
train$Child<-0
train$Child[train$Age<18]<-1
aggregate(Survived~Child + Sex,data=train,FUN=sum)
aggregate(Survived~Child+Sex,data=train,FUN=function(x){sum(x)/length(x)})
summary(train$Fare)
train$Fare2<-'30+'
train$Fare2[train$Fare>=20 & train$Fare<30]<-'20-30'
train$Fare2[train$Fare>=10 & train$Fare<20]<-'10-20'
train$Fare2[train$Fare<10]<-'<10'
summary(train)
aggregate(Survived~Fare2+Pclass+Sex,data=train,FUN=function(x){sum(x)/length(x)})
test$Survived<-0
test$Survived[test$Sex=='female']<-1
test$Survived[test$Sex=='female' & test$Pclass==3 & test$Fare>=20]<-0
submit<-data.frame(PassengerID=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="MenWomenWithClass3Perish.csv",row.names=FALSE)
library(rpart)
fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method="class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction<-predict(fit,test,type="class")
Prediction[1:5]
submit<-data.frame(PassengerID=test$PassengerId,Survived=Prediction)
write.csv(submit,"DecisionTreePrediction.csv",row.names=FALSE)
?rpart.control
fit<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method="class",control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#modified rcontrol params for decision trees
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(xval=20,maxdepth = 30))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
Prediction<-predict(fit,test,type="class")
submit<-data.frame(PassengerID=test$PassengerId,Survived=Prediction)
write.csv(submit,"DecisionTreeModifiedPredictions.csv",row.names=FALSE)
test$Survived<-NA
combi<-rbind(train,test)
ncol(train)
ncol(test)
str(train)

#feature engineering
Orgtrain <- read.csv("F:/1 - NCSU/Projects/Titanic Machine Learning from Disaster/train.csv")
test$Survived<-NA #to make equal no. of cols in both train and test
combi<-rbind(Orgtrain,test)

combi$Name[1] # seen as factor
combi$Name<-as.character(combi$Name)
combi$Name[1] # seen as string of characters
strsplit(combi$Name[1],split = '[,.]')
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1],split='[,.]')[[1]][2]
combi$Title<-sapply(combi$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})
combi$Title[1:5]
combi$Title<-sub(' ','',combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mlle','Mme')]<-'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title<-factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname<-sapply(combi$Name, FUN=function(x){strsplit(x,split='[.,]')[[1]][1]})
str(combi)
combi$FamilyID<-paste(as.character(combi$FamilySize),combi$Surname,sep="")
combi$FamilyID[1:5]
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891,]
test <- combi[892:1309,]
str(train)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
Prediction<-predict(fit,test,type="class")
submit<-data.frame(PassengerID=test$PassengerId,Survived=Prediction)
write.csv(submit,"section4completed.csv",row.names=FALSE)

#Section5
#Random Forest
sample(1:10, replace = TRUE)
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
install.packages('randomForest')
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, importance=TRUE, ntree=2000)