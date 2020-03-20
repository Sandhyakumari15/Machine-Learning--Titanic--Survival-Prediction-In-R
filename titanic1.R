
library(readr)
titanictrain<- read.csv(file.choose())
titanictest<-read.csv(file.choose())
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 20 13:16:07 2020

@author: windows 10
"""

library(readr)
titanictrain<- read.csv("../input/titanic/train.csv")
titanictest<-read.csv("../input/titanic/test.csv")




titanictrain$Sex<-ifelse(titanictrain$Sex=="male",1,0)

titanictest$Sex<-ifelse(titanictest$Sex=="male",1,0)

titanictrain<-subset(titanictrain,select = -c(Cabin,PassengerId,Name,Ticket,Embarked,SibSp))

titanictest<-subset(titanictest,select = -c(Cabin,PassengerId,Name,Ticket,Embarked,SibSp))

boxplot(titanictrain)
plot(titanictrain)
hist(titanictrain$Fare)
quantile(titanictrain$Fare)
lowerq = quantile(titanictrain$Fare)[2]
upperq = quantile(titanictrain$Fare)[4]
iqr = upperq - lowerq
upperth<-upperq+(1.5*iqr)
lowerth<-lowerq-(1.5*iqr)
summary(titanictrain$Fare)
titanictrain$Fare<-ifelse(titanictrain$Fare>65.6,65.6,titanictrain$Fare)
titanictest$Fare<-ifelse(titanictest$Fare>65.6,65.6,titanictest$Fare)

quantile(titanictrain$Fare)
boxplot(titanictrain)
sum(is.na(titanictrain))
sum(is.na(titanictest))
library(mice)
library(DMwR)
summary(titanictrain)
knnimputtrain<-knnImputation(titanictrain,k=3)
knnimputtest<-knnImputation(titanictest,k=3)

install.packages("visdat")
library(visdat)
vis_dat(knnimputtrain)
vis_dat(knnimputtest)
titanictrain$Age<-as.integer(titanictrain$Age)
knnimputtrain$Age<-as.integer(knnimputtrain$Age)

titanictest$Age<-as.integer(titanictest$Age)
knnimputtest$Age<-as.integer(knnimputtest$Age)
agefinal<-cbind(titanictrain$Age,knnimputtrain$Age)

sum(is.na(knnimputtrain))
sum(is.na(knnimputtest))
str(knnimputtrain)
cor(knnimputtrain)

knnimputtrain$Survived<-as.factor(knnimputtrain$Survived)

library(randomForest)

model<-randomForest(Survived~.,data=knnimputtrain,importance=TRUE)
predictmodel_train<-predict(model,knnimputtrain)
predictmodel_test<-predict(model,knnimputtest)
mean(predictmodel_train==knnimputtrain$Survived)

#boosting
library(adabag)
adaboost = boosting(Survived ~ .,
                    data = knnimputtrain,
                    mfinal = 30,boos = TRUE)

importanceplot(adaboost)
adaboost$importance
preds_train = predict(adaboost,knnimputtrain,type='response')

preds_test = predict(adaboost,knnimputtest,type='response')

mean(knnimputtrain$Survived==preds_train$class)

