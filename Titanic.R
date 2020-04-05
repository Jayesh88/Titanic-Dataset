library(gmodels) 
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(caTools)
library(pROC)
library(caret)
library(glmnet)
library(mlbench)
library(psych)

rd_train<-read.csv('C:/Users/HP PC/Documents/Titanic/train.csv')
rd_test<-read.csv('C:/Users/HP PC/Documents/Titanic/test.csv')
gndr_submission<-read.csv('C:/Users/HP PC/Documents/Titanic/gender_submission.csv')

head(rd_train)
str(rd_train)

ggplot(rd_train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.5)
ggplot(rd_train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)),alpha=0.5)

ggplot(rd_train,aes(Survived))+geom_bar(aes(fill=factor(Pclass)),position = "dodge",alpha=0.5)

ggplot(rd_train,aes(x=Pclass,y=Age))+geom_boxplot(aes(fill=factor(Pclass)))

#Data cleaning

#We cateorised the passengers on the pclass by using boxplot 
#take median for NA values of AGE as we see it in the boxplot

for (i in 1:nrow(rd_train)) {
  if(rd_train$Pclass[i]==1 & is.na(rd_train$Age[i]) ){
    rd_train$Age[i]=37
  } else if(rd_train$Pclass[i]==2 & is.na(rd_train$Age[i])) {
    rd_train$Age[i]=29
  } else if(rd_train$Pclass[i]==3 & is.na(rd_train$Age[i])){
    rd_train$Age[i]=24
  }
}

# for (i in 1:nrow(rd_test)) {
#   if(rd_test$Pclass[i]==1 & is.na(rd_test$Age[i]) ){
#     rd_test$Age[i]=37
#   } else if(rd_test$Pclass[i]==2 & is.na(rd_test$Age[i])) {
#     rd_test$Age[i]=29
#   } else if(rd_test$Pclass[i]==3 & is.na(rd_test$Age[i])){
#     rd_test$Age[i]=24
#   }
# }


sum(is.na(rd_train))
sum(is.na(rd_test))

  
# rd_test$Fare<-replace(rd_test$Fare,is.na(rd_test$Fare),mean(rd_test$Fare,na.rm = T))
# rd_test$Fare<-lapply(rd_test$Fare,function(x)gsub(".0000"," ",as.numeric(x)))
# rd_test$Fare<-as.numeric(rd_test$Fare)

str(rd_train)
str(rd_test)

plot(rd_train)

ggplot(rd_train,aes(x=Survived,fill=Sex))+geom_bar(position = 'fill')+ggtitle('Titanic Survival:Male Vs Female')

#logistic regression

logistic_model<-glm(Survived~Pclass+Sex+Age,data = rd_train,family = binomial(link = 'logit'))
summary(logistic_model)

pred_log_prob<-predict(logistic_model,newdata = rd_test,type = 'response')
pred_log<-ifelse(pred_log_prob>0.5,1,0)
CrossTable(pred_log)

#we can predict an accuracy of 91% on the Test data set as we compare it with the Gender submission
mean(pred_log==gndr_submission$Survived,na.rm=TRUE)

confusion_mat_log<-table(pred_log,gndr_submission$Survived)


#Decision tree
library(rpart)
library(rpart.plot)
Decision_model<-rpart(Survived~Pclass+Sex+Age,data = rd_train)
summary(Decision_model)

pred_mod_decesion<-predict(Decision_model,newdata = rd_test)
pred_dec<-ifelse(pred_mod_decesion>0.5,1,0)
CrossTable(pred_dec)

#Accuracy obtained from decision tree is 97.36%
mean(pred_dec==gndr_submission$Survived)
rpart.plot(Decision_model, box.palette="RdBu", shadow.col="gray", nn=TRUE)

confusion_mat_dec<-table(pred_dec,gndr_submission$Survived)


#Random Forest
library(randomForest)
set.seed(123)
rndm_frst<-randomForest(Survived~Pclass+Sex+Age,data = rd_train)
summary(rndm_frst)

pred_for_prob<-predict(rndm_frst,rd_test)
pred_forest<-ifelse(pred_for_prob>0.5,1,0)

#Accuracy obtained from random forest is 93.73%
mean(pred_forest==gndr_submission$Survived,na.rm=TRUE)

imp<-importance(rndm_frst)
varImpPlot(rndm_frst)

confusion_mat_ran<-table(pred_forest,gndr_submission$Survived)

#support machine vector
library(e1071)
sop_ac_vec<-svm(Survived~Pclass+Sex+Age,data =rd_train)
summary(sop_ac_vec)

pred_for_svm<-predict(sop_ac_vec,rd_test)
pred_svm<-ifelse(pred_for_svm>0.5,1,0)

#Accuracy obtained from svm is 58.61%
mean(pred_svm==gndr_submission$Survived)

confusion_mat_ran<-table(pred_svm,gndr_submission$Survived)

