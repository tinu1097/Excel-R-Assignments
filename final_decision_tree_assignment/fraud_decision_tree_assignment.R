#....problem statement-2.......#
library(readr)
Fraud<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Decision tree assignments/Fraud_check (1).csv")
View(Fraud)
library(C50) ##use for decision tree#
library(gmodels) ##use for cross table#
#library(party)##also use for decision tree#
library(caret) #use for confusion matrix#

#.........EDA.............#
sum(is.na(Fraud)) ##checking null values#

#we convert numerical values into categories#
tex_cat<-ifelse(Fraud$Taxable.Income <=30000, "Risky","Good")
Fraud<-data.frame(Fraud,tex_cat)
str(Fraud)
Fraud<-Fraud[-3]


#........spiliting..........#

Fraud_train<-Fraud[1:450,]
Fraud_test<-Fraud[451:600,]

prop.table(table(Fraud_train$tex_cat))
prop.table(table(Fraud_test$tex_cat))

#Building Model on Training data set#
tex_train<-C5.0(Fraud_train[,-6],Fraud_train$tex_cat)
plot(tex_train)

#op_tree = ctree(tex_cat ~ Undergrad +Marital.Status  + City.Population + Work.Experience+Urban , data = Fraud_train)
                

#plot(op_tree)  

##training Accuracy#
pred_train<-predict(tex_train,Fraud_train)
#confusion matrix 
confusionMatrix(pred_train,Fraud_train$tex_cat)

mean(pred_train==Fraud_train$tex_cat) #0.77#

##predicting on test data#
pred_test<-predict(tex_train,newdata = Fraud_test)
table(pred_test,Fraud_test$tex_cat)
#cross table for comparison#
CrossTable(Fraud_test$tex_cat,pred_test)
mean(pred_test==Fraud_test$tex_cat) ##accuracy=0.775#



















































