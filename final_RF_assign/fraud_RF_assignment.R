#....problem statement-2.......#
library(readr)
Fraud<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Decision tree assignments/Fraud_check (1).csv")
View(Fraud)
library(randomForest) ##use for random forest#
library(gmodels) ##use for cross table#
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

#Building a random forest model on Training data set#
tex_forest<-randomForest(tex_cat~.,data = Fraud_train,na.action = na.roughfix,importance=TRUE)

##training Accuracy#
pred_train<-tex_forest$predict
#confusion matrix 
confusionMatrix(pred_train,Fraud_train$tex_cat)

mean(Fraud_train$tex_cat==predict(tex_forest,Fraud_train)) #0.91#

##predicting on test data#
pred_test<-predict(tex_forest,newdata = Fraud_test)
table(pred_test,Fraud_test$tex_cat)
#cross table for comparison#
CrossTable(Fraud_test$tex_cat,pred_test)

#confusion matrix#
confusionMatrix(pred_test,Fraud_test$tex_cat)

mean(pred_test==Fraud_test$tex_cat) ##accuracy=0.83#

# Visualization 
plot(tex_forest,lwd=1.5)
legend("topright", colnames(tex_forest$err.rate),col=1:4,cex=0.8,fill=1:4)




















































