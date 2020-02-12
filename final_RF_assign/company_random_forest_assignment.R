##problem statement-1##
company<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Decision tree assignments/Company_Data.csv")

library(randomForest) ##use for random forest#
library(gmodels) ##use for cross table#
library(caret) #use for confusion matrix#

#.........EDA.............#
sum(is.na(company)) ##checking null values#

#we convert numerical values into categories#
sales<-ifelse(company$Sales<9.5, "NO","YES")
company<-data.frame(company,sales)
company<-company[-1]
str(company)
#........OR.............#
#sales<-company$Sales 
#cut(sales,2)
#cut(sales,2,labels = c("NO","YES"))
#company$sal_cat<-cut(sales,2,labels = c("NO","YES"))
#str(company)

#........spiliting..........#

com_train<-company[1:300,]
com_test<-company[301:400,]

prop.table(table(com_train$sales))
prop.table(table(com_test$sales))

#Building a random forest model on Training data set#
sales_train<-randomForest(sales~.,data = com_train,na.action = na.roughfix,importance=TRUE)
##training Accuracy#
pred_train<-predict(sales_train,com_train)

#confusion matrix 
confusionMatrix(pred_train,com_train$sales)

mean(com_train$sales==predict(sales_train,com_train))

##predicting on test data#
pred_test<-predict(sales_train,newdata = com_test)
table(pred_test,com_test$sales)
#cross table for comparison#
CrossTable(com_test$sales,pred_test)
#confusion matrix#
confusionMatrix(pred_test,com_test$sales)

mean(pred_test==com_test$sales) ##accuracy=0.85
 

# Visualization 
plot(sales_train,lwd=1.5)
legend("topright", colnames(sales_train$err.rate),col=1:4,cex=0.8,fill=1:4)






























