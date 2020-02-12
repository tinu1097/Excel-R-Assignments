##problem statement-1##
company<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Decision tree assignments/Company_Data.csv")
library(C50) ##use for decision tree#
library(gmodels) ##use for cross table#
#library(party)##also use for decision tree#
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

com_train<-company[1:200,]
com_test<-company[201:400,]

prop.table(table(com_train$sales))
prop.table(table(com_test$sales))

#Building Model on Training data set#
sales_train<-C5.0(com_train[,-11],com_train$sales)
plot(sales_train)

#op_tree = ctree(sales ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                #+ Age + Education + Urban + US, data = com_train)
                
#plot(op_tree) ## ctree gives me less rule but it gives me less accuracy so i go with c5.0# 

##training Accuracy#
pred_train<-predict(sales_train,com_train)
#confusion matrix 
confusionMatrix(pred_train,com_train$sales)

mean(pred_train==com_test$sales) #0.975#
table(com_test$sales)
##predicting on test data#
pred_test<-predict(sales_train,newdata = com_test)
table(pred_test,com_test$sales)
#cross table for comparison#
CrossTable(com_test$sales,pred_test)
mean(pred_test==com_test$sales) ##accuracy=0.775#
 




















