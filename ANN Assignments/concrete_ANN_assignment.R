
##.....problem statement-3...##
##predicting strength##

concrete<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/ANN assignments/concrete.csv")
library(neuralnet)  ##in ANN using for regression#
library(gmodels) ##using for cross table
library(caret) ##using for confusion matrix##

###EDA(Exploratery Data Analysis)
attach(concrete) ##accessing direct column#
str(concrete)
summary(concrete)

#.....normalize the data ..........#
concre_norm<-scale(concrete)
concre_norm<-as.data.frame(concre_norm)

#...........splitting the data into train and test..........#
con_train<-concre_norm[1:800,]
con_test<-concre_norm[801:1030,]

#....ModelBuilding.........#
strength_model<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = con_train)
str(strength_model)
plot(strength_model)  ##plotting## 
model_result<-compute(strength_model,con_test[,1:8]) ##predicting##
str(model_result)
predict_str<-model_result$net.result

##Accuracy##
cor(predict_str,con_test$strength)  ##0.80##
plot(predict_str,con_test$strength) ##scatter plot##

#..........for improve Model Accuracy.........#
strength_model1<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = con_train,hidden = 4)
str(strength_model1)
plot(strength_model1) ##plotting## 
model_result1<-compute(strength_model1,con_test[,1:8]) ##predicting##
str(model_result1)
predict_str1<-model_result1$net.result

##Accuracy##
cor(predict_str1,con_test$strength)  ##0.93##
plot(predict_str1,con_test$strength) ##scatter plot##

