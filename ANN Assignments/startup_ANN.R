#....problem statement-2.......#
##predicting profit##
startup<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/ANN assignments/50_Startups.csv")
library(neuralnet)  ##in ANN using for regression#
library(gmodels) ##using for cross table
library(caret) ##using for confusion matrix##

#............EDA........#
attach(forestfires) ##accessing direct column#
str(startup)
summary(startup)
#........covert categorical value into numeric......#
startup$State<-as.numeric(as.factor(startup$State))
str(startup)
#.........nomalizing whole data..........
startup_norm<-scale(startup)                                    
startup_norm<-as.data.frame(startup_norm)

#...........splitting the data into train and test..........#
startup_train<-startup_norm[1:35,]
startup_test<-startup_norm[35:50,]

#.........model building.........#
model_startup<- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup_train)
str(model_startup) ##structure#
plot(model_startup) ##plotting##

model_result <- compute(model_startup,startup_test[,1:4]) ##predicting##
str(model_result) #structure#
predicted_profit<- model_result$net.result

#Accuracy##
cor(predicted_profit,startup_test$Profit)  ##0.83##
plot(predicted_profit,startup_test$Profit) ##scatter plot##

