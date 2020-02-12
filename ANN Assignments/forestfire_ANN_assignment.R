###problem statement -1###
##predicting burned area##

forestfires<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/ANN assignments/forestfires.csv")
library(neuralnet)  ##in ANN using for classification#
library(gmodels) ##using for cross table
library(caret) ##using for confusion matrix##

###EDA(Exploratery Data Analysis)
attach(forestfires) ##accessing direct column#
str(forestfires)
forestfires<-forestfires[,-c(1,2)]  ##exculde both column#

##Normalize Data##
forest_norm <- scale(forestfires[,-29])
forest_norm <-cbind(forest_norm, forestfires$size_category)
colnames(forest_norm)[29]<-"size_category"
forest_norm <- as.data.frame(forest_norm)

###convert categorical data into numeric values##
forest_norm$size_category[forest_norm$size_category==2] <- 0
forest_norm$size_category[forest_norm$size_category==1] <- 1
forest_norm$size_category <- as.factor(forest_norm$size_category)

#...........splitting the data into train and test..........#
forest_train<-forest_norm[1:350,]
forest_test<-forest_norm[351:517,]
prop.table(table(forest_train$size_category))
prop.table(table(forest_test$size_category))

#.........model building.........#
model_size <- neuralnet(size_category~.,hidden=2,act.fct = "logistic",
                        linear.output = FALSE,data = forest_train)
str(model_size) ##structure#
plot(model_size) ##plotting##
model_result <- compute(model_size,forest_test[,1:28]) ##predicting##
str(model_result) #structure#
predict_size<- model_result$net.result
predict_size <- as.data.frame(predict_size) ##create Data frame##
predict_size <- ifelse(predict_size$V1>0.5,0,1) 
predict_prob<- as.data.frame(predict_size) ##create Data frame##

forest_test<-cbind(forest_test,predict_prob) ##add column in forest_test##

##Table###
table_pred <- table(predict_prob$predict_size,forest_test[,29])

##cross table##
CrossTable(predict_prob$predict_size,forest_test[,29])

###Accuracy##
mean(predict_prob$predict_size==forest_test$size_category) #0.93#


