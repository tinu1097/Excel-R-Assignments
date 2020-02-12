##problem statement-2##
forestfir<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SVM assignments/forestfires.csv")
library(kernlab) ##using for SVM#
library(caret) ##using for confusionmatrix#

###EDA(Exploratery Data Analysis)
attach(forestfir) ##accessing direct column#
str(forestfir)
forestfir<-forestfir[,-c(1,2)]  ##exculde both column#

##Normalize Data##
forest_norm <- scale(forestfir[,-29])
forest_norm <-cbind(forest_norm, forestfir$size_category)
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


#.....model Building.........#
v_model<-ksvm(size_category~.,data = forest_train,kernel = "vanilladot" )
predict_v<-predict(v_model,newdata = forest_test)

table_v<-table(predict_v,forest_test$size_category)
prop_v<-prop.table(table_v)

#.Accuracy...#
mean(predict_v==forest_test$size_category) #0.84#

#improve Accuracy#
#using different kernel#
rf_model<-ksvm(size_category~.,data = forest_train,kernel = "rbfdot")
predict_rf<-predict(rf_model,newdata = forest_test)
#Accuracy#
mean(predict_rf==forest_test$size_category) #0.70#

#kernel=ploydot#
ploy_model<-ksvm(size_category~.,data = forest_train, kernel = "polydot")
predict_poly<-predict(ploy_model,newdata = forest_test)
#Accuracy#
mean(predict_poly==forest_test$size_category) #0.8443#

#kernel=besseldot#
besell_model<-ksvm(size_category~.,data = forest_train, kernel = "besseldot")
predict_bessell<-predict(besell_model,newdata = forest_test)
#Accuracy#
mean(predict_poly==forest_test$size_category) ##0.84#


##conclusion:since rfbdot model gives less accuracy so we dont use this  and remaining model gives good accuracy so we can finalize any model except rfbdot model#






