
glass<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/KNN Assignment/glass.csv")

####EDA(Exploratory Data Analysis)####
sum(is.na(glass))  #checking null values##
table(glass$Type) #table of types 
prop.table(table(glass$Type)*100)  ##proportion pecentage value##
round(prop.table(table(glass$Type))*100,1) 

str(glass)  ##structer of the data set##
#KNN mostly deal with numerical data so we will only convert type to factor which is our class.
glass$Type<-as.factor(glass$Type)
levels(glass$Type)
glass_data<-factor(glass$Type,levels = c("1","2","3","4","5","7"),labels=c("1","2","3","5","6","7"))
str(glass)


##normalize the data and adding z-label coloumn##
glass_norm<-scale(glass[,1:9]) 

#....spiliting the daat train and test......#
glass_train <- glass_norm[1:160,]
glass_test <- glass_norm[161:214,]

#Get labels for training and test datasets
glass_train_labels <- glass[1:160,10]
glass_test_labels <- glass[161:214,10]

##model building##
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model##
##predecting calss with 5 neighboures ##
glass_pred_train <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=6)
#train accuracy##
mean(glass_pred_train==glass_train_labels)

glass_pred_test <- knn(train=glass_test,test=glass_test,cl=glass_test_labels,k=6)
##test accuracy
mean(glass_pred_test==glass_test_labels)
##for best value of k###
glass_train_acc <- NULL
glass_test_acc<- NULL
for (i in seq(2,50,2))
{
  glass_pred_train <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i) 
  glass_train_acc<- c(glass_train_acc,mean(glass_pred_train==glass_train_labels))
  glass_pred_test <- knn(train=glass_test,test=glass_test,cl=glass_test_labels,k=i)
  glass_test_acc<-c(glass_test_acc,mean((glass_pred_test==glass_test_labels)))
}



##plotting##

accuracy <- data.frame(list(glass_train_acc=glass_train_acc, glass_test_acc=glass_test_acc,k_value=seq(2,50,2)))
# Plotting 2 different graphs on same co-ordinate axis
library(ggplot2) #for the visualization##
ggplot(accuracy,aes(x=accuracy$k_value))+
  geom_line(aes(y=accuracy$glass_train_acc,colour="train_acc"),lwd=1)+
  geom_line(aes(y=accuracy$glass_test_acc,colour="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#conclusion:k=24 then test accuracy starts to a fall down##





