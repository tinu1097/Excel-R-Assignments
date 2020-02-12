#.....problem statement-1......#
zoo<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/KNN Assignment/Zoo.csv")
zoo<-zoo[,-1]

####EDA(Exploratory Data Analysis)####
sum(is.na(zoo))  #checking null values##
table(zoo$type) #table of types 
prop.table(table(zoo$type)*100)  ##proportion pecentage value##
round(prop.table(table(zoo$type))*100,1) 

str(zoo)  ##structer of the data set##
#KNN mostly deal with numerical data so we will only convert type to factor which is our class.
zoo$type<-as.factor(zoo$type)
levels(zoo$type)
zoo_data<-factor(zoo$type,levels = c("1","2","3","4","5","6","7"),labels=c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean"))
str(zoo)

##normalize the data and adding z-label coloumn##
zoo_norm <- scale(zoo[,1:16])  
 
#....spiliting the daat train and test......#
zoo_train <- zoo_norm[1:75,]
zoo_test <- zoo_norm[76:101,]

#Get labels for training and test datasets
zoo_train_labels <- zoo[1:75,17]
zoo_test_labels <- zoo[76:101,17]

##model building##
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model##
##predecting calss with 5 neighboures ##
zoo_pred_train <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_labels,k=4)
#train accuracy##
mean(zoo_pred_train==zoo_train_labels)

zoo_pred_test <- knn(train=zoo_test,test=zoo_test,cl=zoo_test_labels,k=4)
##test accuracy
mean(zoo_pred_test==zoo_test_labels)
##for best value of k###
zoo_train_acc <- NULL
zoo_test_acc<- NULL
for (i in seq(2,20,2))
{
  zoo_pred_train <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_labels,k=i) 
  zoo_train_acc<- c(zoo_train_acc,mean(zoo_pred_train==zoo_train_labels))
  zoo_pred_test <- knn(train=zoo_test,test=zoo_test,cl=zoo_test_labels,k=i)
  zoo_test_acc<-c(zoo_test_acc,mean((zoo_pred_test==zoo_test_labels)))
}



##plotting##

accuracy <- data.frame(list(zoo_train_acc=zoo_train_acc, zoo_test_acc=zoo_test_acc,k_value=seq(2,20,2)))
# Plotting 2 different graphs on same co-ordinate axis
library(ggplot2) #for the visualization##
ggplot(accuracy,aes(x=accuracy$k_value))+
  geom_line(aes(y=accuracy$zoo_train_acc,colour="train_acc"),lwd=1)+
  geom_line(aes(y=accuracy$zoo_test_acc,colour="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#conclusion: k=12 then test accuracy starts to a fall down###






