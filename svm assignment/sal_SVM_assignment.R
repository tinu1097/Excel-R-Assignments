##...problem statement-1##
##import train and test data ## 
sal_train<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SVM assignments/SalaryData_Train(1) (1).csv")
sal_test<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SVM assignments/SalaryData_Test(1) (3).csv")

library(kernlab) ##using for SVM#
library(ggplot2) ##using for attractive visualization#
library(caret) ##using for confusionmatrix#

#...........EDA.........#
attach(sal_train)  ##direct access coloumn#
attach(sal_test) ##direct access coloumn#
str(sal_train)
str(sal_test)

#........convert int to factor...#
sal_train$educationno<-as.factor(sal_train$educationno)
sal_test$educationno<-as.factor(sal_test$educationno)

#.....visualization........#
ggplot(data = sal_train,aes(x=sal_train$Salary,y=sal_train$age,fill = sal_train$Salary)) + 
  geom_boxplot() + 
  ggtitle("Box Plot")

ggplot(data = sal_train,aes(x=sal_train$Salary,y=sal_train$hoursperweek,fill = sal_train$Salary)) + 
  geom_boxplot() + 
  ggtitle("Box Plot")

ggplot(data = sal_train,aes(x=sal_train$Salary,y=sal_train$education,fill = sal_train$Salary)) + 
  geom_boxplot() + 
  ggtitle("Box Plot")


#Density Plot 

ggplot(data=sal_train,aes(x = sal_train$age, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


#.....model Building.........#
v_model<-ksvm(Salary~.,data = sal_train,kernel = "vanilladot" )
predict_v<-predict(v_model,newdata = sal_test)

table_v<-table(predict_v,sal_test$Salary)
prop_v<-prop.table(table_v)

#.Accuracy...#
mean(predict_v==sal_test$Salary) #0.85#

#improve Accuracy#
#using different kernel#
rf_model<-ksvm(Salary~.,data = sal_train,kernel = "rbfdot")
predict_rf<-predict(rf_model,newdata = sal_test)
#Accuracy#
mean(predict_rf==sal_test$Salary) #0.852#

#kernel=ploydot#
ploy_model<-ksvm(Salary~.,data = sal_train, kernel = "polydot")
predict_poly<-predict(ploy_model,newdata = sal_test)
#Accuracy#
mean(predict_poly==sal_test$Salary) #0.85#

#kernel=besseldot#
besell_model<-ksvm(Salary~.,data = sal_train, kernel = "besseldot")
predict_bessell<-predict(besell_model,newdata = sal_test)
#Accuracy#
mean(predict_poly==sal_test$Salary) ##0.84##


#conclusion: i can go vanniladot because its gives me highest accuracy and remaining model gives me less accuracy ##









