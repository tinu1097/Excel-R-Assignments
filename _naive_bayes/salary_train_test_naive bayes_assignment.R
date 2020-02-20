#......problem statement-1....#
Train_sal<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/navie bayes assignments/SalaryData_Train.csv")
Test_sal<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/navie bayes assignments/SalaryData_Test.csv")
attach(Train_sal) #for direct access coloumn#
attach(Test_sal) #for direct access coloumn#
library(ggplot2) #for visualization,plotting graph#
#......EDA...........#
#checking null values#
sum((is.na(Train_sal)))
sum(is.na(Test_sal))
str(Train_sal)
table(Train_sal$Salary)

str(Test_sal)
table(Test_sal$Salary)
#Visualization 
# Plot and ggplot on  train data set#
ggplot(data=Train_sal,aes(x=Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


ggplot(data=Train_sal,aes(x=Salary, y = educationno, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=Train_sal,aes(x=Salary, y = hoursperweek, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


#.......Model Building...............#
library(e1071) #for naive bayes#
#......navie Bayes Model.........#
n_model <- naiveBayes(Train_sal$Salary ~ .,data=Train_sal)
n_model$levels


Model_pred <- predict(n_model,Test_sal)
Model_pred[1:400]
#confusiom matrix#
confusionMatrix(Model_pred,Test_sal$Salary)

library(gmodels) #invoke the library for cross tavle#
CrossTable(Model_pred,Test_sal$Salary,
           prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))

#Accuracy#
mean(Model_pred==Test_sal$Salary)




