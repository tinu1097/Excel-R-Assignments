##predict weight gained using calori consumed###3
##problem statement-1###

calories_consumed<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION/calories_consumed.csv")
View(calories_consumed)

attach(calories_consumed)   #direct accesing coloumnes 
str(calories_consumed)     #data structur 
library(lattice)      #invoking package for more graphical representation ##
library(ggplot2)     ## ggplot for adding regression line for data set ##

#EDA-Exploratory Data Analysis 
Weight<-Weight.gained..grams.
calori<-Calories.Consumed

##4 moment business decision

mean.data<-c("mean_weight"=mean(Weight),"mean_calori"=mean(calori))   # mean 
median.data<-c("weight_medi"=median(Weight),"calori_medi"=median(calori)) #median 
var.data<-c("var_weight"=var(Weight),"var_calori"=var(calori)) #variance
sd.data<-c("sd_weight"=sd(Weight),"sd_calori"=sd(calori))  #standard deviation
c(mean.data,median.data,var.data,sd.data)
plot(Calories.Consumed,Weight) #scatter plot for checking relationship or not
cor(Calories.Consumed,Weight) #Correlation Coefficent 'r'#0.9469

##Checking Missing Values 
is.na(calories_consumed)

sum(is.na(calories_consumed))
summary(calories_consumed)

library(moments)#invoking this package 
skewness(Calories.Consumed)#cheking skewness 
skewness(Weight.gained..grams.)#checking skewness

hist(Calories.Consumed)
hist(Weight.gained..grams.)

boxplot(calories_consumed$Calories.Consumed,horizontal = T)
boxplot(calories_consumed$Weight.gained..grams.,col="red",horizontal = T)

##Graphical Representation
dotplot(calories_consumed$Calories.Consumed,main="plot of calories consume")
dotplot(calories_consumed$Weight.gained..grams.,main=" plot of weight gained")

qqnorm(calories_consumed$Calories.Consumed,main ="calories.consume")
qqline(calories_consumed$Calories.Consumed,mmain="calories.consume")

qqnorm(calories_consumed$Weight.gained..grams.,main ="weight.gained")
qqline(calories_consumed$Weight.gained..grams.,main="weight.gained")

##complete scatter plot for weight gained and calori consumed##
plot(calories_consumed$Calories.Consumed,calories_consumed$Weight.gained..grams.,main = "scatter plot",col="Dodgerblue4",col.main="Dodgerblue4",xlab="calories consume",ylab="weight gained",pch=15)

##model building###
##model-1###
Weight<-Weight.gained..grams.  ##for easy access of coloumn##
calori<-Calories.Consumed

weight_model1 <-lm(Weight~calori)  #lm(y~x)simple linear regression model
predict_weight_model1<-weight_model1$fitted.values  #predicted weight 

error_weight_model1<-weight_model1$residuals    ##error
mean(weight_model1$residuals)
confint(weight_model1,level =0.95)    
predict(weight_model1,interval="predict")
summary(weight_model1) #check p value,intercept,coefficients,R-squared value=0.8968

## RMSE(root mean square error)
sqrt(mean(error_weight_model1^2))


model_compare<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("id","modelfunc","r","rsq","RMSE"))

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(Weight~calori)",
                                               r=sqrt(summary(weight_model1)$r.squared),
                                               rsq=summary(weight_model1)$r.squared,RMSE=sqrt(mean(error_weight_model1^2))))


# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = calories_consumed, aes(x = calori, y = Weight)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=calori, y=predict_weight_model1))

##model building###
##model-2###
Weight<-Weight.gained..grams.  ##for easy access of coloumn##
calori<-Calories.Consumed

weight_model2 <-lm(Weight~sqrt(calori)+I(calori^4))  #lm(y~x)simple linear regression model
predict_weight_model2<-weight_model2$fitted.values  #predicted weight 

error_weight_model2<-weight_model2$residuals   ##error(actual value - predicted value )
mean(weight_model2$residuals)
confint(weight_model2,level =0.95)    
predict(weight_model2,interval="predict")
summary(weight_model2) #check p value,intercept,coefficients,R-squared value=0.9362

## RMSE(root mean square error)
sqrt(mean(error_weight_model2^2)) ##81.244#



model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(Weight~sqrt(calori)+I(calori^4))",
                                               r=sqrt(summary(weight_model2)$r.squared),
                                               rsq=summary(weight_model2)$r.squared,RMSE=sqrt(mean(error_weight_model2^2))))


# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = calories_consumed, aes(x = calori, y = Weight)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=calori, y=predict_weight_model2))


##Model-3###
##We can use other transformation for improve R-squared value and reduce RMSE#####
# Logrithamic+resiprocal##

weight_model3 <-lm(Weight~log(calori)+I(1/calori))  #lm(y~x) 
summary(weight_model3) # pvalues ,intercept,coefficients,rsquared-0.9729
predict_weight_model3<- reg_model_3$fitted.values ##predicte weight
plot(predict_weight_model3)
error_weight_model3 <- reg_model_3$residuals ##Error 
predict(weight_model3,interval="predict")
## RMSE(root mean square error)
sqrt(mean(error_weight_model3^2)) ##52.96   


model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(Weight~log(calori)+I(1/calori))",
                                               r=sqrt(summary(weight_model_3)$r.squared),
                                               rsq=summary(weight_model_3)$r.squared,RMSE=sqrt(mean(error_weight_model3^2))))

model_compare
##conclusion:from above table we can see that first two model gives less R-squared value and Higher RMSE value .
#but our final model(weight_model3) gives improved R-squared value and reduced RMSE value.


predictedweight<-predict_weight_model3 ##predicted value for our final model



# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = calories_consumed, aes(x = log(calori)+I(calori^2) , y = Weight)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=log(calori)+I(calori^2), y=predictedweight))



calories_consumed$Predicted_weight<-predict_weight_model3  ##append new value in data set

getwd()
setwd("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION")

##write csv in R
write.csv(calories_consumed,file = "calories_consumed_predicted.csv")











         








