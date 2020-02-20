##Prediction model for  Churn_out_rate ###
##problem statement-3##

emp_data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION/emp_data.csv")
View(emp_data)
sh<-Salary_hike
cor<-Churn_out_rate
attach(emp_data)     #can use column without mention file name 
str(emp_data)
library(lattice)
summary(emp_data)

#EDA-Exploratory Data Analysis 
sh<-Salary_hike   #easy of access for coloumn
cor<-Churn_out_rate
mean_data<-c("mean_sh"=mean(sh),"mean_cor"=mean(cor))   # mean 
median_data<-c("sh_medi"=median(sh),"cor_medi"=median(cor)) #median 
var_data<-c("var_sh"=var(sh),"var_cor"=var(cor)) #variance
sd_data<-c("sd_sh"=sd(sh),"sd_cor"=sd(cor))  #standard deviation
c(mean_data,median_data,var_data,sd_data)

plot(sh,cor) #scatter plot for checking relationship or not
cor(cor,sh) #Correlation Coefficent 'r',r= -0.91 ("strong negative")

##Checking Missing Values in the data set
is.na(emp_data)
sum(is.na(emp_data)) #sum of NA values
summary(emp_data)

sh<-Salary_hike
cor<-Churn_out_rate
library(moments)#invoking this package
skewness(sh)#cheking skewness 
skewness(cor)#checking skewness

hist(sh) #histrogram 
hist(cor) #histrogram

boxplot(emp_data$Salary_hike,col="blue",horizontal = T) 
boxplot(emp_data$Churn_out_rate,col="red",horizontal = T)

##Graphical Representation
dotplot(emp_data$Salary_hike,main="do plot of salary_hike")
dotplot(emp_data$Churn_out_rate,main="do plot of churn_out_rate")

#for salary_hike#
qqnorm(emp_data$Salary_hike,main ="salary_hike")
qqline(emp_data$Salary_hike,main ="salary_hike")

#for churn_out_rate#
qqnorm(emp_data$Churn_out_rate)
qqline(emp_data$Churn_out_rate)

##complete scatter plot 
plot(emp_data$Salary_hike,emp_data$Churn_out_rate,main = "scatter plot",col="black",col.main="red",xlab="salary_hike",ylab="churn_out_rate",pch=15)

## model Building ##
##model-1##
sh<-Salary_hike
cor<-Churn_out_rate

#simple linear regression model
model_churn1 <-lm(cor~sh) #lm(y~x)
predict_model1<-model_churn1$fitted.values  #predicted churn_out_rate

error_model1<-model_churn1$residuals    ##error(actual value - predicted value )
mean(model_churn1$residuals)
confint(model_churn1,level =0.95)
predict(model_churn1,interval="predict")
summary(model_churn1) #check p value,intercept,coefficients,R-squared=0.8312

## RMSE(root mean square error)
sqrt(mean(error_model1^2))  ##3.9975

model_compare<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("id","modelfunc","r","rsq","RMSE"))

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(cor~sh)",
                                               r=sqrt(summary(model_churn1)$r.squared),
                                               rsq=summary(model_churn1)$r.squared,RMSE=sqrt(mean(error_model1^2))))

# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = emp_data, aes(x = sh, y = cor)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=sh, y=predict_model1))

##model-2###
##We can use other transformation for improve R-squared value#####
#square+logarithmic model#
sh<-Salary_hike
cor<-Churn_out_rate

#squar+logarithmic model#
model_churn2 <-lm(sqrt(cor)~log(sh)) #lm(y~x)
predict_model2<-(model_churn2$fitted.values)^2  #predicted churn_out_rate

error_model2<- emp_data$Churn_out_rate - predict_model2    ##error(actual value - predicted value )
mean(error_model2)
confint(model_churn2,level =0.95)
predict(model_churn2,interval = "predict")^2
summary(model_churn2) #check p value,intercept,coefficients,R-squared=0.8695

## RMSE(root mean square error)
sqrt(mean(error_model2^2))  ##3.53

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(sqrt(cor)~log(sh))",
                                               r=sqrt(summary(model_churn2)$r.squared),
                                               rsq=summary(model_churn2)$r.squared,RMSE=sqrt(mean(error_model2^2))))

# ggplot for adding regresion line for data
sh<-Salary_hike
cor<-Churn_out_rate
library(ggplot2)
ggplot(data = emp_data, aes(x = sh, y = cor)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=sh, y=predict_model2))

##model-3##
##again we use other transformation (in transformation no thumb rule ) to improve R squared value ##
sh<-Salary_hike
cor<-Churn_out_rate

#logarithmic+recliprocal mode#
model_churn3 <-lm(cor~log(sh)+I(1/sh)) #lm(y~x)
predict_model3<-(model_churn3$fitted.values)  #predicted churn_out_rate

error_model3<-model_churn3$residuals    ##error
mean(error_model3)
confint(model_churn3,level =0.95)
predict(model_churn3,interval = "predict")
summary(model_churn3) #check p value,intercept,coefficients,R-squared=0.9784
## RMSE(root mean square error)
sqrt(mean(error_model3^2))  ##1.430

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(cor~log(sh)+I(1/sh))",
                                               r=sqrt(summary(model_churn3)$r.squared),
                                               rsq=summary(model_churn3)$r.squared,RMSE=sqrt(mean(error_model3^2))))
model_compare
##conclusion :in above table we can see that in first two model low R-squared value and high RMSE value.
#and our final model(model_churn3) gives improved R-squared value and reduced RMSE value. 

predictedchurn<-predict_model3 ##predicted value for our final model



# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = emp_data, aes(x = sh, y = cor)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=sh, y=predictedchurn))



emp_data$Pred_churn_out_time<-predictedchurn ##append new value in data set

getwd()
setwd("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION")


##write csv in R
write.csv(emp_data,file ="churn_out_pred_time.csv")




















