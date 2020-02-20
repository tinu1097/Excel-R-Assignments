##prediction model for salary_hike###
##problem statement-4##
Salary_Data_1_<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION/Salary_Data (1).csv")
salary_hike<-Salary_Data_1_  
View(salary_hike)

attach(salary_hike)     #can use column without mention file name 
str(salary_hike)
library(ggplot2) ### ggplot for adding regression line for data set##
library(lattice) #invoking the package for more grphical representation##
summary(salary_hike)

#EDA-Exploratory Data Analysis 
yex<-YearsExperience
sal<-Salary
mean.data<-c("mean_yex"=mean(yex),"mean_sal"=mean(sal))   # mean 
median.data<-c("yex_medi"=median(yex),"sal_medi"=median(sal)) #median 
var.data<-c("var_yex"=var(yex),"var_sal"=var(sal)) #variance
sd.data<-c("sd_yex"=sd(yex),"sd_sal"=sd(sal))  #standard deviation

pairs(startup_data) #scatter plot for checking relationship or not#
cor() #Correlation Coefficent 'r',r= 0.97 ("strong positive")


##Checking Missing Values in the data set
is.na(salary_hike)
sum(is.na(salary_hike)) #sum of NA values

library(moments)#invoking this package 
yex<-YearsExperience 
sal<-Salary

skewness(yex)#cheking skewness 
skewness(sal)#checking skewness
hist(yex) #histrogram 
hist(sal) #histrogram

boxplot(salary_hike$YearsExperience,col="blue",horizontal = T) 
boxplot(salary_hike$Salary,col="red",horizontal = T)

##Graphical Representation
dotplot(salary_hike$YearsExperience,main="plot of years of experience")
dotplot(salary_hike$Salary,main="plot of salary")

#for years of experience#
qqnorm(salary_hike$YearsExperience,main ="yearsexperience")
qqline(salary_hike$YearsExperience,main ="yearsexperience")

#for salary#
qqnorm(salary_hike$Salary,main="salary")
qqline(salary_hike$Salary,main="salary")

##complete scatter plot for salary and years of exprience## 
plot(salary_hike$YearsExperience,salary_hike$Salary,main = "scatter plot",col="black",col.main="red",xlab="yearsexperience",ylab="salary",pch=15)

####Model Building######
##model-1##
yex<-YearsExperience
sal<-Salary

#simple linear regression model
model_sal<-lm(sal~yex) #lm(y~x)
predict_mode_sal1<-model_sal$fitted.values  #predicted salary in model-1
error_model_sal1<-model_sal$residuals    ##error(actual value - predicted value )
mean(model_sal$residuals)
confint(model_sal,level =0.95)
predict(model_sal,interval="predict")
summary(model_sal) #check p value,intercept,coefficients,R-squared=0.957

## RMSE(root mean square error)
sqrt(mean(error_model_sal1^2))  ##5592.044


model_compare<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("id","modelfunc","r","rsq","RMSE"))
                                                                                    
model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(sal~yex)",
                  r=sqrt(summary(model_sal)$r.squared),
                  rsq=summary(model_sal)$r.squared,RMSE=sqrt(mean(error_model_sal1^2))))
                                                      

# ggplot for adding regression line for data set##
library(ggplot2)  ##invoking the ggplot2 pacakage##
ggplot(data = salary_hike, aes(x = yex, y = sal)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_hike, aes(x=yex, y=predict_mode_sal1))

########model-2###########

##We can use other transformation for improve R-squared value and reduce RMSE#####

#square+logarithmic model#
yex<-YearsExperience
sal<-Salary

#squar+logarithmic model#
model_sal2 <-lm(sal~yex+log10(yex)+I(yex)) #lm(y~x)
predict_model_sal2<-(model_sal2$fitted.values) #predicted salary in model-2
error_model_sal2<-salary_hike$Salary - predict_model_sal2 ##error(actual value - predicted value )
mean(error_model_sal2)
confint(model_sal2,level =0.95)
predict(model_sal2,interval = "predict")

summary(model_sal2) #check p value,intercept,coefficients,R-squared=0.9579

## RMSE(root mean square error)
sqrt(mean(error_model_sal2^2))  ##5533.673


model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(sal~yex+log10(yex)+I(yex))",
                                               r=sqrt(summary(model_sal2)$r.squared),
                                               rsq=summary(model_sal2)$r.squared,RMSE=sqrt(mean(error_model_sal2^2))))



# ggplot for adding regresion line for data
ggplot(data = salary_hike, aes(x = yex, y = sal)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_hike, aes(x=yex, y=predict_model_sal2))

##again we use other transformation (in transformation no thumb rule ) to improve R squared value and reduce RMSE ##
######model-3########
#square+logarithmic model#
yex<-YearsExperience
sal<-Salary

#logarithmic+quadratic model#
model_sal3 <-lm(log(sal)~yex+I(yex*yex)) #lm(y~x)
predict_model_sal3<-exp((model_sal3$fitted.values))  #predicted salary in model-3
error_model_sal3<-salary_hike$Salary - predict_model_sal3   ##error(actual value - predicted value )
mean(error_model_sal3) ##mean of error##

summary(model_sal3) #check p value,intercept,coefficients,R-squared=0.9486

## RMSE(root mean square error)
sqrt(mean(error_model_sal3^2))  ##5391.082

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(log(sal)~yex+I(yex*yex))",
                                               r=sqrt(summary(model_sal3)$r.squared),
                                               rsq=summary(model_sal3)$r.squared,RMSE=sqrt(mean(error_model_sal3^2))))



# ggplot for adding regresion line for data
ggplot(data = salary_hike, aes(x = yex, y = sal)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_hike, aes(x=yex, y=predict_model_sal3))



##model-4####
yex<-YearsExperience
sal<-Salary

#squar+logarithmic model#
model_sal4 <-lm(sal~yex+log10(yex)+I(yex*yex)) #lm(y~x)
predict_model_sal4<-(model_sal4$fitted.values) #predicted churn_out_rate
##error(actual value - predicted value )
error_model_sal4<-salary_hike$Salary - predict_model_sal4
mean(error_model_sal4)
confint(model_sal4,level =0.95)
predict(model_sal4,interval = "predict")

summary(model_sal4) #check p value,intercept,coefficients,R-squared=0.9624

## RMSE(root mean square error)
sqrt(mean(error_model_sal4^2))  ##5226.073

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(sal~yex+log10(yex)+I(yex*yex))",
                                               r=sqrt(summary(model_sal4)$r.squared),
                                               rsq=summary(model_sal4)$r.squared,RMSE=sqrt(mean(error_model_sal4^2))))


model_compare
#conclusion: in above table we can see that first three model huge difference between actual and predictive value and  low R-squared ,high RMSE.
#but in our final model less difference between actual and predictive value and improved R-squared,reduce RMSE.

predictedsalary<-predict_model_sal4   ###predicted value for our final model

# ggplot for adding regresion line for data set##
library(ggplot2)
ggplot(data = salary_hike, aes(x = yex, y = sal)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_hike, aes(x=yex, y=predictedsalary))

salary_hike$Predicted_salary<-predictedsalary ##append new value in data set

getwd()
setwd("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION")


##write csv in R
write.csv(salary_hike,file ="predicted_salary.csv")












