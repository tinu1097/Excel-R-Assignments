#### Predict delivery time by sorting time###
#problem statement 2 #

delivery_time<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION/delivery_time.csv")
dl_tm<-delivery_time
View(delivery_time)
dt<-Delivery.Time  #easy of access for coloumn##
st<-Sorting.Time
attach(dl_tm)     #can use column without mention file name 
str(dl_tm)
library(lattice)   
library(ggplot2)     ## ggplot for adding regression line for data set ##

#EDA-Exploratory Data Analysis 
plot(st,dt) #scatter plot for checking relationship or not
cor(dt,st) #Correlation Coefficent 'r=0.82599

##4 moments of business decision###
mean.data<-c("mean_dt"=mean(dt),"mean_st"=mean(st))   # mean 
median.data<-c("dt_medi"=median(dt),"st_medi"=median(st)) #median 
var.data<-c("var_dt"=var(dt),"var_st"=var(st)) #variance
sd.data<-c("sd_dt"=sd(dt),"sd_st"=sd(st))  #standard deviation
c(mean.data,median.data,var.data,sd.data)

##Checking Missing Values 
is.na(dl_tm)

sum(is.na(dl_tm))
summary(dl_tm)

library(moments)#invoking this package 
skewness(st)#cheking skewness 
skewness(dt)#checking skewness

hist(st) #histrogram 
hist(dt) #histrogram

boxplot(dl_tm$Sorting.Time,col="blue",horizontal = T) 
boxplot(dl_tm$Delivery.Time,col="red",horizontal = T)

##Graphical Representation
dotplot(dl_tm$Sorting.Time,main="plot of sorting time")
dotplot(dl_tm$Delivery.Time,main=" plot of delivary time")

qqnorm(dl_tm$Sorting.Time,main ="sorting time")
qqline(dl_tm$Sorting.Time,main ="sorting time")

qqnorm(dl_tm$Delivery.Time,main="delivary time")
qqline(dl_tm$Delivery.Time,main="delivary time")

##complete scatter plot 
plot(dl_tm$Sorting.Time,dl_tm$Delivery.Time,main = "scatter plot",col="black",col.main="red",xlab="sorting_time",ylab="delivary_time",pch=15)

##model building##
##model-1##
dt<-Delivery.Time
st<-Sorting.Time

#simple linear regression model
model_del1 <-lm(dt~st) #lm(y~x)
predict_model_del1<-model_del1$fitted.values  #predicted weight 

error_model_del1<-model_del1$residuals    ##error(actual value - predicted value )
mean(model_del1$residuals)
confint(model_del1,level =0.95)
predict(model_del1,interval="predict")
summary(model_del1) #check p value,intercept,coefficients,R-squared=0.6823

## RMSE(root mean square error)
sqrt(mean(error_model_del1^2))  ##2.791

model_compare<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("id","modelfunc","r","rsq","RMSE"))

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(dt~st)",
                                               r=sqrt(summary(model_del1)$r.squared),
                                               rsq=summary(model_del1)$r.squared,RMSE=sqrt(mean(error_model_del1^2))))

# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = dl_tm, aes(x = st, y = dt)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dl_tm, aes(x=st, y=predict_model_del1))

##model-2###
##We can use other transformation for improve R-squared value.so i use logrithmic model#####
##logarithmic##
dt<-Delivery.Time
st<-Sorting.Time
#simple linear regression model
model_del2<-lm(dt~log10(st)) #lm(y~x)

predict_model_del2<-model_del2$fitted.values  #predicted weight 

error_model_del2<- dl_tm$Delivery.Time - predict_model_del2   ##error(actual value - predicted value )
mean(error_model_del2)
confint(model_del2,level =0.95)
predict(model_del2,interval="predict")
summary(model_del2) #check p value,intercept,coefficients,R-squared=0.6954

## RMSE(root mean square error)
sqrt(mean(error_model_del2^2)) ##2.73

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(dt~log10(st))",
                                               r=sqrt(summary(model_del2)$r.squared),
                                               rsq=summary(model_del2)$r.squared,RMSE=sqrt(mean(error_model_del2^2))))


# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = dl_tm, aes(x = st, y = dt)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dl_tm, aes(x=st, y=predict_model_del2))


##model-3##
##again we use other transformation to improve R squared value ##
# Logrithamic Model+polynomial/quadratic
dt<-Delivery.Time
st<-Sorting.Time

# Logrithamic Model+polynomial/quadratic
model_del3 <-lm(dt~log(st)+I(st^2))  #lm(y~x) 
summary(model_del3)     # p-values ,intercept,coefficients ,R-squared=0.6988
predict_delivery_time<- model_del3$fitted.values ##predicte delivery time 
predict_delivery_time
plot(predict_delivery_time)
error_delivery_time <- dl_tm$Delivery.Time - predict_delivery_time  ##Error 
error_delivery_time
predict(model_del3,interval="predict")
## RMSE(root mean square error)
sqrt(mean(error_delivery_time^2)) ##2.718

model_compare<-rbind(model_compare,data.frame (id=1,modelfunc="lm(dt~log(st)+I(st^2))",
                                               r=sqrt(summary(model_del3)$r.squared),
                                              rsq=summary(model_del3)$r.squared,RMSE=sqrt(mean(error_delivery_time^2))))

model_compare
#conclusion : we can see that in our final model(model_del_3) R-squared not good but its moderate and RMSE is low. 


# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = dl_tm, aes(x = st, y = dt)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dl_tm, aes(x=st, y=predict_delivery_time))

dl_tm$Pred_delivery_time<-predict_delivery_time ##append new value in data set 

getwd()
dl_tm<-delivery_time
setwd("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/SIMPLE LINEAR REGRESSION")


##write csv in R
write.csv(delivery_time,file ="delivery_pred.csv")






