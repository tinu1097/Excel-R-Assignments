##required all libraries invoking##
library(forecast)
library(fpp)
library(smooth)
plastic_sale<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/PlasticSales.csv")
View(plastic_sale)
library(tseries)
# Converting data into time series object
plastic_data<-ts(plastic_sale$Sales,frequency = 12,start=c(1949,1),end=c(1953,12))
start(plastic_data)
end(plastic_data)
cycle(plastic_data)
frequency(plastic_data)
summary(plastic_data)
View(plastic_data)
attach(plastic_data)

#Checking Trend and sesonality.
plot(plastic_data)
abline(reg=lm(plastic_data~time(plastic_data)))

# dividing entire data into training and testing data 
train<-plastic_data[1:48]
test<-plastic_data[49:60] #monthly data is given so we will predict next 12 month...
# Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)

# Plotting time series data
plot(plastic_data) # Visualization shows that it has level, trend, seasonality => Additive seasonality


#### USING HoltWinters function ################
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
plas_hw<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
summary(plas_hw)
plas_pred<-data.frame(predict(plas_hw,n.ahead=12))
View(plas_pred)
summary(plas_pred)
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(plas_hw,h=12))
plas_mape<-MAPE(plas_pred$fit,test)*100

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
plas_hw1<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
plas_pred1<-data.frame(predict(plas_hw1,n.ahead = 12))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(plas_hw1,h=12))
plas_mape1<-MAPE(plas_pred1$fit,test)*100

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
plas_hw2<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
plas_pred2<-data.frame(predict(plas_hw2,n.ahead = 12))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(plas_hw2,h=12))
plas_mape2<-MAPE(plas_pred2$fit,test)*100


# With optimum values 
plas_opt<-HoltWinters(train,beta = F,gamma = F)
opt_pred<-data.frame(predict(plas_opt,n.ahead = 12))
plot(forecast(plas_opt,h=12))
plas_mape3<-MAPE(opt_pred$fit,test)*100

plas_opt1<-HoltWinters(train,gamma=F)
opt_pred1<-data.frame(predict(plas_opt1,n.ahead=12))
plot(forecast(plas_opt1,h=12))
plas_mape4<-MAPE(opt_pred1$fit,test)*100

plas_opt2<-HoltWinters(train)
opt_pred2<-data.frame(predict(plas_opt1,n.ahead =12))
plot(forecast(plas_opt1,h=12))
plas_mape5<-MAPE(opt_pred2$fit,test)*100
############################## STOP HERE ###############################

df_mape<-data.frame(c("plas_mape","plas_mape1","plas_mape2","plas_mape3","plas_mape4","plas_mape5"),c(plas_mape,plas_mape1,plas_mape2,plas_mape3,plas_mape4,plas_mape5))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

##moving average#
my_model<-sma(train)
my_pred<-data.frame(predict(my_model,h=12))
plot(forecast(my_model))
my_mape<-MAPE(my_pred$Point.Forecast,test)*100
