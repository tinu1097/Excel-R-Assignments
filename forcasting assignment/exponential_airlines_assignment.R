
##required all libraries invoking##
library(forecast)
library(fpp)
library(smooth)
airlines_data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/Airlines+Data.csv")

View(cocacola)
library(tseries)
# Converting data into time series object
airlines<-ts(airlines_data$Passengers,frequency = 12,start=c(1995,1),end=c(2002,12))
start(airlines)
end(airlines)
cycle(airlines)
frequency(airlines)
summary(airlines)
View(airlines)

#Checking Trend and sesonality.
plot(airlines)
abline(reg=lm(airlines~time(airlines)))

# dividing entire data into training and testing data 
train<-airlines[1:84]
test<-airlines[85:96] #monthly data is given so we will predict next 12 month...
# Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)

# Plotting time series data
plot(airlines) # Visualization shows that it has level, trend, seasonality => Additive seasonality


#### USING HoltWinters function ################
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
air_hw<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
summary(air_hw)
air_pred<-data.frame(predict(air_hw,n.ahead=12))
View(air_pred)
summary(air_pred)
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(air_hw,h=12))
air_mape<-MAPE(air_pred$fit,test)*100

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
air_hw1<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
air_pred1<-data.frame(predict(air_hw1,n.ahead = 12))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(air_hw1,h=12))
air_mape1<-MAPE(air_pred1$fit,test)*100

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
air_hw2<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
air_pred2<-data.frame(predict(air_hw2,n.ahead = 12))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(air_hw2,h=12))
air_mape2<-MAPE(air_pred2$fit,test)*100


# With optimum values 
air_opt<-HoltWinters(train,beta = F,gamma = F)
opt_pred<-data.frame(predict(air_opt,n.ahead = 12))
plot(forecast(air_opt,h=12))
air_mape3<-MAPE(opt_pred$fit,test)*100

air_opt1<-HoltWinters(train,gamma=F)
opt_pred1<-data.frame(predict(air_opt1,n.ahead=12))
plot(forecast(air_opt1,h=12))
air_mape4<-MAPE(opt_pred1$fit,test)*100

air_opt2<-HoltWinters(train)
opt_pred2<-data.frame(predict(air_opt1,n.ahead =12))
plot(forecast(air_opt1,h=12))
air_mape5<-MAPE(opt_pred2$fit,test)*100
############################## STOP HERE ###############################

df_mape<-data.frame(c("air_mape","air_mape1","air_mape2","air_mape3","air_mape4","air_mape5"),c(air_mape,air_mape1,air_mape2,air_mape3,air_mape4,air_mape5))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

##moving average#
my_model<-sma(train)
my_pred<-data.frame(predict(my_model,h=12))
plot(forecast(my_model))
my_mape<-MAPE(my_pred$Point.Forecast,test)*100

















