##required all libraries invoking##
library(forecast)
library(fpp)
library(smooth)
cocacola<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/CocaCola_Sales_Rawdata.csv")
View(cocacola)
library(tseries)
# Converting data into time series object
coca<-ts(cocacola$Sales,frequency = 4,start=c(86),start=c(1986,1),end=c(1996,2))
start(coca)
end(coca)
cycle(coca)
frequency(coca)
summary(coca)
View(coca)
# dividing entire data into training and testing data 
train<-coca[1:38]
test<-coca[39:42] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)

# Plotting time series data
plot(coca) # Visualization shows that it has level, trend, seasonality => Additive seasonality

#### USING HoltWinters function ################
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
coca_l<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
cocaa_pred<-data.frame(predict(coca_l,n.ahead=4))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(coca_l,h=4))
cocaa_mape<-MAPE(cocaa_pred$fit,test)*100


# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
coca_l_t<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = F)
cocaab_pred<-data.frame(predict(coca_l_t,n.ahead = 4))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(coca_l_t,h=4))
cocaab_mape<-MAPE(cocaab_pred$fit,test)*100



# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
coca_l_t_s<-HoltWinters(train,alpha = 0.2,beta = 0.15,gamma = 0.05)
cocaabg_pred<-data.frame(predict(coca_l_t_s,n.ahead = 4))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(coca_l_t_s,h=4))
cocaabg_mape<-MAPE(cocaabg_pred$fit,test)*100


# With optimum values 
coca_na<-HoltWinters(train,beta = F,gamma = F)
cocana_pred<-data.frame(predict(coca_na,n.ahead = 4))
plot(forecast(coca_na,h=4))
cocana_mape<-MAPE(cocana_pred$fit,test)*100

coca_nab<-HoltWinters(train,gamma=F)
cocanab_pred<-data.frame(predict(coca_nab,n.ahead=4))
plot(forecast(coca_nab,h=4))
cocanab_mape<-MAPE(cocanab_pred$fit,test)*100

coca_nabg<-HoltWinters(train)
cocanabg_pred<-data.frame(predict(coca_nabg,n.ahead =4))
plot(forecast(coca_nabg,h=4))
cocanabg_mape<-MAPE(cocanabg_pred$fit,test)*100
############################## STOP HERE ###############################

df_mape<-data.frame(c("cocaa_mape","cocaab_mape","cocaabg_mape","cocana_mape","cocanab_mape","cocanabg_mape"),c(cocaa_mape,cocaab_mape,cocaabg_mape,cocana_mape,cocanab_mape,cocanabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

##moving average#
my_model<-sma(train)
my_pred<-data.frame(predict(my_model,h=4))
plot(forecast(my_model))
my_mape<-MAPE(my_pred$Point.Forecast,test)*100
