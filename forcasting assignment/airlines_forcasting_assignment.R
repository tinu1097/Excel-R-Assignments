##problem statement-1##
Airline<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/Airlines+Data.csv")
View(Airline)
plot(Airline$Passengers,type = "o")

# So creating 12 dummy variables 
air<-data.frame(outer(rep(month.abb,length = 96),month.abb,"==")+ 0)
View(air)
colnames(air)<-month.abb

airlines_data<-cbind(Airline,air)
airlines_data["t"] <- 1:96

airlines_data["log_passenger"]<-log(airlines_data["Passengers"])
airlines_data["t_square"]<-airlines_data["t"]*airlines_data["t"]
attach(airlines_data)

#..........spliting data.....#
train_air<-airlines_data[1:80,]
test_air<-airlines_data[81:96,]

########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train_air)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_air))
View(linear_pred)
rmse_linear<-sqrt(mean((test_air$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 47.54


######################### Exponential #################################

expo_model<-lm(log_passenger~t,data=train_air)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test_air))
rmse_expo<-sqrt(mean((test_air$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 43.47

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train_air)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test_air))
rmse_Quad<-sqrt(mean((test_air$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 43.65

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_air)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test_air,interval='predict'))
rmse_sea_add<-sqrt(mean((test_air$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #129.266

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_air)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test_air))
rmse_Add_sea_Linear<-sqrt(mean((test_air$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 33.50

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data= train_air)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test_air))
rmse_Add_sea_Quad<-sqrt(mean((test_air$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #23.91

################## Multiplicative Seasonality Linear trend ##########################

multi_linear_sea_model<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_air)
summary(multi_linear_sea_model) 
multi_linear_sea_pred<-data.frame(predict(multi_linear_sea_model,newdata=test_air,interval='predict'))
rmse_multi_linear_sea<-sqrt(mean((test_air$Passengers-exp(multi_linear_sea_pred$fit))^2,na.rm = T))
rmse_multi_linear_sea # 9.469

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_linear_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_linear_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#multiplicative seasonality with linear has least RMSE##

final_air<-lm(log_passenger~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airlines_data)
summary(final_air)
final_predict <- exp(predict(final_air))
res_final<- airlines_data$Passengers - final_predict
acf(res_final,lag.max = 10) 
#as we can see graph max. of lag values cross +2 so we will build arima model#

airline_arima <- arima(res_final, order=c(1,0,0))
str(airline_arima)
View(data.frame(res=res_final,newresid=airline_arima$residuals))
#as we can see we got improved value with less error #
acf(airline_arima$residuals,lag.max = 10)
pred_res<- predict(arima(airline_arima$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)

airlines_data["residuals"] <- res_final
airlines_data["final_prediction"] <- final_predict
#final plot after prediction#
plot(airlines_data$final_predict,type = "o")

