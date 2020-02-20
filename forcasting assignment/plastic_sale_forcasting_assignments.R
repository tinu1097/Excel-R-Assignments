##problem statement-3#
##read data##
Plastic_sale<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/PlasticSales.csv")
attach(Plastic_sale)
View(Plastic_sale)
plot(Plastic_sale$Sales,type = "o")

# So creating 12 dummy variables 
Plas_sale<-data.frame(outer(rep(month.abb,length = 60),month.abb,"==")+ 0)
View(Plas_sale)
colnames(Plas_sale)<-month.abb
attach(Plas_sale)
Plas_sale<-cbind(Plastic_sale,Plas_sale)
Plas_sale["t"] <- 1:60
Plas_sale["log_sale"]<-log(Plas_sale["Sales"])

Plas_sale["t_square"]<-Plas_sale["t"]*Plas_sale["t"]
attach(Plas_sale)

#..........spliting data.....#
train_sale<-Plas_sale[1:44,]
test_sale<-Plas_sale[45:60,,]


########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train_sale)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_sale))
View(linear_pred)
rmse_linear<-sqrt(mean((test_sale$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 238.12


######################### Exponential #################################

expo_model<-lm(log_sale~t,data=train_sale)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test_sale))
rmse_expo<-sqrt(mean((test_sale$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 242.8

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train_sale)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test_sale))
rmse_Quad<-sqrt(mean((test_sale$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #238.51 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_sale)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test_sale,interval='predict'))
rmse_sea_add<-sqrt(mean((test_sale$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #259.92

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_sale)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test_sale))
rmse_Add_sea_Linear<-sqrt(mean((test_sale$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 113.91

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data= train_sale)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test_sale))
rmse_Add_sea_Quad<-sqrt(mean((test_sale$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #136.79

################## Multiplicative Seasonality Linear trend ##########################

multi_linear_sea_model<-lm(log_sale~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_sale)
summary(multi_linear_sea_model) 
multi_linear_sea_pred<-data.frame(predict(multi_linear_sea_model,newdata=test_sale,interval='predict'))
rmse_multi_linear_sea<-sqrt(mean((test_sale$Sales-exp(multi_linear_sea_pred$fit))^2,na.rm = T))
rmse_multi_linear_sea # 128.19

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_Add_sea_Linear","rmse_multi_linear_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_Linear,rmse_multi_linear_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#additive seasonality with linear has least RMSE##

final_sale<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Plas_sale)
summary(final_sale)
final_predict <- predict(final_sale)
res_final<-Plas_sale$Sales- final_predict
windows()
acf(res_final,lag.max = 10)

#as we can see graph mst of lag values cross +2 so we will build arima model#

sale_arima <- arima(res_final, order=c(1,0,0))
str(sale_arima)
View(data.frame(res=res_final,newresid=sale_arima$residuals))

#as we can see we got improved value with less error #

acf(sale_arima$residuals,lag.max = 10)
pred_res<- predict(arima(sale_arima$residuals,order=c(1,0,0)),n.ahead = 12)
str(pred_res)

Plas_sale["residuals"] <- res_final
Plas_sale["final_prediction"] <- final_predict

##final plot##

plot(Plas_sale$final_prediction,type = "o")
















