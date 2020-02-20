##problem statement-2##

cocacola<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/CocaCola_Sales_Rawdata.csv")
View(Airline)
plot(cocacola$Sales,type = "o")

Q1 <-  ifelse(grepl("Q1",cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cocacola$Quarter),'1','0')
# So creating 4 dummy variables 
coca_data<-cbind(cocacola,Q1,Q2,Q3,Q4)
View(coca_data)
colnames(coca_data)
coca_data["t"]<- 1:42
View(coca_data)
coca_data["log_Sales"]<-log(coca_data["Sales"])
coca_data["t_square"]<-coca_data["t"]*coca_data["t"]
attach(coca_data)

#..........spliting data.....#
train_coca<-coca_data[1:32,]
test_coca<-coca_data[33:42,]


########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train_coca)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_coca))
View(linear_pred)
rmse_linear<-sqrt(mean((test_coca$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #752.9


######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train_coca)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test_coca))
rmse_expo<-sqrt(mean((test_coca$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 590.33

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train_coca)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test_coca))
rmse_Quad<-sqrt(mean((test_coca$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 457.7


######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train_coca)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test_coca,interval='predict'))
rmse_sea_add<-sqrt(mean((test_coca$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #1850.46

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train_coca)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test_coca))
rmse_Add_sea_Linear<-sqrt(mean((test_coca$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 673.44

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train_coca)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test_coca))
rmse_Add_sea_Quad<-sqrt(mean((test_coca$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #277.35

################## Multiplicative Seasonality Linear trend ##########################

multi_linear_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data=train_coca)
summary(multi_linear_sea_model) 
multi_linear_sea_pred<-data.frame(predict(multi_linear_sea_model,newdata=test_coca,interval='predict'))
rmse_multi_linear_sea<-sqrt(mean((test_coca$Sales-exp(multi_linear_sea_pred$fit))^2,na.rm = T))
rmse_multi_linear_sea #448.86

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_linear_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_linear_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


#additive seasonality with quadratic has least RMSE##
final_coca<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=coca_data)
summary(final_coca)
final_predict <- predict(final_coca)
res_final<- coca_data$Sales - final_predict
acf(res_final,lag.max = 10) 
#as we can see graph only one  lag values cross +2 so we will build arima model#
coca_arima <- arima(res_final, order=c(1,0,0))
str(coca_arima )
View(data.frame(res=res_final,newresid=coca_arima $residuals))

#as we can see we got improved value with less error #
acf(coca_arima $residuals,lag.max = 10)
pred_res<- predict(arima(coca_arima$residuals ,order=c(1,0,0)),n.ahead = 4)
str(pred_res)


#in arima also show one value cross -2SE so again we re use arima model$##
#p,d,q,values#
coca_arima <- arima(res_final, order=c(2,0,2))
str(coca_arima )
View(data.frame(res=res_final,newresid=coca_arima $residuals))

#as we can see we got improved value with less error #

acf(coca_arima $residuals,lag.max = 10)
pred_res<- predict(arima(coca_arima$residuals ,order=c(1,0,0)),n.ahead = 4)
str(pred_res)

coca_data["residuals"] <- res_final
coca_data["final_prediction"] <- final_predict

##final plot## 
plot(coca_data$final_prediction,type = "o")













