##########predicting sell###########3
#####problem statmenet-2###########
Computer_Data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Multi linear regression/Computer_Data.csv")

attach(Computer_Data)       #can use column without mention file name#
library(plyr)            #invoking the packages#
library(GGally)         ##for representation##
library(caTools)        #spliting data#  
library(car)            #for plotting##


####EDA(Exploratory Data Analysis)####
sum(is.na(Computer_Data))   #checking missing values##
summary(Computer_Data)
Computer_Data<-Computer_Data[-1]      #remove first coloumn##

#.......replace yes=0,no=1........#
str(Computer_Data$cd)
Computer_Data$cd <- as.character(Computer_Data$cd)
Computer_Data$cd[Computer_Data$cd=='no'] <- 1
Computer_Data$cd[Computer_Data$cd=='yes'] <- 0

Computer_Data$multi <- as.character(Computer_Data$multi)
Computer_Data$multi[Computer_Data$multi=='no'] <- 1
Computer_Data$multi[Computer_Data$multi=='yes'] <- 0

Computer_Data$premium <- as.character(Computer_Data$premium)
Computer_Data$premium[Computer_Data$premium=='no'] <- 1
Computer_Data$premium[Computer_Data$premium=='yes'] <- 0

#...........character convert into factor............ 
Computer_Data$cd <- factor(Computer_Data$cd)
Computer_Data$multi <- factor(Computer_Data$multi)
Computer_Data$premium<- factor(Computer_Data$premium)

### 4 moment of business decision ####
summary(Computer_Data)
plyr::colwise(mean)(Computer_Data[-c(6,7,8)])
plyr::colwise(median)(Computer_Data[-c(6,7,8)])
plyr::colwise(var)(Computer_Data[-c(6,7,8)])
plyr::colwise(sd)(Computer_Data[-c(6,7,8)])

pairs(Computer_Data,main="scatterplot")   #scatter plot for checking relationship or not#
cor(Computer_Data[-c(6,7,8)])   ##Correlation Coefficents 'r'#

#..........plotting........#
pairs(Computer_Data,main="scatterplot") 
boxplot(Computer_Data,main="boxplot")

ggpairs(Computer_Data)   ##we can also see correlation coefficient and scatter plot together##

par(mfrow=c(2,2))  
##Histrogram##
for (i in 1:10){
  if(i==6:8){
    print("no plot for factor")
  }else{
    name<-names(Computer_Data)[i]
    hist(Computer_Data[,i],xlab = name,main = name)
  }
}

Computer_DataA<-Computer_Data[-c(6,7,8)]   ##remove factor coloumns####

library(corpcor)
cor2pcor(cor(Computer_DataA))   ##partial correlation matrix-pur correlation b/w the variables##

#...........Splitting the data into train and test.........#
library(caTools)
split<-(sample.split(Computer_DataA$price,SplitRatio=0.70))
table(split)
sell_train <- subset(Computer_DataA,split==TRUE)
sell_test <- subset(Computer_DataA,split==FALSE)

#####Model-Building##########
computer_model1<-lm(price~speed+hd+ram+screen+ads+trend,data = sell_train)
summary(computer_model1)    ##check R-squared value and '***','**','*',denotes dependent variables significant otherwise insignificant##                   

# Multicollinearity check
#check relationship each independent and dependent variable##
##prediction based on only speed ##
model_speed<-lm(price~speed)
summary(model_speed)     #R-squared=0.090,and  speed is significant##

##prediction based on only hd  ##
model_hd<-lm(price~hd) 
summary(model_hd)   ##R-squared=0.1851, hd is  significant#

##prediction based on only ram###
model_ram<-lm(price~ram)
summary(model_ram)     #R-squared=0.3878,and Marketing.Spend  is significant

##prediction based on only screen###
model_screen<-lm(price~screen)
summary(model_screen)     #R-squared=0.08764,and screen is significant

##prediction based on only ads###
model_ads<-lm(price~ads)
summary(model_ads)     #R-squared=0.002975,and ads is significant

##prediction based on only trend###
model_trend<-lm(price~trend)
summary(model_trend)     #R-squared=0.03999,and trend is significant

## we see that all variables are significant#####
#check  Variance Inflation factor(VIF) to check collinearity b/n variables value
library(car)
vif(computer_model1) 
#all variable VIF value less then 10 .so no relationship each oter##

## Added Variable plot to check correlation b/n variables and o/p variable
library(car)
avPlots(computer_model1,id.n=5,id.cex=0.7)  #by avplots we can see that screen & ads are less contributing to price(sell)##

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
library(car)
influence.measures(computer_model1)
influenceIndexPlot(computer_model1,id.n=5) # index plots for infuence measures
influencePlot(computer_model1,id.n=5) # A user friendly representation of the above

# Regression after deleting the 1441th,4283th,994th Observations
computer_model2<-lm(price~speed+hd+ram+screen+ads+trend,data = sell_train[-c(1701,4074)])
summary(computer_model2)

#after delete influence value all variables are significant#

#..........Model Testing............#

train_vs_test<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("S.N.","modelfunc","r","rsq","RMSE"))
#....model building for Train data ..........#
#...............train model-1......................
train_model_1<-lm(price~speed+hd+ram+screen+ads+trend,data = sell_train)
summary(train_model_1)
error_model_1<-train_model_1$residuals

## RMSE(root mean square error)
sqrt(mean(error_model_1^2))  ##312.5538

train_vs_test<-rbind(train_vs_test,data.frame (S.N.=1,modelfunc="lm(price~speed+hd+ram+screen+ads+trend,data = sell_train)",
                                                         r=sqrt(summary(train_model_1)$r.squared),
                                                         rsq=summary(train_model_1)$r.squared,RMSE=sqrt(mean(error_model_1^2))))

#............test model -1...............................

test_model_1<-lm(price~speed+hd+ram+screen+ads+trend,data = sell_test)

summary(test_model_1)
error_model_1<-test_model_1$residuals

## RMSE(root mean square error)
sqrt(mean(error_model_1^2))  ##308.4366

train_vs_test<-rbind(train_vs_test,data.frame (S.N.=1,modelfunc="lm(price~speed+hd+ram+screen+ads+trend,data = sell_test)",
                                                         r=sqrt(summary(test_model_1)$r.squared),
                                                         rsq=summary(test_model_1)$r.squared,RMSE=sqrt(mean(error_model_1^2))))

########Train model-2##########

train_model_2<-lm((1/log(price))~speed+I(speed^2)+hd+I(1/hd)+ram+screen+ads+trend,data = sell_train)
summary(train_model_2)
predict_model_2<- exp(train_model_2$fitted.values)^-1 #predicted 
error_model_2<-sell_train$price - predict_model_2

## RMSE(root mean square error)
sqrt(mean(error_model_2^2)) 

train_vs_test<-rbind(train_vs_test,data.frame (S.N.=1,modelfunc="lm((1/log(price)~speed+I(speed^2)+hd+I(1/hd)+ram+screen+ads+trend,data = sell_train)",
                                               r=sqrt(summary(train_model_2)$r.squared),
                                               rsq=summary(train_model_2)$r.squared,RMSE=sqrt(mean(error_model_2^2))))



#............test model-2...............................
test_model_2<-lm((1/log(price))~speed+I(speed^2)+hd+I(1/hd)+ram+screen+ads+trend,data = sell_test)
summary(test_model_2)
predict_model_2<-exp(test_model_2$fitted.values)^-1 #predicted 
error_model_2<- sell_test$price - predict_model_2
## RMSE(root mean square error)
sqrt(mean(error_model_2^2))  

train_vs_test<-rbind(train_vs_test,data.frame (S.N.=1,modelfunc="lm((1/log(price)~speed+I(speed^2)+hd+I(1/hd)+ram+screen+ads+trend,data = sell_test)",
                                                         r=sqrt(summary(test_model_2)$r.squared),
                                                         rsq=summary(test_model_2)$r.squared,RMSE=sqrt(mean(error_model_2^2))))

##.........train model-3...........##
train_model_3<-lm(sqrt(price)~log(speed)+hd+I(1/hd)+ram+I(ram^4)+screen+I(1/screen^4)+log(ads)+trend,data = sell_train)
summary(train_model_3)
predict_model_3<-(train_model_3$fitted.values)^2 #predicted 
error_model_3<-sell_train$price - predict_model_3

## RMSE(root mean square error)
sqrt(mean(error_model_3^2))  ##294.5255

train_vs_test<-rbind(train_vs_test,data.frame (S.N.=1,modelfunc="lm(sqrt(price)~log(speed)+hd+I(1/hd)+ram+I(ram^4)+screen+I(1/screen^4)+log(ads)+trend,data = sell_train)",
                                               r=sqrt(summary(train_model_3)$r.squared),
                                               rsq=summary(train_model_3)$r.squared,RMSE=sqrt(mean(error_model_3^2))))

######test model-3#####################

test_model_3<-lm(sqrt(price)~log(speed)+hd+I(1/hd)+ram+I(ram^4)+screen+I(1/screen^4)+log(ads)+trend,data = sell_test)
summary(test_model_3)
predict_model_3<-(test_model_3$fitted.values)^2 #predicted 
error_model_3<-sell_test$price - predict_model_3

## RMSE(root mean square error)
sqrt(mean(error_model_3^2))  ##291.7277



#.................final model.........................
#we see that in train_vs_test table train_model-3 and test_model_3 both models have nearly same r r-square and RMSE value so we finalize the model-3##

final_model<-lm(sqrt(price)~log(speed)+hd+I(1/hd)+ram+I(ram^4)+screen+I(1/screen^4)+log(ads)+trend,data = Computer_Data)

summary(final_model)
predicted_final<-(final_model$fitted.values)^2
error_final<- Computer_Data$price - predicted_final
confint(final_model,level =0.95)

## RMSE(root mean square error)
sqrt(mean(error_final^2))  ##293.8487

# ggplot for adding regression line for data set##
library(ggplot2)  ##invoking the ggplot2 pacakage##
ggplot(data = Computer_DataA, aes(x =speed+hd+ram+screen+ads+trend, y = price)) + 
  geom_point(color='black') +
  geom_line(color='red',data = Computer_DataA, aes(x=speed+hd+ram+screen+ads+trend, y=predicted_final))














