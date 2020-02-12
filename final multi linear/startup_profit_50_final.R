######problem statement-1####
####predicting model for 50-startuo data #####


startup_data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Multi linear regression/50_Startups (1).csv")

attach(startup_data)            #can use column without mention file name 
summary(startup_data)
library(plyr)            #invoking the packages#
library(GGally)         ##for representation##
library(fastDummies)   ##for create duumy variables ##
library(caTools)       #spliting data#   
library(car)          #for plotting##
library(corpcor)      #for checking collinearity##

####EDA(Exploratory Data Analysis)####
startup_data[startup_data==0]   #find 0 values in the data set#
startup_data[startup_data==0] <- NA  #replace 0 with NA#
summary(startup_data)

boxplot(startup_data$R.D.Spend,col="blue",horizontal = T)          #there are no outlier#
boxplot(startup_data$Marketing.Spend,col="red",horizontal = T)     #there are no outlier#


startup_data$R.D.Spend[is.na(startup_data$R.D.Spend)] <- 76793        #we replace NA value with R.D. spend coloumn mean because we have seen above boxplot there are no outlier#
startup_data$Marketing.Spend[is.na(startup_data$Marketing.Spend)] <- 224495  ##above statement#

### 4 moment of business decision ####
summary(startup_data)
plyr::colwise(mean)(startup_data[,-4])
plyr::colwise(median)(startup_data[,-4])
plyr::colwise(var)(startup_data[,-4])
plyr::colwise(sd)(startup_data[,-4])

pairs(startup_data,main="scatter plot")       #scatter plot for checking relationship or not#
cor(startup_data[,-4])    ##Correlation Coefficents 'r'#

##............ Create Dummy Veriable..............##
levels(State)
?dummy_cols
dummy_col<-dummy_cols(startup_data$State)
colnames(dummy_col)=c("S.N.","california","Florida","New York")
startup_dataA<- cbind(startup_data,dummy_col[,2:4])
startup_dataA<-startup_dataA[,-4]

####checking collinearity#############
library(corpcor)
cor2pcor(cor(startup_dataA))   ##partial correlation matrix-pur correlation b/w the variables##

#..........plotting........#
pairs(startup_data,main="scatter plot")       #scatter plot for checking relationship or not#
boxplot(startup_dataA,main="boxplot")

ggpairs(startup_dataA)   ##we can also see correlation coefficient and scatter plot together##

par(mfrow=c(2,1))  
##Histrogram##
for (i in 1:5){
  if(i==4){
    print("no plot for text data")
  }else{
    name<-names(startup_data)[i]
    hist(startup_data[,i],xlab = name,main = name)
    }
}



#...........Splitting the data into train and test.........#
library(caTools)
split<-(sample.split(startup_dataA$Marketing.Spend,SplitRatio=0.60))
table(split)
startup_train <- subset(startup_dataA,split==TRUE)
startup_test <- subset(startup_dataA,split==FALSE)

#####Model-Building##########

startup_model1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup_train)
summary(startup_model1)    ##check R-squared value and '***','**','*',denotes dependent variables significant otherwise insignificant##                   

# Multicollinearity check
#check relationship each independent and dependent variable##
##prediction based on only R.D.Spend ##
model_R.D.<-lm(Profit~R.D.Spend)
summary(model_R.D.)     #R-squared=0.9465,and R.D.Spend is significant##
        
##prediction based on only Administration  ##
model_Administration<-lm(Profit~Administration) 
summary(model_Administration)   ##R-squared=0.04029,but Administration is not significant#

##prediction based on only Marketing.Spend###
model_Marketing<-lm(Profit~Marketing.Spend)
summary(model_Marketing)     #R-squared=0.5592,and Marketing.Spend  is significant
 
#we see that administration is not significant and R.D spend and marketing_spend are significant# 
 #check  Variance Inflation factor(VIF) to check collinearity b/n variables value
library(car)
vif(startup_model1) 
 #all variable VIF value less then 10 .so no relationship each oter##

## Added Variable plot to check correlation b/n variables and o/p variable
library(car)
avPlots(startup_model1,id.n=3)  #by avplots we can see that administration is not contributing to profit##
help(avplot())
# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
library(car)
influence.measures(startup_model1)
influenceIndexPlot(startup_model1,id.n=3) # index plots for infuence measures
influencePlot(startup_model1,id.n=3) # A user friendly representation of the above

# Regression after deleting the 47th, 49th & 50th Observations
startup_model2<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startup_train[-c(47,49,50)])
summary(startup_model2)

#after delete influence value administration does not significant#.
#so we exclude administration coloumn ##

startup_modelB<-lm(Profit~R.D.Spend+Marketing.Spend,data=startup_train[-c(47,49,50)])
summary(startup_modelB)
influenceIndexPlot(startup_modelB,id.n=3)

#..........Model Testing............#

train_compare_test<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("S.N.","modelfunc","r","rsq","RMSE"))
#....model building for Train data ..........#

#...............train model-1......................
train_model_1<-lm(Profit~R.D.Spend+Marketing.Spend,data = startup_train)
summary(train_model_1)
error_model_1<-train_model_1$residuals

## RMSE(root mean square error)
sqrt(mean(error_model_1^2))  ##17882.01


train_compare_test<-rbind(train_compare_test,data.frame (S.N.=1,modelfunc="lm(Profit~R.D.Spend+Marketing.Spend,data = startup_train)",
                                               r=sqrt(summary(train_model_1)$r.squared),
                                               rsq=summary(train_model_1)$r.squared,RMSE=sqrt(mean(error_model_1^2))))
#............test model -1...............................

test_model_1<-lm(Profit~R.D.Spend+Marketing.Spend,data = startup_test)
summary(test_model_1)
error_model_1<-test_model_1$residuals
## RMSE(root mean square error)
sqrt(mean(error_model_1^2))  ##17557.27


train_compare_test<-rbind(train_compare_test,data.frame (S.N.=1,modelfunc="lm(Profit~R.D.Spend+Marketing.Spend,data = startup_test)",
                                                         r=sqrt(summary(test_model_1)$r.squared),
                                                         rsq=summary(test_model_1)$r.squared,RMSE=sqrt(mean(error_model_1^2))))

########Train model-2##########

train_model_2<-lm((Profit^2)~R.D.Spend+log(R.D.Spend)+Marketing.Spend+I(Marketing.Spend^4),data = startup_train )
summary(train_model_2)
predict_model_3<-sqrt(train_model_2$fitted.values) #predicted 
error_model_2<-startup_train$Profit - predict_model_3

## RMSE(root mean square error)
sqrt(mean(error_model_2^2))  ##17212.98

train_compare_test<-rbind(train_compare_test,data.frame (S.N.=2,modelfunc="lm((Profit^2)~R.D.Spend+log(R.D.Spend)+Marketing.Spend+I(Marketing.Spend^4),data = startup_train )",
                                                         r=sqrt(summary(train_model_2)$r.squared),
                                                         rsq=summary(train_model_2)$r.squared,RMSE=sqrt(mean(error_model_2^2))))

##test model-2##

test_model_2<-lm((Profit^2)~R.D.Spend+log(R.D.Spend)+Marketing.Spend+I(Marketing.Spend^4),data = startup_test)
summary(test_model_2)
predict_model_2<-(test_model_2$fitted.values)^2 #predicted 
error_model_2<- startup_test$Profit - predict_model_2
## RMSE(root mean square error)
sqrt(mean(error_model_2^2))  ##17657.33

train_compare_test<-rbind(train_compare_test,data.frame (S.N.=2,modelfunc="lm((Profit^2)~R.D.Spend+log(R.D.Spend)+Marketing.Spend+I(Marketing.Spend^4),data = startup_test)",
                                                         r=sqrt(summary(test_model_2)$r.squared),
                                                         rsq=summary(test_model_2)$r.squared,RMSE=sqrt(mean(error_model_2^2))))

#..........train model-3.....................

train_model_3<-lm(log(Profit)~log(R.D.Spend)+sqrt(Marketing.Spend)+I(1/Marketing.Spend^2),data = startup_train )
summary(train_model_3)
predict_model_3<-exp(train_model_3$fitted.values) #predicted 
error_model_3<-startup_train$Profit - predict_model_3

## RMSE(root mean square error)
sqrt(mean(error_model_3^2))  ##22825.01

train_compare_test<-rbind(train_compare_test,data.frame (S.N.=3,modelfunc="lm(log(Profit)~R.D.Spend+Marketing.Spend+I(1/Marketing.Spend^2) ,data = startup_train )",
                                                         r=sqrt(summary(train_model_3)$r.squared),
                                                         rsq=summary(train_model_3)$r.squared,RMSE=sqrt(mean(error_model_3^2))))


#......test model-3...............#
test_model_3<-lm(log(Profit)~log(R.D.Spend)+sqrt(Marketing.Spend)+I(1/Marketing.Spend^2),data = startup_test )
summary(test_model_3)
predict_model_3<-exp(test_model_3$fitted.values) #predicted 
error_model_3<-startup_test$Profit - predict_model_3

## RMSE(root mean square error)
sqrt(mean(error_model_3^2))  ##19226.46

train_compare_test<-rbind(train_compare_test,data.frame (S.N.=3,modelfunc="lm(log(Profit)~log(R.D.Spend)+sqrt(Marketing.Spend)+I(1/Marketing.Spend^2),data = startup_test)",
                                                         r=sqrt(summary(test_model_3)$r.squared),
                                                         rsq=summary(test_model_3)$r.squared,RMSE=sqrt(mean(error_model_3^2))))
#we see that in train_compare_test table train_model_1 and test_model_1 both models have nearly same r r-square and RMSE value so we finalize the model-1##

#.....final model.........#
final_model<-lm(Profit~R.D.Spend+Marketing.Spend,data = startup_data)
summary(final_model)
predicted_final<-final_model$fitted.values
error_final<-final_model$residuals
confint(final_model,level =0.95)
## RMSE(root mean square error)
sqrt(mean(error_final^2))  ##17983.61

# ggplot for adding regression line for data set##
library(ggplot2)  ##invoking the ggplot2 pacakage##
ggplot(data = startup_data, aes(x =R.D.Spend+Marketing.Spend, y = Profit)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = startup_data, aes(x=R.D.Spend+Marketing.Spend, y=predicted_final))

                
                 