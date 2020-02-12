##########predicting price#############
##problem statement-3#####################
ToyotaCorolla<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Multi linear regression/final multi linear/ToyotaCorolla.csv")
corolla <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(corolla)       #can use column without mention file name#
library(plyr)                   #invoking the packages#
library(GGally)  ##for representation##
library(car)   #for plotting##
library(caTools) #spliting data#
library(moments) #skewness and kurtosis#


#########EDA(Exploratory Data Analysis)########

###Data Cleanup####
sum(is.na(corolla))   #checking missing values##
summary(corolla)

### 4 moment of business decision ####
summary(corolla)
plyr::colwise(mean)(corolla)
plyr::colwise(median)(corolla)
plyr::colwise(var)(corolla)
plyr::colwise(sd)(corolla)
plyr::colwise(skewness)(corolla)
plyr::colwise(kurtosis)(corolla)

pairs(corolla,main="scatterplot")   #scatter plot for checking relationship or not#
cor(corolla)   ##Correlation Coefficents 'r'#
library(corpcor)
cor2pcor(cor(corolla))   ##partial correlation matrix-pur correlation b/w the variables##

#..........plotting........#
pairs(corolla,main="scatterplot") 
boxplot(corolla,main="boxplot")$out
boxplot(corolla$Quarterly_Tax)$out
ggpairs(corolla)   ##we can also see correlation coefficient and scatter plot together##


##QQ-plot##
#.
qqnorm(corolla[,1], main="Corolla Price", ylab= "plot")
qqline(corolla[,1],main="Corolla Price", ylab= "plot")
  
qqnorm(corolla[,2], main="Corolla Price", ylab= "plot")
qqline(corolla[,2],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,3], main="Corolla Price", ylab= "plot")
qqline(corolla[,3],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,4], main="Corolla Price", ylab= "plot")
qqline(corolla[,4],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,5], main="Corolla Price", ylab= "plot")
qqline(corolla[,5],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,6], main="Corolla Price", ylab= "plot")
qqline(corolla[,6],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,7], main="Corolla Price", ylab= "plot")
qqline(corolla[,7],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,8], main="Corolla Price", ylab= "plot")
qqline(corolla[,8],main="Corolla Price", ylab= "plot")

qqnorm(corolla[,9], main="Corolla Price", ylab= "plot")
qqline(corolla[,9],main="Corolla Price", ylab= "plot")



#We only remove outlier from cc cause there is only one influncer which is far away from Q3 data point.
#outlier
boxplot(corolla$cc)$out
IQR(corolla$cc)
quantile(corolla$cc)
upper_outlier <- 1600+1.5*200
lower_outlier <- 1400-1.5*200
summary(corolla$cc)
corolla$cc[corolla$cc>upper_outlier] <- 1600
corolla$cc[corolla$cc<lower_outlier] <- 1600

#...........Splitting the data into train and test.........#
library(caTools)
split<-(sample.split(corolla$Price,SplitRatio=0.70))
corolla_train <- subset(corolla,split==TRUE)
corolla_test <- subset(corolla,split==FALSE)

#####Train_Model-Building##########
corolla_model_1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = corolla_train)
summary(corolla_model_1)    ##check R-squared value and '***','**','*',denotes dependent variables significant otherwise insignificant##                   
#Doors is insignificant.

# Multicollinearity check
#check relationship each independent and dependent variable##
##prediction based on only speed ##
model_Age_08_04<-lm(Price~Age_08_04)
summary(model_Age_08_04)     #R-squared=0.7684,and Age_08_04 is significant##

##prediction based on only KM  ##
model_KM<-lm(Price~KM)
summary(model_KM)   ##R-squared=0.3249, KM is  significant#

##prediction based on only HP###
model_HP<-lm(Price~HP)
summary(model_HP)     #R-squared=0.09922,and HP  is significant

##prediction based on only screen###
model_cc<-lm(Price~cc)
summary(model_cc)     #R-squared=0.01597,and cc is significant


##prediction based on only Doors###
model_Doors<-lm(Price~Doors)
summary(model_Doors)     #R-squared=0.03435,and Doors is significant

##prediction based on only Geras###
model_Gears<-lm(Price~Gears)
summary(model_Gears)     #R-squared=0.003982,when Gears is insignificant.

##prediction based on only Quarterly_Tax###
model_Quarterly_Tax<-lm(Price~Quarterly_Tax)
summary(model_Quarterly_Tax)     #R-squared=0.04805,and Quarterly_Tax is significant


##prediction based on only Weight###
model_Weight<-lm(Price~Weight)
summary(model_Weight)     #R-squared=0.3378,and Weight is significant


## we see that all variables are significant but gears insignificant#####
#check  Variance Inflation factor(VIF) to check collinearity b/n variables value
library(car)
vif(corolla_model_1) 
#all variable VIF value less then 10 .so no relationship each oter##

## Added Variable plot to check correlation b/n variables and o/p variable
library(car)
avPlots(corolla_model_1,id.n=5,id.cex=0.7)  #by avplots we can see that doors and gears are less contributing to price##

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
library(car)
influence.measures(corolla_model_1)
influenceIndexPlot(corolla_model_1,id.n=5) # index plots for infuence measures
influencePlot(corolla_model_1,id.n=5) # A user friendly representation of the above
 
# Regression after deleting the 222,961th Observations
corolla_model_2<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = corolla_train[,-c(222,961)])
summary(corolla_model_2)
#after deleting 222,961th observation Doors is not significant.

#Conclusion : We will build model excluding Doors.

#..........Model Testing............#

train_vs_test<- setNames(data.frame(matrix(ncol = 5,nrow = 0)),c("S.N.","modelfunc","r","rsq","RMSE"))

######## Train_model-1 ############# 

train_model_1<-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = corolla_train)  
r <- sqrt(summary(train_model_1)$r.squared)
rsq <- summary(train_model_1)$r.squared
rmse <- sqrt(mean(train_model_1$residuals^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=1,modelfunc="Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight",
                                              r=r,rsq=rsq,RMSE=rmse))

######### Test Model 1 ################

test_model_1<-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data = corolla_test)  
r <- sqrt(summary(test_model_1)$r.squared)
rsq <- summary(test_model_1)$r.squared
rmse <- sqrt(mean(test_model_1$residuals^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=1,modelfunc="Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight",
                                              r=r,rsq=rsq,RMSE=rmse))

################################################################################
######## Train_model-2 ############# 
train_model_2<-lm(log(Price)~I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Quarterly_Tax+Weight,data = corolla_train)  
predicted<- exp(predict(train_model_1))
x<- Price- predicted
r<- sqrt(summary(train_model_2)$r.squared)
rsq <- summary(train_model_2)$r.squared
rmse <- sqrt(mean(x^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=2,modelfunc="log(Price)~I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Quarterly_Tax+Weight",
                                              r=r,rsq=rsq,RMSE=rmse))

######### Test Model 2 ################

test_model_2<-lm(log(Price)~I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Quarterly_Tax+Weight,data = corolla_test)  
pedicted<- exp(predict(test_model_2))
x <- Price-predic_pri
r<- sqrt(summary(test_model_2)$r.squared)
rsq <- summary(test_model_2)$r.squared
rmse <- sqrt(mean(x^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=2,modelfunc="log(Price)~I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Quarterly_Tax+Weight",
                                              r=r,rsq=rsq,RMSE=rmse))

#Now build 3rd model
######train model-3#################
train_model_3<-lm(sqrt(Price)~log(Age_08_04)+KM+sqrt(HP)+cc+Gears+I(Gears^4)+Quarterly_Tax+Weight+I(1/Weight^2),data = corolla_train)  
predicted<- (predict(train_model_3))^2
x<- Price- predicted
r<- sqrt(summary(train_model_3)$r.squared)
rsq <- summary(train_model_3)$r.squared
rmse <- sqrt(mean(x^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=2,modelfunc="sqrt(Price)~log(Age_08_04)+KM+sqrt(HP)+cc+Gears+I(Gears^4)+Quarterly_Tax+Weight+I(1/Weight^2)",
                                              r=r,rsq=rsq,RMSE=rmse))
######train model-3#################
test_model_3<-lm(sqrt(Price)~log(Age_08_04)+KM+sqrt(HP)+cc+Gears+I(Gears^4)+Quarterly_Tax+Weight+I(1/Weight^2),data = corolla_test)  
predicted<- (predict(test_model_3))^2
x<- Price- predicted
r<- sqrt(summary(test_model_3)$r.squared)
rsq <- summary(test_model_3)$r.squared
rmse <- sqrt(mean(x^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=2,modelfunc="sqrt(Price)~log(Age_08_04)+KM+sqrt(HP)+cc+Gears+I(Gears^4)+Quarterly_Tax+Weight+I(1/Weight^2)",
                                              r=r,rsq=rsq,RMSE=rmse))



######train model -4#################3
train_model_4<-lm(Price~log(Age_08_04)+I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Gears+log(Quarterly_Tax)+I(Quarterly_Tax^4)+(1/Weight^2),data = corolla_train)  
predicted<- (predict(train_model_4))
x<- train_model_4$residuals
r<- sqrt(summary(train_model_4)$r.squared)
rsq <- summary(train_model_4)$r.squared
rmse <- sqrt(mean(x^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=2,modelfunc="Price~log(Age_08_04)+I(Age_08_04^2)+log(KM)+sqrt(HP)+cc+sqrt(Gears)+I(Gears^4)+Quarterly_Tax+I(Quarterly_Tax^4)+Weight+I(1/Weight^2)",
                                              r=r,rsq=rsq,RMSE=rmse))

########test model-4##################
test_model_4<-lm(Price~log(Age_08_04)+I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Gears+log(Quarterly_Tax)+I(Quarterly_Tax^4)+(1/Weight^2),data = corolla_test)  
predicted<- (predict(test_model_4))
x<- test_model_4$residuals
r<- sqrt(summary(test_model_4)$r.squared)
rsq <- summary(test_model_4)$r.squared
rmse <- sqrt(mean(x^2))
train_vs_test<-rbind(train_vs_test,data.frame(S=2,modelfunc="Price~log(Age_08_04)+I(Age_08_04^2)+log(KM)+sqrt(HP)+cc+sqrt(Gears)+I(Gears^4)+Quarterly_Tax+I(Quarterly_Tax^4)+Weight+I(1/Weight^2)",
                                              r=r,rsq=rsq,RMSE=rmse))

#we will go with train_model_4and test_model_4 because both models having similiarity in r,r-square,RMSE##
#########final model#######################
final_model<-lm(Price~log(Age_08_04)+I(Age_08_04^2)+KM+sqrt(HP)+cc+Gears+Gears+log(Quarterly_Tax)+I(Quarterly_Tax^4)+(1/Weight^2),data = corolla[,-6])  
summary(final_model)
predicted_final<-(final_model$fitted.values)
error_final<- Price- predicted_final
confint(final_model,level =0.95)
hist(final_model$residuals)
## RMSE(root mean square error)
sqrt(mean(error_final^2))  ##1355.768

# ggplot for adding regression line for data set##
library(ggplot2)  ##invoking the ggplot2 pacakage##
ggplot(data = corolla, aes(x =Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, y =Price)) + 
  geom_point(color='black') +
  geom_line(color='red',data = corolla, aes(x=Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight, y=predicted_final))
















