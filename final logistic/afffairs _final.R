###problem statemnet-2###
##predict affairs###

Affairs<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/logistic regression/Affairs.csv")
library(AER)
data("Affairs",package = "AER")   ##by question###
attach(Affairs)  #can use column without mention file name#
library(plyr)      ##"plyr is a set of tools that solves a common set of problems#
library(ROCR)   ##for graphical representation###
library(pROC)    ##forvisualization and comparing ###
library(dplyr)   ##for manipulation of the data##


####EDA(Exploratory Data Analysis)####
sum(is.na(Affairs))   ##checking NA value##
str(Affairs)

Affairs$affairs[Affairs$affairs>0] <- 1  #find >0 values replace with 1 in the data set#
str(Affairs)

#Convert categorical to numerical.
for (i in c(1,2,5)) {
  if(i==1){
    Affairs[,1] <- as.factor(Affairs[,1])
  }
  if(levels(Affairs[,i])==2){
  Affairs[,i] <- as.numeric(Affairs[,i])
  Affairs[,i][Affairs[,i]==2] <- 0
  Affairs[,i] <- as.factor(Affairs[,i])
  }else{
    Affairs[,i] <- as.numeric(Affairs[,i])
   Affairs[,i] <- as.factor(Affairs[,i])
  }
}

### 4 moment of business decision ####
str(Affairs)
plyr::colwise(mean)(Affairs[,c(3,4,6,7,8,9)])
plyr::colwise(median)(Affairs[,c(3,4,6,7,8,9)])
plyr::colwise(var)(Affairs[,c(3,4,6,7,8,9)])
plyr::colwise(sd)(Affairs[,c(3,4,6,7,8,9)])



#........Plotting.....

#.......Box plot..........
for (i in c(3,4,6,7,8,9)) {
  name <- names(Affairs)[i]
  boxplot(Affairs[,i],main = name)
  readline("press enter to continue")
}


boxplot(Affairs$age)$out
summary(Affairs$age)
## we can see that there is outlier in age cases. so we remove outlier ###### 

boxplot(Affairs$age)$out
IQR(Affairs$age)
quantile(Affairs$age)
upper_outlier <- 37+1.5*10
lower_outlier <- 27-1.5*10
summary(Affairs$age)
Affairs$age[Affairs$age>upper_outlier] <- 32
Affairs$age[Affairs$age<lower_outlier] <- 32

#........QQ plot..........
for (i in c(3,4,6,7,8,9)){
  if(i==1|i==2){
    print("no plot for factor")
  }else{
    name<-names(Affairs)[i]
    qqnorm(Affairs[,i],main = c(name,"plot"),ylab =name)
    qqline(Affairs[,i])
  }
  readline("press enter")
}


#....histrogram.......#

for (i in c(3,4,6,7,8,9)) {
  name <- names(Affairs)[i]
  hist(Affairs[,i] ,main= name)
  readline("press enter to continue")
}


#.........for scatter plot.........#
pairs(Affairs,main="scatter plot")  


# We can no way use the linear regression technique to classify the data
# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1

Affairs_model<-glm(affairs~.,family = "binomial",data = Affairs)
summary(Affairs_model)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(Affairs_model))

#predict(Affairs_model,Affairs)
(Affairs_model$fitted.values)

# Confusion matrix table 
prob. <- predict(Affairs_model,Affairs,type="response")

# Confusion matrix and considering the threshold value as 0.6 
confusion<-table(prob.>0.5,Affairs$affairs)
confusion

table(Affairs$affairs)   ##total false value and total true value##

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.75

error=1-Accuracy
error

prop.table(table(Affairs$affairs))
prop.table(table(prob.>0.5))

# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
rocrpred<-prediction(prob.,Affairs$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)


plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))
# More area under the ROC Curve better is the logistic regression model obtained


## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

rocr_cutoff<-round(rocr_cutoff,2)
rocr_cutoff<-arrange(rocr_cutoff,desc(TPR))

##auc(area under the curve)####
auc<-performance(rocrpred,measure = "auc")
auc<-auc@y.values[[1]]
str(auc)



















