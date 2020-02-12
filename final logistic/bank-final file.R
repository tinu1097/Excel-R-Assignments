##problem statement-1##
#predict y in the data set##

bank_data <- read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/logistic regression/final logistic/new_bank.csv")
bank_data<-bank_data[-1]
attach(bank_data)  #can use column without mention file name#
library(plyr)         ##"plyr is a set of tools that solves a common set of problems##
library(ROCR)   ##for graphical representation###
library(pROC)    ##for visualization and comparing ###
library(dplyr)   ##for manipulation of the data##


####EDA(Exploratory Data Analysis)####
sum(is.na(bank_data))    #checking NA value##
str(bank_data)


#Convert categorical to numerical.

#bank_data$y <- as.character(bank_data$y)
#bank_data$y[bank_data$y=='no'] <- 0
#bank_data$y[bank_data$y=='yes'] <- 1

#bank_data$loan <- as.character(bank_data$loan)
#bank_data$loan[bank_data$loan=='no'] <- 0
#bank_data$loan[bank_data$loan=='yes'] <- 1

#bank_data$housing <- as.character(bank_data$housing)
#bank_data$housing[bank_data$housing=='no'] <- 0
#bank_data$housing[bank_data$housing=='yes'] <- 1

#bank_data$default<- as.character(bank_data$default)
#bank_data$default[bank_data$default=='no'] <- 0
#bank_data$default[bank_data$default=='yes'] <- 1

###########OR#################

#Convert categorical to numerical.
str(bank_data)

for (i in c(2,3,4,5,7,8,9,11,16,17)) {
  if(levels(bank_data[,i])==2){
    bank_data[,i][bank_data[,i]=='no'] <- 0
    bank_data[,i][bank_data[,i]=='yes'] <- 1
    bank_data[,i] <- as.factor(bank_data[,i])
  }else{
    bank_data[,i] <- as.numeric(bank_data[,i])
    bank_data[,i] <- as.factor(bank_data[,i])
  }
  #print(levels(bank_data[,i]))
}

str(bank_data)    ##strucure of the data set##

### 4 moment of business decision ####
summary(bank_data)
plyr::colwise(mean)(bank_data[,c(1,6,10,12:15)])
plyr::colwise(median)(bank_data[,c(1,6,10,12:15)])
plyr::colwise(var)(bank_data[,c(1,6,10,12:15)])
plyr::colwise(sd)(bank_data[,c(1,6,10,12:15)])

#Plotting.
#.........Box plot.........
for (i in c(1,6,10,12,13,14,15)) {
  name <- names(bank_data)[i]
  boxplot(bank_data[,i],main = name)
  readline("press enter to continue")
}
## we can see that there is outlier in most cases###### 

#..........QQ plot...........

for (i in c(1,6,10,12,13,14,15)){
  if(i==2|i==5){
    print("no plot for factor")
  }else{
    name<-names(bank_data)[i]
    qqnorm(bank_data[,i],main = c(name,"plot"),ylab =name)
    qqline(bank_data[,i])
  }
  readline("press enter")
}


#..........histrogram........#

for (i in c(1,6,10,12,13,14,15)) {
  name <- names(bank_data)[i]
  hist(bank_data[,i] ,main= name)
  readline("press enter to continue")
}


#.......for scatter plot..........
pairs(bank_data,main="scatter plot")  


# We can no way use the linear regression technique to classify the data
# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1

bank_model<-glm(y~.,family = "binomial",data = bank_data)
summary(bank_model)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(bank_model))

#predict(bankmodel,bank_data)
(bank_model$fitted.values)

# Confusion matrix table 
prob. <- predict(bank_model,bank_data,type="response")

# Confusion matrix and considering the threshold value as 0.6 
confusion<-table(prob.>0.6,bank_data$y)
confusion

table(bank_data$y)   ##total false value and total true value##

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.90

error=1-Accuracy
error

prop.table(table(bank_data$y))
prop.table(table(prob.>0.5))

# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
rocrpred<-prediction(prob.,bank_data$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)

bank_data<-bank_data[,c(17,1:16)] #change last coloumn to first coloumn###

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))
# More area under the ROC Curve better is the logistic regression model obtained


## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

rocr_cutoff<-round(rocr_cutoff,2)
rocr_cutoff<-arrange(rocr_cutoff,desc(TPR))

##auc(area under the curve more auc model is good )####
auc<-performance(rocrpred,measure = "auc")
auc<-auc@y.values[[1]]
str(auc)
