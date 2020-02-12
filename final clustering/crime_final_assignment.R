####problem statement-1###
###use hierrarchical method###

crime_data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/clustering assignment/crime_data.csv")
crime<-crime_data[-1]
attach(crime)
library(plyr)


####EDA(Exploratory Data Analysis)####
sum(is.na(crime))    #checking NA value##
summary(crime)

#4th moments of business decision#
plyr::colwise(mean)(crime)
plyr::colwise(median)(crime)
plyr::colwise(var)(crime)
plyr::colwise(sd)(crime)

######>>>>plottin>>>>>>#######
#.........Box plot.........
for (i in c(1:4)) {
  name <- names(crime)[i]
  boxplot(crime[,i],main = name)
  readline("press enter to continue")
}

boxplot(crime$Rape)$out
summary(crime$Rape)
### we can see that there is outlier in rape cases. so we remove outlier ######
boxplot(crime$Rape)$out
IQR(crime$Rape)
quantile(crime$Rape)
upper_outlier <- 26.18+1.5*11.1
lower_outlier <- 15.07-1.5*11.1
summary(crime$Rape)
crime$Rape[crime$Rape>upper_outlier] <- 20.10
crime$Rape[crime$Rape<lower_outlier] <- 20.10

boxplot(crime$Rape)$out   ## after remove outlier we check###

#..........histrogram........#

for (i in c(1:4)) {
  name <- names(crime)[i]
  hist(crime[,i] ,main= name)
  readline("press enter to continue")
}

#........QQ plot..........

for (i in c(2,3,4,5)){
  if(i==1){
    print("no plot for character")
  }else{
    name<-names(crime_data)[i]
    qqnorm(crime_data[,i],main = c(name,"plot"),ylab =name)
    qqline(crime_data[,i])
  }
  readline("press enter")
}

#.........for scatter plot.........#
pairs(crime,main="scatter plot")

# Normalizing continuous columns to bring them under same scale
# Normalizing continuous columns to bring them under same scale
normalized_crime<-scale(crime[1:4]) #excluding the X columnbefore normalizing
dist_crime<- dist(normalized_crime, method = "euclidean") # distance matrix
dist_crime1<-dist(normalized_crime,method = "manhattan")  
dist_crime2<-dist(normalized_crime,method = "maximum")
dist_crime3<-dist(normalized_crime,method = "canberra")

fit_crime <- hclust(dist_crime, method="complete")  ##farthest ponit b/w two cluster###
str(fit_crime)
fit_crime1<-hclust(dist_crime,method="centroid")   ## calculate mean each cluster##
str(fit_crime1)
fit_crime2<-hclust(dist_crime,method="single")    ## closest point b/w two cluster##
str(fit_crime2) 
fit_crime3<-hclust(dist_crime,method="average")    ##average of all  distance members b/w two cluster## 
str(fit_crime3)

plot(fit_crime) # display dendrogram by complete method#
plot(fit_crime, hang=-1)
plot(fit_crime1)   # display dendrogram by centroid method#
plot(fit_crime1,hang = -1)
plot(fit_crime2)    # display dendrogram by single method#
plot(fit_crime2,hang = -1)
plot(fit_crime3)        # display dendrogram by average method#
plot(fit_crime3,hang = -1)

##but we will use complete method ####
rect.hclust(fit_crime, k=3, border="red")

groups <- cutree(fit_crime, k=3)
groups


membership<-as.matrix(groups) # groups or cluster numbers

final_cr <- data.frame(crime_data, membership)
final_crime<-final_cr[,c(ncol(final_cr),1:(ncol(final_cr)-1))] 
aggregate(crime,by=list(final_crime$membership),mean)

#inference: group-2= most danger
            #group-1= more danger
            #group-3= less danger
#here  are three groups in three groups group2 is most dengearous because in group2 (murder,assault,urbanpop,rape)  are high rate.
#and first  group is more danger because in group1(murder,assault,urbanpop,rape) are high comparison with group3.


###################################################################################
 ##non heirrarchical method (K-mean)
crime_data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/clustering assignment/crime_data.csv")
crime<-crime_data[-1]
attach(crime)

#EdA procees  done above######

#Normalizing continuous columns to bring them under same scale
# Normalizing continuous columns to bring them under same scale
normalized_crime<-scale(crime[1:4]) #excluding the X columnbefore normalizing
wss = NULL   ##(within sum of squre)####

###elbow curve######
k_crime<-kmeans(normalized_crime,3)
str(k_crime)
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normalized_crime,i)$tot.withinss)
  
}

plot(2:15, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# Choosing the best cluster as 5
k_5_crime <- kmeans(normalized_crime,5)
crime_data["Cluster"] <- k_5_crime$clusters
aggregate(crime_data[-1],by=list(k_5_crime$cluster),FUN=mean)












