####problem statement-2###
###use hierrarchical method###

airlines<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/clustering assignment/airlines.csv")
airlines<-airlines[-1]
attach(airlines)
library(plyr)
####EDA(Exploratory Data Analysis)####
sum(is.na(airlines))    #checking NA value##
summary(airlines)


#4th moments of business decision#
plyr::colwise(mean)(airlines)
plyr::colwise(median)(airlines)
plyr::colwise(var)(airlines)
plyr::colwise(sd)(airlines)



######>>>>plottin>>>>>>#######
#.........Box plot.........
for (i in c(1:12)) {
  name <- names(airlines)[i]
  boxplot(airlines[,i],main = name)
  readline("press enter to continue")
}

###we can see that most of in outliers ######

#..........histrogram........#

for (i in c(1:12)) {
  name <- names(airlines)[i]
  hist(airlines[,i] ,main= name)
  readline("press enter to continue")
}

#........QQ plot..........


for (i in c(1:11)){
  if(i==12){
    print("no plot for binary")
  }else{
    name<-names(airlines)[i]
    qqnorm(airlines[,i],main = c(name,"plot"),ylab =name)
    qqline(airlines[,i])
  }
  readline("press enter")
}

#.........for scatter plot.........#
pairs(airlines,main="scatter plot")

# Normalizing continuous columns to bring them under same scale
# Normalizing continuous columns to bring them under same scale
normalized_airlines<-scale(airlines) #excluding the X columnbefore normalizing
dist_airlines<- dist(normalized_airlines, method = "euclidean") # distance matrix
dist_airlines1<-dist(normalized_airlines,method = "manhattan")  
dist_airlines2<-dist(normalized_airlines,method = "maximum")
dist_airlines3<-dist(normalized_airlines,method = "canberra")

fit_airlines <- hclust(dist_airlines, method="complete")  ##farthest ponit b/w two cluster###
str(fit_airlines)
fit_airlines1<-hclust(dist_airlines1,method="centroid")   ## calculate mean each cluster##
str(fit_airlines1)
fit_airlines2<-hclust(dist_airlines2,method="single")    ## closest point b/w two cluster##
str(fit_airlines2) 
fit_airlines3<-hclust(dist_airlines3,method="average")    ##average of all  distance members b/w two cluster## 
str(fit_airlines3)

plot(fit_airlines) # display dendrogram by complete method#
plot(fit_airlines, hang=-1)
plot(fit_airlines1)   # display dendrogram by centroid method#
plot(fit_airlines1,hang = -1)
plot(fit_airlines2)    # display dendrogram by single method#
plot(fit_airlines2,hang = -1)
plot(fit_airlines3)        # display dendrogram by average method#
plot(fit_airlines3,hang = -1)


##but we will use complete method ####
rect.hclust(fit_airlines, k=10, border="red")

groups <- cutree(fit_airlines, k=10)
groups


membership<-as.matrix(groups) # groups or cluster numbers

final_air <- data.frame(airlines, membership)
final_airlines<-final_air[,c(ncol(final_air),1:(ncol(final_air)-1))] 
aggregate(airlines,by=list(final_airlines$membership),mean)


###################################################################################
##non heirrarchical method (K-mean)

airlines<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/clustering assignment/airlines.csv")
final_airlines<-airlines[-1]
attach(final_airlines)
#EdA procees  done above######
#Normalizing continuous columns to bring them under same scale
# Normalizing continuous columns to bring them under same scale
normalized_airlines<-scale(final_airlines) #excluding the id columnbefore normalizing
wss = NULL   ##(within sum of squre)####

###elbow curve######
k_airlines<-kmeans(normalized_airlines,10)
str(k_airlines)
twss <- NULL
for (i in 1:40){
  twss <- c(twss,kmeans(normalized_airlines,i)$tot.withinss)
  
}

plot(1:40, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# Choosing the best cluster as 13
k_13_airlines <- kmeans(normalized_airlines,13)
airlines["Cluster"] <- k_13_airlines$clusters
aggregate(airlines[-1],by=list(k_13_airlines$cluster),FUN=mean)

























