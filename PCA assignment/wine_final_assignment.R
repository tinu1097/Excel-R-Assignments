
####problem statemnet for wine PCA#######
 #read the file ####
wine<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/wine.csv")
View(wine)

wine<-wine[-1]    ###remove first coloumn type######
attach(wine)
cor(wine)     ###correlation of the data set###

# cor = TRUE use correlation matrix for getting PCA scores

pca_wine<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca_wine)
str(pca_wine)
loadings(pca_wine)  ##loadings=sum of squared weights##

#.........OR.............#
##we check individually sum of squared weights unique or not ########
#sumweights<-sum(pca_wine$loadings[,6]**2)  
#sumweights
#sumweights<-sum(pca_wine$loadings[,4]**2)  
#sumweights
#sumweights<-sum(pca_wine$loadings[,11]**2)  
#sumweights

##plotting###
plot(pca_wine) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pca_wine)
# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components

##for cumulative sums plot######
plot(cumsum(pca_wine$sdev*pca_wine$sdev)*100/(sum(pca_wine$sdev*pca_wine$sdev)),type="b")

pca_wine$scores[,1:3] #  in this case Top 3 PCA Scores which  represents not whole data only 66% data####

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wine<-cbind(wine,pca_wine$scores[,1:3])
View(wine)

# preparing data for clustering (considering only pca scores as they represent the entire data)
###heirrarchical clustering####
clus_wine<-wine[,14:16]

# Normalizing the data 
norm_clus<-scale(clus_wine) # Scale function is used to normalize data
dist_wine<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit_wine<-hclust(dist_wine,method="complete") # method here is complete linkage

plot(fit_wine,hang = -1) # Displaying Dendrogram

##but we will use complete method ####
rect.hclust(fit_wine, k=7, border="red")
groups<-cutree(fit_wine,7) # Cutting the dendrogram for 5 clusters
groups

membership<-as.matrix(groups) # groups or cluster numbers

final<-cbind(membership,wine) # binding column wise with orginal data
View(final)

View(aggregate(final[,-c(14,15,16),by=list(membership),FUN=mean])) # Inferences can be
# drawn from the aggregate of the universities data on membership_1
View(aggregate(wine[,-c(14,15,16)],by=list(final$membership),mean))

#............k-mean clustering (scree plot or elbow plot)................#
#Normalizing the data 
norm_clus<-scale(clus_wine) # Scale function is used to normalize data
wss = NULL   ##(within sum of squre)####

### scree plot or elbow curve######
k_wine<-kmeans(norm_clus,7)
str(k_wine)
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(norm_clus,i)$tot.withinss)
  
}

plot(2:15, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

..# Choosing the best cluster as 10
k_10_wine <- kmeans(norm_clus,10)
wine["Cluster"] <- k_10_wine$clusters
View(aggregate(wine[,-c(14,15,16)],list(k_10_wine$cluster),FUN = mean))











