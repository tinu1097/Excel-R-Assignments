#......problem statement-2.........#

library(arules) 
##find associations between many variables.and apriori algorithm  use for frequent item sets#####
library(arulesViz) 
# visualization  Frequecy plot###
my_movies<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Association_rule_assignment/final assignment/my_movies.csv")
str(my_movies)
movies<-my_movies[,6:15]
attach(movies)
str(movies)
summary(movies)

###frequency table###
for (i in c(1:10)){
  if(i==1){
    cat("0","1","Colnames","\n")
  }
  cat(table(movies[i])[[1]],
      table(movies[i])[[2]])
  cat(" ",names(movies)[i],"\n")
}



#barplot(sapply(movies,sum),col=1:10)  ##we can check fist 10 frequency## 

#.........creating Rule............#
## Applying apriori algorithm to get relevant rules##
#With 1 support value,Confidence 50%, and minimum length of 4.create model=29
movies_rule<-apriori(as.matrix(movies),parameter = list(support=0.1,confidence=0.5,minlen=4))
inspect(sort(movies_rule,by="lift"))
summary(movies_rule)

#.......plotting...............#

plot(movies_rule,method = "scatterplot")
plot(movies_rule,method = "grouped")
plot(movies_rule,method = "graph")
plot(movies_rule,method = "paracoord")

###reduant values#########
movies_redundant<-is.redundant(movies_rule)
summary(movies_redundant)
##here we can see that 5 rules are duplicate and 24 are not duplicate ## 
movies_redundant<-(movies_rule[is.redundant(movies_rule)])
arules::inspect(movies_redundant)
###remove reduant values#######
final_movies<-(movies_rule[!is.redundant(movies_rule)])  
arules::inspect(final_movies) 






