#......problem statement-1.........#

library(arules) 
##find associations between many variables.and apriori algorithm  use for frequent item sets#####
library(arulesViz) 
# visualization  Frequecy plot###
book<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Association_rule_assignment/final assignment/book.csv")
attach(book)
str(book)
summary(book)

###frequency table###
for (i in c(1:11)){
  if(i==1){
    cat("  0  ","1  ","Colnames","\n")
  }
  cat(table(book[i])[[1]],
      table(book[i])[[2]])
  cat(" ",names(book)[i],"\n")
}


#barplot(sapply(book,sum),col=1:50)  ##we can check fist 50 frequency## 

#.........creating Rule............#
## Applying apriori algorithm to get relevant rules##
#With 2 support value,Confidence 50%, and minimum length of 3.
book_rule<-apriori(as.matrix(book),parameter = list(support=0.02,confidence=0.5,minlen=3))
inspect(sort(book_rule,by="lift"))
summary(book_rule)
#With 2 support value,Confidence 50%, and minimum length of 3.
book_rule_1<-apriori(as.matrix(book),parameter = list(support=0.02,confidence=0.5,minlen=6))
inspect(sort(book_rule_1,by="lift"))
summary(book_rule_1)

#.......plotting...............#

plot(book_rule,method = "scatterplot")
plot(book_rule,method = "grouped")
plot(book_rule,method = "graph")
plot(book_rule,method = "paracoord")
plot(book_rule,method = "matrix3D")

book_subrules <- subset(book_rule, lift>3) #Get only rules above lift ration of 4.
inspect(sort(book_subrules,by="lift"))
plot(book_subrules, method="matrix3D")
plot(book_subrules, method="grouped")
plot(book_subrules, method="paracoord")
plot(book_subrules,method = "graph")

###reduant values#########
book_redundant<-is.redundant(book_subrules)
summary(book_redundant)
###here we can see that 32 rules are duplicate and 30 are not duplicate ##
book_redundant<-(book_subrules[is.redundant(book_subrules)])
arules::inspect(book_redundant)

###remove reduant values#######
final_book<-(book_subrules[!is.redundant(book_subrules)])  
arules::inspect(final_book) 





