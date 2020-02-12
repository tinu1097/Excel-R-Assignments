
#......problem statement-3........#
library(arules) 
##find associations between many variables.and apriori algorithm  use for frequent item sets#####
library(arulesViz) 
# visualization  Frequecy plot###
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:20])
summary(groceries)
showMethods("inspect")

# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 

itemFrequencyPlot(groceries,topN=30)
groceries_rules<-apriori(groceries,parameter = list(support = 0.001,confidence = 0.5,minlen=3))
inspect(sort(groceries_rules,by="lift"))

#..........plotting........#
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
plot(groceries_rules, method="paracoord")

###reduant values#########
groceries_redundant<-is.redundant(groceries_rules)
summary(groceries_redundant)
##here we can see that 35 rules are duplicate and 151 are not duplicate ## 
groceries_redundant<-(groceries_rules[is.redundant(groceries_rules)])
arules::inspect(groceries_redundant)
###remove reduant values#######
final_groceries_rule<-(groceries_rules[!is.redundant(groceries_rules)])  
arules::inspect(final_groceries_rule)
