#all required libraries#
library(recommenderlab)
library(Matrix)
library(caTools)
library(reshape2)
library(ggplot2)
books<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/Recommendation/books.csv",stringsAsFactors = F)
#...........EDA............#
books <- books[,-1]
books_data <- books[1:3000,]
colnames(books_data)<-c("users","title","auther","publisher","ratings")
books_data$title<-as.factor(books_data$title)
books_data$publisher<-as.factor(books_data$publisher)
books_data$auther<-as.factor(books_data$auther)
str(books_data)
head(books_data)
str(books_data)
attach(books_data)

#...plotting............#
 hist(books_data$ratings)
hist(books_data$users)
boxplot(books_data$ratings)
plot(books_data$users,books_data$ratings,ylab="Ratings",xlab="Users",main="Rating Distributer")
qqplot(books_data$users,books_data$ratings)
qqline(books_data$ratings)

#........Building Recommendation#####
auther_matrix <- as.matrix(acast(books_data,users~auther,fun.aggregate = mean))
publisher_matrix<- as.matrix(acast(books_data,users~publisher,fun.aggregate = mean))
rating_matrix<- as.matrix(acast(books_data,users~ratings,fun.aggregate = mean))
View(auther_matrix)
View(publisher_matrix)
View(rating_matrix)

#creating real rating matrix for auther,publisher,rating#
auther_matrix_r<-as(auther_matrix,"realRatingMatrix")
publisher_matrix_r<-as(publisher_matrix,"realRatingMatrix")
rating_matrix_r<-as(rating_matrix,"realRatingMatrix")


##applying different methods(populare,ubcf,ibcf)## 
auther_model1=Recommender(auther_matrix_r,method="POPULAR")  #popular method##
auther_model2=Recommender(auther_matrix_r, method="UBCF") ##user based collabrative filtering#
auther_model3=Recommender(auther_matrix_r,method="IBCF") ##item based collabrative filtering#

publisher_model1=Recommender(publisher_matrix_r,method="POPULAR") ##popular method##
publisher_model2=Recommender(publisher_matrix_r, method="UBCF") ## user based collabrative filtering#
publisher_model3=Recommender(publisher_matrix_r,method="IBCF") ## item based collabrative filtering#


rating_model1=Recommender(rating_matrix_r,method="POPULAR") ##popular method
rating_model2=Recommender(rating_matrix_r, method="UBCF") ##user based collabrative filtering#
rating_model3=Recommender(rating_matrix_r,method="IBCF") ##item based collabrative filtering#


##using different methood for recommendation##
#recommended books by using authores## 

recommended_book <- predict(auther_model1,auther_matrix_r[1], n=5) ##5recommended by popular method#
as(recommended_book, "list")

recommended_book1 <- predict(auther_model2,auther_matrix_r[1], n=5) #5recommendation by UBCF##
as(recommended_book1, "list") 
recommended_book2 <- predict(auther_model3,auther_matrix_r[1], n=5) #5 recommendation by IBCF#
as(recommended_book2, "list")

##recommendedbooks by using publishers##

recommended_book_p <- predict(publisher_model1,publisher_matrix_r[1501:1506], n=5) ##5recommended by popular method#
as(recommended_book_p, "list")

recommended_book_p1 <- predict(publisher_model2,publisher_matrix_r[1501:1506], n=5) #5recommendation by UBCF##
as(recommended_book1, "list") 


recommended_book_p2 <- predict(publisher_model3,publisher_matrix_r[1])   #5 recommendation by IBCF#
as(recommended_book_p2, "list")

#recommendation books by ratings#
recommended_book_r <- predict(rating_model1,rating_matrix_r[1501:1506], n=5) ##recommended by popular method#
as(recommended_book_r, "list")
  
recommended_book_r1 <- predict(rating_model2,rating_matrix_r[1], n=5)    #5recommendation by UBCF
as(recommended_book_r1, "list")

recommended_book_r2 <- predict(rating_model3,rating_matrix_r[1])   #5 recommendation by IBCF#
as(recommended_book_r2, "list")



















