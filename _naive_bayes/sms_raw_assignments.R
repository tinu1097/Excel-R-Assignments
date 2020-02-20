sms_data<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/navie bayes assignments/sms_raw_NB (2).csv",stringsAsFactors = FALSE)
class(sms_data)
str(sms_data)
sms_data$type<-factor(sms_data$type)  ##type coloumn convert into factor##
str(sms_data)

table(sms_data$type)

library(tm) ##invoke textminning package##

#preparation corpus for the text data#
sms_corpus<-Corpus(VectorSource(sms_data$text))
sms_corpus$content[1:50]

#cleaning and remove unwanted symboles#
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
class(sms_corpus_clean)
sms_corpus_clean$content[1:10]

#creating documenttermmatrix (DTM)##
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
class(sms_dtm)

#creating train and test datasets#
sms_train<-sms_data[1:4180,]
sms_test<-sms_data[4181:5559,]

sms_dtm_train <- sms_dtm[1:4180,]
sms_dtm_test<- sms_dtm[4181:5559,]

sms_corpus_train<- sms_corpus_clean[1:4180]
sms_corpus_test<- sms_corpus_clean[4181:5559]

#proportion of tge table##
prop.table(table(sms_train$type))
prop.table(table(sms_test$type))

sms_dict<-findFreqTerms(sms_dtm_train,4)
list(sms_dict[1:130]) #for checking words##

###Document term matrix##
sms_final_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict))

sms_final_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict))

#converts count to a factor#
convert_count<-function(x){
  x<- ifelse(x > 0,1,0)
  x<- factor(x,levels = c(0,1),labels = c("No","Yes"))
}

sms_final_train<-apply(sms_final_train,MARGIN = 2,convert_count)
sms_final_test<-apply(sms_final_test,MARGIN = 2,convert_count)

View(sms_final_train) ##view sms_final_train##
View(sms_final_test) #view sms_final_test##

#.......Model Buidling......#
library(e1071) ##invoke the pacakge for naive bayes##
#...naive Bayes....#
sms_classi<-naiveBayes(sms_final_train,sms_train$type)
sms_classi$levels

sms_test_predi<-predict(sms_classi,sms_final_test)

table(sms_test$type)
final_table<-table(sms_test_predi,sms_test$type)

library(gmodels) #invoke the library for cross tavle#
CrossTable(sms_test_predi,sms_test$type,
           prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))

sms_classi1<-naiveBayes(sms_final_train,sms_train$type,laplace = 2)
sms_test_predi1<-predict(sms_classi1,sms_final_test)


final_table1<-table(sms_test_predi1,sms_test$type)
##cross table##
CrossTable(sms_test_predi1,sms_test$type,
           prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))

##Accuracy##
mean(sms_test_predi==sms_test$type)  ##0.9768
mean(sms_test_predi1==sms_test$type) ##0.97317
