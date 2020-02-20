Airline<-read.csv("C:/Users/Hp/Downloads/DATA SCIENCE ASSIGNMENTS/forcasting/Airlines+Data.csv")
View(Airline)
plot(Airline$Passengers,type = "o")

# So creating 12 dummy variables 

air<-data.frame(outer(rep(month.abb,length = 96),month.abb,"==")+ 0)
View(air)
colnames(air)<-month.abb
