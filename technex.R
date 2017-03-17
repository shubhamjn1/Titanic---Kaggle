setwd("/home/shubham/Downloads")
train <- read.csv('Training_data.csv')
test <- read.csv('Test Data.csv')
## str(train)
install.packages('lubridate')
library('lubridate')
train$date <- dmy(train$date)
install.packages('dummies')
library(dummies)

plot(train$Hour, train$Total.Visitors)

train$hour_slot <- 1
train$hour_slot[train$Hour > 6 & train$Hour < 20] <- 2
train$hour_slot[train$Hour >= 20 & train$Hour < 24 ] <- 3

cor(train$Temp,train$Feel_Temp)
install.packages('caret')
library(caret)

train$Weekday <- as.factor(train$Weekday)
train$Weekday <- as.factor(train$Weather)
train <- dummy.data.frame(data = train, names =c("time.of.year","Weekday","Weather"))

train$Humidity1 <- train$Humidity
train$Humidity1[train$Humidity1 > 1] <- train$Humidity1[train$Humidity1 > 1]/2
train$Humidity1[train$Humidity1 < 0] <- mean(train$Humidity1[train$Humidity1 > 0 & train$Humidity1 < 1])
temp <- read.csv('Training_data.csv')
train$time.of.year <- temp$time.of.year
train$Weekday <- temp$Weekday
train$Weather <- temp$Weather

lm(Total.Visitors ~ time.of.year + Year + Hour + Weekday + Workingday + Temp + Humidity + Fog.Density, data = train)




##### creating partition in training data for cross validation purpose

sample_size=floor(0.75*nrow(train))

sample_index=sample(seq_len(nrow(train)),size = sample_size)

training<-train[sample_index,]
cv<-train[-sample_index,]

model1<-lm(Total.Visitors ~ time.of.year + Year + Hour + Weather  + Weekday + Workingday + Temp + Humidity + Fog.Density, data = training)
result<-predict(model1,newdata = cv)


#calculating rmse

rmse=sqrt(sum(cv$Total.Visitors-result)^2)

### model2
model2 <-lm(Total.Visitors ~ time.of.year1 + time.of.year2 + time.of.year3 + time.of.year4 + Year + Hour + Weekday1 + Weekday2 + Weekday3 + Weekday4 + Weekday5 + Weekday6 + Weather1 + Weather2 + Weather3 + Weather4 + Workingday + Temp + Humidity + Fog.Density, data = training)
result2 <-predict(model2,newdata = cv)
rmse2 =sqrt(sum(cv$Total.Visitors-result2)^2)

######### date
train$date1 <- format(as.Date(train$date, format="%d/%m/%Y"),"%d")
train$date1 <- as.numeric(train$date1)
train$date1[train$date1 < 10] <- 1
train$date1[train$date1 >= 10] <- 0

cv$date1 <- format(as.Date(cv$date, format="%d/%m/%Y"),"%d")
cv$date1 <- as.numeric(cv$date1)
cv$date1[cv$date1 < 10] <- 1
cv$date1[cv$date1 >= 10] <- 0
####
train$Weather <- as.factor(train$Weather)
train$Weekday <- as.factor(train$Weekday)
train$time.of.year <- as.factor(train$time.of.year)
train$hour_slot <- as.factor(train$hour_slot)
train$Year <- as.factor(train$Year)
train$Day.off <- as.factor(train$Day.off)
train$Workingday <- as.factor(train$Workingday)
train$date1 <- as.factor(train$date1)

cv$date1 <- as.factor(cv$date1)
cv$Weather <- as.factor(cv$Weather)
cv$Weekday <- as.factor(cv$Weekday)
cv$time.of.year <- as.factor(cv$time.of.year)
cv$hour_slot <- as.factor(cv$hour_slot)
cv$Year <- as.factor(cv$Year)
cv$Day.off <- as.factor(cv$Day.off)
cv$Workingday <- as.factor(cv$Workingday)

model3 <-lm(Total.Visitors ~ time.of.year + Year + date1 + hour_slot  +  Workingday + Temp + Humidity1 + Fog.Density, data = train)
result3 <-predict(model3,newdata = cv)
rmse3=sqrt(sum(cv$Total.Visitors-result3)^2)
