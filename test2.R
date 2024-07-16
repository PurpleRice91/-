rm(list=ls())
train=read.csv("C:/Users/user/Desktop/專題/train.csv")
store=read.csv("C:/Users/user/Desktop/專題/store.csv")
test=read.csv("C:/Users/user/Desktop/專題/test.csv")
merge1=read.csv("c:/Users/user/Desktop/專題/merge1.csv")
merge2=read.csv("c:/Users/user/Desktop/專題/merge2.csv")

merge1 = merge(train, store, all = T)
merge1$Year = 9
merge1$Month = 9
merge1$Day = 9
#install.packages("lubridate")
library(lubridate)
merge1$Year = year(merge1$Date)
merge1$Month = month(merge1$Date)
merge1$Day = day(merge1$Date)
merge1 = subset(merge1, select = c(Store, DayOfWeek, Year, Month, Day, Sales, Customers, Open, Promo, StateHoliday,	SchoolHoliday, StoreType, Assortment,	CompetitionDistance, CompetitionOpenSinceMonth, CompetitionOpenSinceYear, Promo2,	Promo2SinceWeek, Promo2SinceYear, PromoInterval))
merge1$StoreType = factor(merge1$StoreType, levels = c('a', 'b', 'c','d'), labels = c('1', '2', '3','4'))
merge1$Assortment = factor(merge1$Assortment, levels = c('a', 'b', 'c'), labels = c('1', '2', '3'))
train$StateHoliday = factor(train$StateHoliday, levels = c('0','a', 'b', 'c'), labels = c('0','1', '2', '3'))
merge1$PromoInterval = factor(merge1$PromoInterval, levels = c('Jan,Apr,Jul,Oct', 'Feb,May,Aug,Nov', 'Mar,Jun,Sept,Dec'), labels = c(1, 2, 3))

subset1=subset(merge1,Year>2014&Month>4, drop=F)
subset2=subset(merge1, (Year<2015&Month>0)|Year>2014&Month<5, drop=F)
#subset1=subset(merge2,Year>=2015&Month>=5)
#subset2=subset(merge2,(Year==2015&Month<5)|(Year<2015))
#merge2 <- subset(merge1, select = c(Store	,DayOfWeek,	Date,Year,Month,Day	,Sales,	Customers,	Open,	Promo	,StateHoliday,	SchoolHoliday,	StoreType,	Assortment,	CompetitionDistance,	CompetitionOpenSinceMonth,	CompetitionOpenSinceYear,	Promo2,	Promo2SinceWeek,	Promo2SinceYear,	PromoInterval,	x1,	x2,	x3,	x4,	y1,	y2,	y3))
#predict.model = lm(Sales ~ Promo + StateHoliday + SchoolHoliday, train)
#prediction = predict(predict.model,newdata=test)
#install.packages("ggplot2")
#library(ggplot2)
#train$Sales<- ifelse(train$Sales == "yes", 1, 0);
#N <- nrow(train)
#newtrain<- train[sample(N),]
#取出樣本數的idx
#k_idx <- sample(seq_len(N), size = round(0.7 * N))
#訓練資料與測試資料比例: 70%建模，30%驗證
#ktraindata <- newtrain[k_idx,]
#ktestdata <- newtrain[ - k_idx,]


install.packages("randomForest")
library(randomForest)
set.seed(1117)
modelRF <- randomForest(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday ,data=subset1,mtry=3,ntree=10,importance=T,do.trace=100)
pred = predict(modelRF, newdata=test)
print(modelRF)
print(pred)
round(importance(modelRF),2)
plot(modelRF)


train.rf=randomForest(Sales~StateHoliday,data=train,n.tree=50)
#sprint(train.rf)
library("caret")
train.model= train(Sales~StateHoliday, data=ktraindata, method="rf")
pred = predict(train.model, newdata=ktestdata)
print(pred)
str(train)
