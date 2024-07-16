rm(list=ls())
train=read.csv("C:/Users/user/Desktop/專題/train.csv")
store=read.csv("C:/Users/user/Desktop/專題/store.csv")
test=read.csv("C:/Users/user/Desktop/專題/test.csv")
merge1=read.csv("c:/Users/user/Desktop/專題/merge1.csv")
sample_submission=read.csv("c:/Users/user/Desktop/專題/sample/sample_submission.csv")


merge1 = merge(train, store, all = T)
merge1$Year = 9
merge1$Month = 9
merge1$Day = 9
#install.packages("lubridate")
library(lubridate)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
merge1$Year = year(merge1$Date)
merge1$Month = month(merge1$Date)
merge1$Day = day(merge1$Date)
merge1 = subset(merge1, select = c(Store, DayOfWeek, date, Year, Month, Day, Sales, Customers, Open, Promo, StateHoliday,	SchoolHoliday, StoreType, Assortment,	CompetitionDistance, CompetitionOpenSinceMonth, CompetitionOpenSinceYear, Promo2,	Promo2SinceWeek, Promo2SinceYear, PromoInterval))



merge2$Early.month=0
merge2$Middle.month=0
merge2$Later.month=0
test$Early.month=0
test$Middle.month=0
test$Later.month=0

count = 1
while(count<=844338){
  if (merge2[count,5]<=10){
   merge2[count,21]=1
  }
  if (merge2[count,5]>=11&merge2[count,5]<=20){
    merge2[count,22]=1
  }
  if (merge2[count,5]>=21){
    merge2[count,23]=1
  }
  count <- count + 1
}
k = 1
while(k<=41088){
  if (test[k,5]<=10){
    test[k,10]=1
  }
  if (test[k,5]>=11&test[k,5]<=20){
    test[k,11]=1
  }
  if (test[k,5]>=21){
    test[k,12]=1
  }
  k <- k + 1
}


library(ggplot2) 
library(dplyr) 
library(scales) 
library(RColorBrewer) 
library(gridExtra) 
#install.packages("ggthemes")
library(ggthemes)
#install.packages("caret")
library(caret)


merge1$StoreType = factor(merge1$StoreType, levels = c('a', 'b', 'c','d'), labels = c(1, 2, 3,4))
merge1$StateHoliday = factor(merge1$StateHoliday, levels = c('0', 'a', 'b','c'), labels = c(0, 1, 2,3))
merge1$Assortment = factor(merge1$Assortment, levels = c('a', 'b', 'c'), labels = c(1, 2, 3))
train$StateHoliday = factor(train$StateHoliday, levels = c('0','a', 'b', 'c'), labels = c(0,1, 2, 3))
merge1$PromoInterval = factor(merge1$PromoInterval, levels = c('Jan,Apr,Jul,Oct', 'Feb,May,Aug,Nov', 'Mar,Jun,Sept,Dec'), labels = c(1, 2, 3))
test$StateHoliday = factor(test$StateHoliday, levels = c('0', 'a', 'b','c'), labels = c(0, 1, 2,3))

test$Year = 9
test$Month = 9
test$Day = 9
test$Year = year(test$Date)
test$Month = month(test$Date)
test$Day = day(test$Date)
test = subset(test, select = c(Store,DayOfWeek, Year, Month, Day, Open, Promo, StateHoliday,	SchoolHoliday))


merge2[is.na(merge2)] <- 0
merge1[is.na(merge1)] <- 0
test[is.na(test)] <- 0
train[is.na(train)] <- 0
store[is.na(store)] <- 0
#
which(merge1$Sales==0)
#install.packages("dplyr")
library(dplyr)
merge2=filter(merge1,Sales!='0')
merge1=filter(merge1,Sales!='0')
subset1=subset(merge2,Year>2014&Month>4, drop=F)
subset2=subset(merge2, (Year<2015&Month>0)|Year>2014&Month<5, drop=F)
subset1.test = subset(subset1, select = c(Store,DayOfWeek,Year,Month,Day,Open,Promo,StateHoliday,SchoolHoliday))
subset1.merge1.test = subset(subset1.merge1, select = c(Store,DayOfWeek,Year,Month,Day,Open,Promo,StateHoliday,SchoolHoliday))
merge2=filter(store,Sales!='0')
merge2=filter(merge1,Sales!='0')

m2013
m2014
m2015


subset1.merge1=subset(merge1,Year>2014&Month>4, drop=F)
subset2.merge1=subset(merge1, (Year<2015&Month>0)|Year>2014&Month<5, drop=F)

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

set.seed(1117)
subset2.merge1RF <- randomForest(Sales ~ DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday ,data=subset2.merge1,mtry=3,ntree=10,importance=T,do.trace=100)
pred.S2.merge1 = predict(subset2.merge1RF, newdata=subset1.merge1.test)
print(subset2.merge1RF)
round(importance(pred.S2.merge1),2)

set.seed(1117)
merge1.RF <- randomForest(Sales ~ DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday ,data=subset1.merge1,mtry=3,ntree=10,importance=T,do.trace=100)
pred.merge1.test = predict(merge1.RF, newdata=test)
print(merge1.RF)
round(importance(pred.merge1.test),2)

#subset2->subset1.test
#install.packages("randomForest")
library(randomForest)
set.seed(1117)
subset2RF <- randomForest(Sales ~ DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday ,data=subset2,mtry=3,ntree=10,importance=T,do.trace=100)
pred.S2 = predict(subset2RF, newdata=subset1.test)
print(subset2RF)
print(pred.S2)
round(importance(subset2RF),2)
plot(subset2RF)


RMSPE = function(A){
  Sum<-0
  for(i in 1:nrow(A)){
    if(A[i,1]==0){
      next
    }
    else{
      Sum <- Sum + ((A[i,1]-A[i,2])/A[i,1])^2
    }
    return(sqrt(Sum/nrow(A)))
  }
}

CP=cbind(subset1$Sales,pred.S2)
CP
Error=RMSPE(CP)
Error





# merge2 -> test
library(randomForest)
set.seed(1117)
merge.RF <- randomForest(Sales ~ DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday ,data=merge2,mtry=3,ntree=10,importance=T,do.trace=100)
pred.test = predict(merge.RF, newdata=test)
print(merge.RF)
print(pred.test)
ss=round(importance(merge.RF),2)
plot(merge.RF)
plot(ss)

CP2=cbind(DayOfWeek=test$DayOfWeek,Year=test$Year,Month=test$Month,Day=test$Day,Open=test$Open,Promo=test$Promo,StateHoliday=test$StateHoliday,SchoolHoliday=test$SchoolHoliday,pred.Sales=pred.test)
CP2


# 丟上去
sample_submission.new=cbind(id=sample_submission$Id,Sales=pred.test)
write.csv(sample_submission.new,"c:/Users/user/Desktop/專題/sample_submission.csv",row.names=F)


#儲存
saveRDS(merge1, file = "c:/Users/user/Desktop/專題/merge1", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(merge2, file = "c:/Users/user/Desktop/專題/merge2", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(subset1, file = "c:/Users/user/Desktop/專題/subset1", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(subset2, file = "c:/Users/user/Desktop/專題/subset2", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(subset1.merge1, file = "c:/Users/user/Desktop/專題/subset1.merge1", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(subset2.merge1, file = "c:/Users/user/Desktop/專題/subset2.merge1", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(test, file = "c:/Users/user/Desktop/專題/test", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(train, file = "c:/Users/user/Desktop/專題/train", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(store, file = "c:/Users/user/Desktop/專題/store", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(subset1.test, file = "c:/Users/user/Desktop/專題/subset1.test", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

#讀取
merge1=readRDS(file = "c:/Users/user/Desktop/專題/merge1", refhook = NULL)
merge2=readRDS(file = "c:/Users/user/Desktop/專題/merge2", refhook = NULL)
subset1=readRDS(file = "c:/Users/user/Desktop/專題/subset1", refhook = NULL)
subset2=readRDS(file = "c:/Users/user/Desktop/專題/subset2", refhook = NULL)
test=readRDS(file = "c:/Users/user/Desktop/專題/test", refhook = NULL)
train=readRDS(file = "c:/Users/user/Desktop/專題/train", refhook = NULL)
store=readRDS(file = "c:/Users/user/Desktop/專題/store", refhook = NULL)
subset1.test=readRDS(file = "c:/Users/user/Desktop/專題/subset1.test", refhook = NULL)
subset1.merge1=readRDS(file = "c:/Users/user/Desktop/專題/subset1.merge1", refhook = NULL)
subset2.merge1=readRDS(file = "c:/Users/user/Desktop/專題/subset2.merge1", refhook = NULL)


k <- train(train(Sales~., data=train, method="rf", preProcess="scale", trControl=control))

install.packages("mlbench")
library(mlbench)




qplot(train$Date , train$Sales,color=2, geom="line")
