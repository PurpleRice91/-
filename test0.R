rm(list=ls())
train=read.csv("C:/Users/user/Desktop/專題/train.csv")
store=read.csv("C:/Users/user/Desktop/專題/store.csv")
test=read.csv("C:/Users/user/Desktop/專題/test.csv")
merge1=read.csv("c:/Users/user/Desktop/專題/merge1.csv")
sample_submission=read.csv("c:/Users/user/Desktop/專題/sample/sample_submission.csv")


#variable
mtry=3
n.tree=10
dotrace=100

#setting
merge1 = merge(train, store, all = T)
merge1$Year = 9
merge1$Month = 9
merge1$Day = 9

install.packages("lubridate")
library(lubridate)
merge1$Year = year(merge1$Date)
merge1$Month = month(merge1$Date)
merge1$Day = day(merge1$Date)
merge1 = subset(merge1, select = c(Store, DayOfWeek, Year, Month, Day, Sales, Customers, Open, Promo, StateHoliday,	SchoolHoliday, StoreType, Assortment,	CompetitionDistance, CompetitionOpenSinceMonth, CompetitionOpenSinceYear, Promo2,	Promo2SinceWeek, Promo2SinceYear, PromoInterval))

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
test = subset(test, select = c(DayOfWeek, Year, Month, Day, Open, Promo, StateHoliday,	SchoolHoliday))


#
install.packages("dplyr")
library(dplyr)
merge2=filter(merge1,Sales!='0')
subset1=subset(merge2,Year>2014&Month>4, drop=F)
subset2=subset(merge2, (Year<2015&Month>0)|Year>2014&Month<5, drop=F)
subset1.test = subset(subset1, select = c(Store,DayOfWeek,Year,Month,Day,Open,Promo,StateHoliday,SchoolHoliday))

#
install.packages("randomForest")
library(randomForest)

#subset2->subset1.test
library(randomForest)
set.seed(1117)
subset2RF <- randomForest(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday ,data=subset2,mtry=3,ntree=10,importance=T,do.trace=100)
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


# merge1 -> test
library(randomForest)
set.seed(1117)
merge.RF <- randomForest(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday ,data=merge1,mtry=mtry,ntree=n.tree,importance=T,do.trace=dotrace)
pred.test = predict(merge.RF, newdata=test)
print(merge.RF)
print(pred.test)
round(importance(merge.RF),2)
plot(merge.RF)

CP2=cbind(DayOfWeek=test$DayOfWeek,Year=test$Year,Month=test$Month,Day=test$Day,Promo=test$Promo,StateHoliday=test$StateHoliday,SchoolHoliday=test$SchoolHoliday,pred.Sales=pred.test)
CP2


sample_submission.new=cbind(id=sample_submission$Id,Sales=pred.test)
write.csv(sample_submission.new,"c:/Users/user/Desktop/專題/sample_submission.csv",row.names=F)