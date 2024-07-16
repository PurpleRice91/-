rm(list=ls())
merge1=readRDS(file = "c:/Users/user/Desktop/專題/merge1", refhook = NULL)
merge2=readRDS(file = "c:/Users/user/Desktop/專題/merge2", refhook = NULL)
subset1=readRDS(file = "c:/Users/user/Desktop/專題/subset1", refhook = NULL)
subset2=readRDS(file = "c:/Users/user/Desktop/專題/subset2", refhook = NULL)
test=readRDS(file = "c:/Users/user/Desktop/專題/test", refhook = NULL)
train=readRDS(file = "c:/Users/user/Desktop/專題/train", refhook = NULL)
store=readRDS(file = "c:/Users/user/Desktop/專題/store", refhook = NULL)
subset1.test=readRDS(file = "c:/Users/user/Desktop/專題/subset1.test", refhook = NULL)

library(lubridate)
library(gridExtra)
library(ggplot2) 
library(dplyr) 
library(scales) 
library(RColorBrewer) 
library(gridExtra) 
library(ggthemes)
library(caret)
library(randomForest)
library(mlbench)

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
#subset2 -> subset1.test
set.seed(1117)
subset2.RF <- randomForest(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday ,data=subset2,mtry=3,ntree=10,importance=T,do.trace=100)
pred.s1 = predict(subset2RF, newdata=subset1.test)
print(subset2RF)
print(pred.s1)
round(importance(subset2.RF),2)
plot(subset2.RF)

CP=cbind(subset1$Sales,pred.S2)
CP
Error=RMSPE(CP)
Error

# merge2 -> test
set.seed(1117)
merge.RF <- randomForest(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday ,data=merge2,mtry=3,ntree=10,importance=T,do.trace=100)
pred.test = predict(merge.RF, newdata=test)
print(merge.RF)
print(pred.test)
round(importance(merge.RF),2)
plot(merge.RF)

# 重要性
k <- train(train(Sales~., data=train, method="rf", preProcess="scale", trControl=control))

# 圖表
qplot(train$Date , train$Sales,color=2, geom="line")
qplot(merge1$Date , merge1$Sales,color=2, geom="line")


sample_submission.new=cbind(id=sample_submission$Id,Sales=pred.test)
qplot(pred.test, geom="histogram") 
ggplot(data=pred.test, aes(sample_submission.new$Id)) + geom_histogram()

