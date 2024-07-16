install.packages("AER")
library(AER)
data(CreditCard)
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
library("rpart")
library("rpart.plot")
library("rattle")

bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner, months))
bankcard$card <- ifelse(bankcard$card == "yes", 1, 0);

#(2)測試模型
#取得總筆數
n <- nrow(bankcard)
#設定隨機數種子
set.seed(1117)
#將數據順序重新排列
newbankcard <- bankcard[sample(n),]
