set.seed(123)
setwd("E:/Machine Learning/Regression/Linear/Datasets/BankMarketting/")

library(dplyr)
library(tree)
library(rpart)

bank <- read.csv2("bank.csv", header = T)

# View(bank)

str(bank)

bk <- bank %>% select_if(is.numeric)

# Missing values
colSums(is.na(bank))

# Outliers
sapply(bk, function(x){length(boxplot.stats(x)$out)*100/length(x)})

# Split
banktrain <- bank[sample(seq(1,nrow(bank)), 3616),]
banktest <- bank[sample(seq(1,nrow(bank)),4521-3616),]

bankmodel <- tree(y~., data = banktrain)
summary(bankmodel)
plot(bankmodel);text(bankmodel)

bm <- rpart(y~., data = banktrain)
summary(bm)
plot(bm);text(bm)

bank_pre <- as.data.frame(predict(bankmodel, banktest %>% select(-y)))
bank_pre$predict <- if_else(bank_pre$no>bank_pre$yes, "no","yes")
View(bank_pre)
table(bank_pre$predict)
table(banktest$y)

banktest$predict <- bank_pre$predict
accuracy <- sum(banktest$y == banktest$predict)/nrow(banktest) * 100
