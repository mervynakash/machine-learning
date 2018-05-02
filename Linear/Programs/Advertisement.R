library(ISLR)
library(caret)
library(data.table)
library(dplyr)

set.seed(123)

setwd("E:/Machine Learning/Regression/Linear/Datasets/")

ad <- fread("Advertising.csv", header = T)

colnames(ad)
ad <- ad %>% select(TV,radio,newspaper,sales)

# Checking Missing Values
colSums(is.na(ad))*100/nrow(ad)

# Checking Outliers
boxplot(ad$newspaper)
boxplot(ad$TV)
boxplot(ad$radio)


# Splitting the values
# Method 1
df_train <- ad[1:162,]
df_test <- ad[163:200,]

# Method 2 - Without Replacement
rn <- sample(seq(1,nrow(ad)), 162)
traindf <- ad[rn,]
testdf <- ad[-rn,]

# Method 3 - With Replacement
trdf <- ad[sample(seq(1,nrow(ad)),162),]
tedf <- ad[sample(seq(1,nrow(ad)), 38),]

# Using caret package
trainIndex <- createDataPartition(ad$sales, p = .8, list = F, times = 1)

trainAd <- ad[trainIndex,]
testAd <- ad[-trainIndex,]

###############
# Feature Selection
# Using all input variables --> TV, Radio, Newspaper

###############
# Model Fitting

ad_model <- lm(sales~TV+radio+newspaper, data = trdf)
summary(ad_model)

###############
# Prediction
pred_sales <- predict(ad_model, tedf[,c("TV","radio","newspaper")])
tedf$pred_sales <- pred_sales


###############
# Calculating Error for all the rows
tedf$error <- tedf$sales - tedf$pred_sales
tedf$sqerror <- tedf$error ^ 2

sum(tedf$sqerror)

plot(tedf$sales, type = "l")
lines(tedf$pred_sales, col = "red")


 
####################################################################################
# Personal Notes

model <- lm(sales~TV+radio+newspaper, data = trdf)

summary(model)

model2 <- lm(sales~TV+radio, data = trdf)
summary(model2)

tr <- predict(model2, tedf)
tedf$tr <- tr

tedf$sqrerr <- (tedf$sales - tedf$tr) ^ 2
sum(testAd$sqrerr)

anova(model, model2, test = "Chisq")

mod1 <- train(sales~TV+radio+newspaper, data = trdf, method = "lm")
mod2 <- train(sales~TV+radio, data = trdf, method = "lm")

results <- resamples(list(LM1 = mod1, LM2 = mod2))
summary(results)

bwplot(results)
densityplot(results)