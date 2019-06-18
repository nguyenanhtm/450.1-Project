# CLASS PROJECT: WINE QUALITY - RED

#1. ACCESS THE DATA SET
winequality <- read.csv(file.choose())
head(winequality)
summary(winequality)
str(winequality)

#2. DATA VISUALIZATION
hist(winequality$fixed.acidity, main="Histogram of Fixed Acidity", 
     xlab="Fixed Acidity", col="lightblue")
#most wine has fixed acid level between 6 and 10.

hist(winequality$volatile.acidity, main="Histogram of Volatile Acidity", 
     xlab="Volatile Acidity", col="lightblue")
#most wine ranges from 0.3 to 0.7. Notice that at too high level lead to unpleasant taste

hist(winequality$citric.acid, main = "Histogram of Citric Acid", xlab = "Citric Acid",
     col="lightblue")
#citric acid adds freshness and flavor to wine 

hist(winequality$residual.sugar,main = "Histogram of Residual Sugar", xlab = "Residual Sugar",
     col="lightblue")
#there is no wine with less than 1 gram/liter. Because wines with greater than 45 
#grams/liter are considered sweet, and there is no wines with residual sugar >45, 
#these wines are not sweet

hist(winequality$chlorides,main = "Histogram of Chlorides", xlab = "Chlorides", 
     col="lightblue")
#the vast majority of wines have chloride between 0.05-0.1

hist(winequality$free.sulfur.dioxide,main = "Histogram of Free Sulfure Dioxide", 
     xlab = "Free Sulfurere Dioxide", col="lightblue")
#most lie between 5-10, then frequency slowly falls as free SO2 level rises
#there is one column at 50-55, and one at 65-70, which means these wines have SO2 that 
#inferes with the taste of wine
#contributes to total SO2

hist(winequality$total.sulfur.dioxide,main = "Histogram of Total Sulfure Dioxide", 
     xlab = "Total Sulfurere Dioxide", col="lightblue")
#has similar shape to the free SO2 histogram. 

hist(winequality$density,main = "Histogram of Density", xlab = "Density", col="lightblue")

hist(winequality$pH,main = "Histogram of pH", xlab = "pH", col="lightblue")
#most wines are between 3 and 4 on the pH scale

hist(winequality$sulphates,main = "Histogram of Sulphates", xlab = "Sulphates", 
     col="lightblue")
#contributes to total SO2 level

hist(winequality$alcohol,main = "Histogram of Alcohol", xlab = "Alcohol", 
     col="lightblue")

hist(winequality$quality, main = "Histogram of Quality", xlab = "Quality", 
     col="lightblue")

pairs(winequality[,c(1:11)],col=as.factor(winequality[,12]),pch=20)

boxplot(winequality, main="Wine Quality", xaxt='n', xlab = '')
text(x =  seq_along(colnames(winequality)), y = par("usr")[3] - 1, srt = 45, adj = 1,
     labels = colnames(winequality), xpd = TRUE)

#3. MULTIPLE LINEAR REGRESSION
#split data set
n <- nrow(winequality)
ntrain <- round(n*0.6)
set.seed(314)
tindex <- sample(n, ntrain)
train_wq <- winequality[tindex,]
test_wq <- winequality[-tindex,]

#predict quality score in test set using train model
lm1 <- lm(quality~., data=train_wq)
summary(lm1)
#variables that are statistically significant(***): volatile acidity, chlorides, 
#free sulfur dioxide, total sulfur dioxide, sulphates, alcohol

predict1 <- predict(lm1, newdata = test_wq)
cor(predict1, test_wq$quality)
# correlation is about 0.60. Not very high correlation. 
#Let's try with only those with significant impact

lm2 <- lm(quality ~volatile.acidity + chlorides + free.sulfur.dioxide +
            total.sulfur.dioxide + sulphates + alcohol, data=train_wq)
summary(lm2)
predict2 <- predict(lm2, newdata = test_wq)
cor(predict2, test_wq$quality)
# only a little bit better. We see that free SO2 and total SO2 might be interdependent. 
# so try only total sulfur dioxide

lm3 <- lm(quality ~volatile.acidity + chlorides +
            total.sulfur.dioxide + sulphates + alcohol, data=train_wq)
summary(lm3)
predict3 <- predict(lm3, newdata = test_wq)
cor(predict3, test_wq$quality)
# clearly better. 
plot(lm3)
vif(lm3)
#Normal Q-Q: residuals are normally distributed
#Residuals vs. Leverage: no influential case
#Residuals vs Fitted: linear relationship
#Scale-Location: line is not horizontal, so residuals is not equally spread -> caution

#RMSE
rmse <- function(y_hat, y)
{
  return(sqrt(mean((y_hat-y)^2)))
}
rmse_train <- rmse(predict(lm3),train_wq$quality)
rmse_train

rmse_test <- rmse(predict(lm3, newdata = test_wq), test_wq$quality)
rmse_test

#R-squared
