# Let us get started
setwd("C:/Users/mreza6/Documents/R/ch2")
getwd()
ad <-read.table(file="Advertising.csv", header = T, sep=",")
head(ad)

# Exploratory Data Analysis
scatter.smooth(x=ad$TV, y = ad$sales, main = "Sales ~ TV")
boxplot(ad$TV)
hist(ad$TV, breaks = 30, main="Histogram: TV", xlab="TV", col='red')
plot(density(ad$TV), main="Density Plot: TV")
polygon(density(ad$TV), col = 'red')
cor(ad$sales, ad$TV) output [1] 0.7822244

# Build a simple linear regression
mod1 <- lm(sales ~ TV, data = ad)
summary(mod1)

# Is there a strong relationship between TV & Sales? Why?

# Residual standard error: 3.259 on 198 degrees of freedom
# Multiple R-squared:  0.6119,	Adjusted R-squared:  0.6099 
# F-statistic: 312.1 on 1 and 198 DF,  p-value: < 2.2e-16

# The regression coefficient p-values are close to zero and this the indication of a strong relationship between TV 
# and Sales. Besides, the R Squared indicates about 61 percent of the variation in the response variable Sales due 
# to the predictor variable TV. 

(mod1.coef <- coef(mod1))
# Output
# (Intercept)          TV 
#  7.03259355  0.04753664

mod1.coef["TV"]
#output
#        TV 
#0.04753664

mod1.coef["(Intercept)"]
# (Intercept) 
# 7.032594

#Plot the model fit, to save topdf, uncomment the pdf() and dev.off() lines
#pdf("lm_sales_TV.pdf")
plot(ad$TV, ad$sales, main="sales ~ TV", xlab="TV", ylab="sales", col='blue', pch=20)
abline(mod1, lwd=3)
#dev.off()

# Training and Predicting
set.seed(2019)
trainIndex <- sample(1:nrow(ad), 0.7*nrow(ad))
mod2 <- lm(sales ~ TV, data = ad, subset = trainIndex)
summary(mod2)

pred2 <- predict(mod2, newdata=ad[-trainIndex, ])
(testMSE <- mean((pred2 - ad[-trainIndex, 'sales'])^2))
#[1] 12.28906

(testMAPE <- mean(abs(pred2 - ad[-trainIndex, 'sales']/ad[-trainIndex, 'sales'])))
#[1] 13.81838

#Plot the prediction
plot(ad[-trainIndex,c('TV', 'sales')], col='red', pch="+", main="Test data")
abline(mod2, lwd=3)

#Plot the prediction with intervals
newx <- seq(min(ad[-trainIndex,'TV']), max(ad[-trainIndex,'TV']), length.out = 100)

#Confidence Interval
pred2.CI <- predict(mod2, newdata=data.frame(TV=newx), interval = "confidence")
plot(ad[-trainIndex,c('TV','sales')], type ='n', main = "Regression line with CI")
polygon(c(rev(newx), newx), c(rev(pred2.CI[,'upr']), pred2.CI[,'lwr']), col='grey80', border=NA)
abline(mod2, lwd=2)
points(ad[-trainIndex,c('TV','sales')], col='red', pch="+")
legend("bottomright", legend=c("Test Data", "Model"), col=c("red","black"), pch = c("+",NA), lwd = c(0,3))

#Prediction Interval
pred2.CI <- predict(mod2, newdata=data.frame(TV=newx), interval = "prediction")
plot(ad[-trainIndex,c('TV','sales')], type ='n', main = "Regression line with PI")
polygon(c(rev(newx), newx), c(rev(pred2.CI[,'upr']), pred2.CI[,'lwr']), col='grey80', border=NA)
abline(mod2, lwd=2)

# Exercise: 
# Add training data points to the above plot and in the kegend
points(ad[trainIndex,c('TV','sales')], col='red', pch="+")
points(ad[-trainIndex,c('TV','sales')], col='blue', pch=20)
legend("bottomright", legend=c("Train Data", "Test Data", "Model"), col=c("blue", "red","black"), pch = c(".", "+",NA), lwd = c(0,0,3))

# Exercise
# Build a multiple linear regression model and calculate the testMSE
mod3 <- lm(sales ~ TV + radio + newspaper, data = ad, subset = trainIndex)
summary(mod3)

pred4 <- predict(mod3, newdata=ad[-trainIndex, ])
(testMSE <- mean((pred4 - ad[-trainIndex, 'sales'])^2))
# [1] 2.580169