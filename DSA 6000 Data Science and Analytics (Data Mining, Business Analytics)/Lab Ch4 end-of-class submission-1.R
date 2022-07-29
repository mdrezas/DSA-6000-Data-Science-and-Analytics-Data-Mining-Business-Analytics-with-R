# Md Reza
# Lab Ch4 end-of-class submission
# Due Date: 10-20-2019


# Set work directory
setwd("C:/Users/mreza6/Documents/R/exercise_week_7")
getwd()

#Load the library
library (ISLR)
names(Smarket)
dim(Smarket)
summary (Smarket)
pairs(Smarket)

# produces a matrix that contains all of the pairwise correlations among the predictors in a data set
cor(Smarket [,-9])
attach (Smarket)
plot(Volume)


## 4.6.2 Logistic Regression ##
###################################################################################################

# fit a logistic regression model in order to predict Direction using Lag1 through Lag5 and Volume.
glm.fits=glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,data=Smarket ,family =binomial)
summary (glm.fits)

# use the coef() function in order to access just the coefficients for this fitted model
coef(glm.fits)
summary (glm.fits)$coef

# predict the probability that the market will go up
glm.probs = predict(glm.fits, type ="response")
glm.probs [1:10]

#Use contrasts() function
contrasts (Direction)

# create a vector of class predictions based on whether the predicted probability of a market increase is greater than or less than 0.5.
#creates a vector of 1,250 Down elements
glm.pred=rep ("Down" ,1250)

#transforms to Up all of the elements for which the predicted probability of a market increase exceeds 0.5.
glm.pred[glm.probs >.5]="Up"


#produce a confusion matrix
table(glm.pred,Direction) 
(507+145)/1250

mean(glm.pred == Direction)

# create vestor observing 2001 - 2004 & create helo out data set of 2005
train =(Year <2005)
Smarket.2005= Smarket [!train,]
dim(Smarket.2005)

Direction.2005= Direction [!train]

# logistic regression model using only the subset of the observations that correspond to dates before 2005
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=Smarket ,family =binomial ,subset =train )
glm.probs =predict (glm.fits,Smarket.2005 , type="response")

# compute the predictions for 2005 and compare them to the actual movements of the market over that time period.
glm.pred=rep ("Down" ,252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred ,Direction.2005)
mean(glm.pred== Direction.2005)
mean(glm.pred!= Direction.2005)

# refit the logistic regression using just Lag1 and Lag2

glm.fits=glm(Direction~Lag1+Lag2 ,data=Smarket ,family =binomial ,subset =train)
glm.probs =predict (glm.fits,Smarket.2005 , type="response")
glm.pred=rep ("Down" ,252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred ,Direction.2005)
mean(glm.pred== Direction.2005)
106/(106+76)

#predict Direction on a day when Lag1 and Lag2 equal 1.2 and 1.1 respectively, and on a day when they equal 1.5 and −0.8
predict(glm.fits,newdata =data.frame(Lag1=c(1.2 ,1.5) ,Lag2=c(1.1 , -0.8) ),type ="response")

detach(Smarket)


## 4.6.5 K-Nearest Neighbors ##
###################################################################################################
# bind the Lag1 and Lag2 variables together into two matrices
library (class)
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind (Lag1 ,Lag2)[!train ,]
train.Direction =Direction [train]

# set seed in order to ensure reproducibility of results
set.seed (1)
knn.pred=knn (train.X,test.X,train.Direction ,k=1)
table(knn.pred ,Direction.2005)
Direction.2005
(83+43)/252

# repeat the analysis using K = 3 so that the results would improve slightly.
knn.pred=knn (train.X,test.X,train.Direction ,k=3)
table(knn.pred ,Direction.2005)

mean(knn.pred== Direction.2005)
