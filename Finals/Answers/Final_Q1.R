#logistics model

library(aod)
library(ggplot2)
library(tidyr)

mydata <- read.csv("salmon_full.csv")
drop_na(mydata)
mydata$X <- NULL
mydata$country[mydata$country==2] <- 0
## view the first few rows of the data
head(mydata)
summary(mydata)
#To get the standard deviations, we use sapply to apply the sd function to each variable in the dataset.
sapply(mydata, sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~country + gender, data = mydata)

#mydata <- subset(mydata, gender != 3)

mydata$gender <- factor(mydata$gender)
mydata$country <- factor(mydata$country)

mylogit <- glm(country ~ gender + fresh + marine + weight + length, data = mydata, family = "binomial")
summary(mylogit)

# #conditional density
fac_country <- factor(mydata$country)
cdplot(fac_country ~ mydata$fresh)
cdplot(fac_country ~ mydata$marine)
cdplot(fac_country ~ mydata$weight)
cdplot(fac_country ~ mydata$length)
cdplot(fac_country ~ mydata$gender)

# #scatter plot
# 
# mydata$mu_hat1 <- predict(mylogit, newdata=data.frame(mydata[,2:6]), type="response")
# 
# mydata$DevRes1 <- ifelse(mydata$country==0, 
#                          2*log(
#                            (1-mydata$country)/(1-mydata$mu_hat1), base=exp(1)), 
#                          2*log(
#                            mydata$country/mydata$mu_hat1,base=exp(1))
# )
# 
# 
# plot(mydata$DevRes1, mydata$mu_hat1, main = "Deviance Residuals vs Predicted Values", xlab = "Predicted Values", ylab = "Deviance Residuals")
# #deviance residual
# 
# plot(density(resid(mylogit, type='deviance')))
# scatter.smooth(predict(mylogit, type='response'), rstandard(mylogit, type='deviance'), col='gray')

############## Tree and Random Forest ##################

# load the package used for building a classification tree
library(tree)

set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(mydata), alpha * nrow(mydata))
train.set <- mydata[inTrain,]
test.set  <- mydata[-inTrain,]

train.set$country = as.factor(train.set$country)
train.set$gender = as.factor(train.set$gender)
#convert to factor to run tree model
class(train.set$country)
class(train.set$gender)

# plot and label the classification tree

tree.model <- tree(country ~ gender + fresh + marine + weight + length, data=train.set)
tree.model
summary(tree.model)

plot(tree.model)
text(tree.model)

# #faster
# library(rpart)
# rpart.tree <- rpart(country ~ ., data=train.set)
# plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
# text(rpart.tree, all=TRUE, use.n=TRUE)
# title("Training Set's Classification Tree")

# build the random forest out of the training set. Predict Grade based on all other variables

train_rf <- randomForest(country ~.,data=train.set,importance=T)

# print the output of the random forest. What is the classification rate you get?

print(train_rf)

# return and plot the importance of each predictor variable in the random forest

importance(train_rf)
varImpPlot(train_rf)


# predict the grades for the houses in the testing set, based on the forest built from the training set, and compute the classification rate

test_pred <- predict(train_rf,newdata = test.set)
table_pred <- table(test_pred, test.set$country)

test_accuracy<- mean(test_pred == test.set$country)


############# Linear Discriminant ##############

library("MASS")
library(boot)
library(dplyr)
library(magrittr)
library(car)

mydata <- na.omit(mydata)

mydata.lda <- lda(country ~ ., data=mydata)
mydata.lda


mydata.lda.values <- predict(mydata.lda)
mydata.lda.values$class

sum(mydata$country==mydata.lda.values$class)/length(mydata$country)

ldahist(data = mydata.lda.values$x[,1], g=Type)
