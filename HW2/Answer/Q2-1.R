library(aod)
library(ggplot2)

mydata <- read.csv("salmon.csv")
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
#convert rank to a factor to indicate that gender should be treated as a categorical variable.
mydata$gender <- factor(mydata$gender)
mylogit <- glm(country ~ gender + fresh + marine, data = mydata, family = "binomial")
summary(mylogit)

#conditional density
fac_country <- factor(mydata$country)

cdplot(fac_country ~ mydata$fresh)
cdplot(fac_country ~ mydata$marine)

#scatter plot

mydata$mu_hat1 <- predict(mylogit, newdata=data.frame(mydata[,2:4]), type="response")

mydata$DevRes1 <- ifelse(mydata$country==0, 
                             2*log(
                               (1-mydata$country)/(1-mydata$mu_hat1), base=exp(1)), 
                             2*log(
                               mydata$country/mydata$mu_hat1,base=exp(1))
)


plot(mydata$DevRes1, mydata$mu_hat1, main = "Deviance Residuals vs Predicted Values", xlab = "Predicted Values", ylab = "Deviance Residuals")
#deviance residual

plot(density(resid(mylogit, type='deviance')))
scatter.smooth(predict(mylogit, type='response'), rstandard(mylogit, type='deviance'), col='gray')
