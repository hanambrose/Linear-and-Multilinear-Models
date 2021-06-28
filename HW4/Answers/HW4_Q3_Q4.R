# install and load the Random Forest package
#install.packages("randomForest")
library(randomForest)

# Question 3

# install and load the package containing the Ames Housing data
#install.packages("AmesHousing")
library("AmesHousing")


# the first function is from the AmesHousing package and loads a nice version of the data
# the second is the columsn we want to consider
# the third picks out only the columns we want to consider and omits rows that contain NAs
ames_ord <- make_ordinal_ames()
colnames <- c("Sale_Price", "Lot_Frontage", "Lot_Area", "Bldg_Type", "House_Style", "Overall_Qual", "Overall_Cond", "Year_Built", "Roof_Style", "Roof_Matl", "Exterior_1st", "Exter_Qual", "Foundation", "Bsmt_Cond", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Heating", "Central_Air", "Electrical", "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath", "Bedroom_AbvGr", "Kitchen_AbvGr", "Kitchen_Qual", "Fireplaces", "Garage_Cars", "Garage_Area", "Fence", "Misc_Val")
small_ames_ord <- na.omit(ames_ord[colnames])

# split the data into a testing set and a training set

AmesHousing_Split <- sample(2,nrow(small_ames_ord),replace=TRUE,prob=c(0.7,0.3))
AmesHousing_trainData <- small_ames_ord[AmesHousing_Split==1,]
AmesHousing_testData <- small_ames_ord[AmesHousing_Split==2,]

# build the random forest out of the training set. Predict Sale_Price based on all other variables

AmesHousing_train_rf <- randomForest(Sale_Price ~.,data=AmesHousing_trainData,importance=T)

# print the output of the random forest. What is the MSE you get?

print(AmesHousing_train_rf)

# return and plot the importance of each predictor variable in the random forest
# plot the importance graphically

importance(AmesHousing_train_rf)
varImpPlot(AmesHousing_train_rf)

# predict the sales prices for the houses in the testing set, based on the forest built from the training set, and compute the MSE
# ADD CODE HERE

housing_pred <- predict(AmesHousing_train_rf,newdata = AmesHousing_testData)
table(housing_pred, AmesHousing_testData$Sale_Price)

#MSE
mean((AmesHousing_testData$Sale_Price - housing_pred)^2) 




# Question 4

# load the file containing the grades data. Add the filename location if you need to
library(readr)
Grades <- read_csv("Grades.csv")

# load the package used for building a classification tree
library(tree)

# build the classification tree for the grades data
# see http://www.di.fc.ul.pt/~jpn/r/tree/tree.html#classification-trees for example code

# split the data into a testing set and a training set
set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(Grades), alpha * nrow(Grades))
train.set <- Grades[inTrain,]
test.set  <- Grades[-inTrain,]

#train.set[1:4]<-lapply(train.set[1:4], as.numeric)

train.set$Grade = as.factor(train.set$Grade)
#convert to factor to run tree model
class(train.set$Grade)

# plot and label the classification tree
library(tree)

tree.model <- tree(Grade ~ ., data=train.set)
tree.model
summary(tree.model)

plot(tree.model)
text(tree.model)

#faster
# library(rpart)
# 
# rpart.tree <- rpart(Grade ~ ., data=train.set)
# plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
# text(rpart.tree, all=TRUE, use.n=TRUE)
# title("Training Set's Classification Tree")


# build the random forest out of the training set. Predict Grade based on all other variables

Grade_train_rf <- randomForest(Grade ~.,data=train.set,importance=T)

# print the output of the random forest. What is the classification rate you get?

print(Grade_train_rf)

# return and plot the importance of each predictor variable in the random forest

importance(Grade_train_rf)
varImpPlot(Grade_train_rf)

# predict the grades for the houses in the testing set, based on the forest built from the training set, and compute the classification rate

grade_test_pred <- predict(Grade_train_rf,newdata = test.set)
table_pred <- table(grade_test_pred, test.set$Grade)

accuracy_m1 <- mean(grade_test_pred == test.set$Grade)

#imbalance class

round(prop.table(table(Grades$Grade)),2)
#imbalance_rf <- randomForest(Grade ~.,data=train.set,importance=T, classwt=c(.2,.2,.1,.1,.1,.05,.05,.05,.05))
imbalance_rf <- randomForest(Grade ~.,data=train.set,importance=T, classwt=c(.8,.8,.5,.5,.1,.1,.1,.1,.1))
print(imbalance_rf)

imbalance_pred <- predict(imbalance_rf,newdata = test.set)
table_pred <- table(imbalance_pred, test.set$Grade)

accuracy_imbalance <- mean(imbalance_pred == test.set$Grade)
