# install and load the Random Forest package
install.packages("randomForest")
library(randomForest)

# Question 3

# install and load the package containing the Ames Housing data
install.packages("AmesHousing")
library("AmesHousing")


# the first function is from the AmesHousing package and loads a nice version of the data
# the second is the columsn we want to consider
# the third picks out only the columns we want to consider and omits rows that contain NAs
ames_ord <- make_ordinal_ames()
colnames <- c("Sale_Price", "Lot_Frontage", "Lot_Area", "Bldg_Type", "House_Style", "Overall_Qual", "Overall_Cond", "Year_Built", "Roof_Style", "Roof_Matl", "Exterior_1st", "Exter_Qual", "Foundation", "Bsmt_Cond", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Heating", "Central_Air", "Electrical", "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath", "Bedroom_AbvGr", "Kitchen_AbvGr", "Kitchen_Qual", "Fireplaces", "Garage_Cars", "Garage_Area", "Fence", "Misc_Val")
small_ames_ord <- na.omit(ames_ord[colnames])

# split the data into a testing set and a training set
# ADD CODE HERE

# build the random forest out of the training set. Predict Sale_Price based on all other variables
# ADD CODE HERE

# print the output of the random forest. What is the MSE you get?
# ADD CODE HERE

# return and plot the importance of each predictor variable in the random forest
# ADD CODE HERE

# predict the sales prices for the houses in the testing set, based on the forest built from the training set, and compute the MSE
# ADD CODE HERE




# Question 4

# load the file containing the grades data. Add the filename location if you need to
library(readr)
Grades <- read_csv("Grades.csv")

# load the package used for building a classification tree
library(tree)

# build the classification tree for the grades data
# ADD CODE HERE
# see http://www.di.fc.ul.pt/~jpn/r/tree/tree.html#classification-trees for example code

# plot and label the classification tree 
# see the above website for the code

# split the data into a testing set and a training set
# ADD CODE HERE

# build the random forest out of the training set. Predict Sale_Price based on all other variables
# ADD CODE HERE

# print the output of the random forest. What is the classification rate you get?
# ADD CODE HERE

# return and plot the importance of each predictor variable in the random forest
# ADD CODE HERE

# predict the grades for the houses in the testing set, based on the forest built from the training set, and compute the classification rate
# ADD CODE HERE

