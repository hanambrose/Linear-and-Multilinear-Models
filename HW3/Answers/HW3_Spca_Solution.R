library(elasticnet)

X_csv <- read.csv("**/HW3_spca.csv")

head(X_csv)
## You may notice that it has added a column of row numbers and called that column X. 
## Depending on whether you use this column or not, you may get slightly different 
## results - but they shouldn't be too different. 

X <- X_csv[,-1]

## Ignoring the first column. 
X_pc <- prcomp(X,center = TRUE,scale. = TRUE)

X_pc$rotation 
# We notice that there is a clear seperation in the PC loading values across the first 3 PCs. 
# The first 4 columns: V1-V4 vs V5-V8 vs V9-V10
# This is also borne out with the percent variation explained. 

X_Percent_Variation <- round(X_pc$sdev**2/sum(X_pc$sdev**2),4)
# The first 3 PCs explain most of the variation. 

corX <- cor(X)

## Try sparse PCA now, with 3 PCs, with 4 variables involved in the first two and 2 in the second. 

X_spca <- spca(corX,K = 3, type = "Gram", sparse = "varnum", para = c(4,4,2))

X_spca$loadings  #<- This reinforces what we noticed with regular PCs. 


