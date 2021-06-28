
#An example that has been cited a number of times is the Pitprop data of Jeffers (1967).
#Pitprops are the timbers used to shore up passageways in mines and these data were taken 
#from a study to determine whether Corsican pine grown locally was sufficientlystrong for the purpose. 
#Physical measurements (p = 13) relating to size, density, rings, knots, and so on, 
#were obtained on n = 180 pitprops. This example was a bit “sandier” than those in the previous section, 
#thefirstfivepc’sonlyaccountingforabout80% ofthetraceofthecorrelation matrix. 
#The average root rule would have retained only the first four pc’s (74%); 
#the first pc accounted for only 32%. Nevertheless, this is typical of some PCA studies 
#and the first few pc’s do have reasonable interpretation.

#install.packages("elasticnet")

dat <- read.csv("HW3_spca.csv")
pitprops <- cor(dat[,-1])

require(elasticnet)

#data(pitprops)   # This data set is a 13 by 13 correlation matrix of the original dataset with 180 observations and 13 variables. 
print(pitprops)

## PCA on pitprops (Use princomp or eigen instead of prcomp as princomp allows you to input the correlation matrix directly)
PP_pca <- princomp(covmat = pitprops) 

summary(PP_pca)

names(PP_pca)

# PC scores
PP_pca$scores  ## This returns 'NULL' as we don't have the original X matrix to compute PC scores. (recall: Y = Xe) 
## What we have is just the correlation matrix, which is not enough to compute the PC scores. 

# Eigenvectors
PP_pca$loadings

# Square roots of Eigenvalues are saved in object 'sdev'
PP_pca$sdev

# Eigen values are squares of 'sdev'
PP_pca$sdev**2

## Check Sum of all Eigen values should equal 13 (dimension of the correlation matrix)
## The second line below gives the trace of the correlation matrix that we initially input which is also 13. 
PP_pca$sdev %*% PP_pca$sdev
sum(diag(pitprops))

# Percentage of variation explained by each PC
Perc_Var <-  PP_pca$sdev**2/(PP_pca$sdev %*% PP_pca$sdev)
Perc_Var

# Cumulative variaton explained by successive PCs (We see that about 90% is explained by the first 7 PCs.)
cumsum(Perc_Var)

#### We now use SPCA to make these first few PCs sparse and hence easy to interpret. 

# SPCA
## K = number of components (user specified)
## type = "Gram" (we provide the covariance or correlation matrix)
## lambda = Quadratic penalty parameter. Default value is 1e-6. Can leave this command out. 

#If sparse="penalty", para is a vector of L1-norm penalty parameters. Let's ignore this for now. 
PP_spca_penalty <- spca(pitprops,K = 7, type = "Gram", sparse = "penalty", para=c(0.06,0.16,0.1,0.5,0.5,0.5,.4))

PP_spca_penalty <- spca(pitprops,K = 7, type = "Gram", sparse = "penalty", para=c(0.5,0.5,0.5,0.5,0.5,0.5,.4))


### Loadings
PP_spca_penalty$loadings

#If sparse="varnum", para defines the number of sparse loadings (or number of non-zero loading values) to be obtained in each PC. Let's use this for now. 
## Play around with the numbers in the numbers specified in para and see what happens to the loadsings. 
PP_spca_varnum <- spca(pitprops,K = 7, type = "Gram", sparse = "varnum", para = c(7,4,4,2,1,1,1))

### Loadings
PP_spca_varnum$loadings

