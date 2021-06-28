
library("HSAUR3")
data("USairpollution")

food_varcov <- cov(USairpollution)
food_cor <- cor(USairpollution)

food_pc <- prcomp(USairpollution, 
                  center = TRUE,scale. = TRUE)





screeplot(food_pc, type="lines")         # Clearly first two E.values are the 
# only ones that are greater than 1.
# We hence set m=2 in factor analysis


#### PC based Method #####

food_eigen <- eigen(food_cor) 
L1_eigen <-   food_eigen$vector[,1]*(food_eigen$values[1])**0.5
L2_eigen <-   food_eigen$vector[,2]*(food_eigen$values[2])**0.5

## Specific variances
spec_eigen <- 1-(L1_eigen^2 + L2_eigen^2)

## Loading Matrix
L_eigen <- cbind(L1_eigen,L2_eigen)

## Through this method, we clearly don't see a 
## full recovery of the off diagonals of R. 
## Hence the need for the iterative method. 


#### MLE based Method ####

food_fa2 <- factanal(USairpollution, factors = 2, rotation="none")    
food_fa1 <- factanal(USairpollution, factors = 1, rotation="none")  


# Uniqueness: Specific variances
# Loadings: Factor Loadings 

food_fa1$uniquenesses
food_fa2$uniquenesses

# Compute communality

L2 <- food_fa2$loadings
Com2 <- L2 %*% t(L2)

# Recover the correlation matrix from the communality & specific factors

Decomp2 <- Com2 + diag(food_fa2$uniquenesses)
Decomp2
cor(USairpollution)

#### Rotation ####
## Varimax is the default ##
food_fa2_def <- factanal(USairpollution, factors = 2)
food_fa1_def <- factanal(USairpollution, factors = 1) 

food_fa2_vari <- factanal(USairpollution, factors = 2, rotation="varimax")  
food_fa1_vari <- factanal(USairpollution, factors = 1, rotation="varimax")  

## Rotation Matrix ($rotmat)

#### Varimax makes loadings either "large" or zero
varimax(food_fa2$loadings)

varimax(food_fa1$loadings)


#### Promax mainly retains the variance of the factors. 
promax(food_fa2$loadings)

promax(food_fa1$loadings)


## Plots
par(mfrow = c(1,2))
plot(food_fa2$loadings[,1], 
     food_fa2$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")

plot(food_fa2_vari$loadings[,1], 
     food_fa2_vari$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax")

#Question 3

L<-matrix(c(.1022, .0752, .0765), nrow = 3)

LLprime <-L %*% t(L)

S <-matrix(c(11.072,8.019, 8.160, 8.019, 6.417, 6.005, 8.160, 6.005, 6.773), nrow = 3)
S
