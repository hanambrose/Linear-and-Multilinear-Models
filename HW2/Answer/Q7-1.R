library(MASS)
library(matrixcalc)
library(car)
library(mixtools)
library(mvtnorm)


m <- matrix(c(0,25), nrow=2) #mean

r <- matrix(c(1,0.9,0.9,1), nrow=2, byrow=T) # correlation matrix
is.positive.definite(r)
s11 <- 3
s22 <- 6



sig <- sqrt(diag(c(s11,s22))) %*% r %*% sqrt(diag(c(s11,s22))) #sigma
set.seed(54321)
Obs_X <- mvrnorm(n=100, mu = m, Sigma = sig) 
rownames(Obs_X) <- c(1:100)
Obs_X <- Obs_X[,1:2]
Obs_S <- cov(Obs_X)
Obs_X_Scaled <- scale(Obs_X,center = TRUE, scale = TRUE)

## extract principal components of normalized data:

pc_fit_normal <- prcomp(Obs_X, 
                        center = TRUE,scale. = TRUE)

## extract principal components of unscaled data:

pc_fit_normal_us <- prcomp(Obs_X) 


# PC1, PC2, PC3: PCA_scores
pc_normal_scores <- pc_fit_normal$x
pc_normal_scores_us <- pc_fit_normal_us$x

# PC loadings: Eigenvectors
pc_normal_loadings <- pc_fit_normal$rotation
pc_normal_loadings_us <- pc_fit_normal_us$rotation

## Computing the percentage of variation explained
Pct_Var_normal <- pc_fit_normal$sdev**2/sum(pc_fit_normal$sdev**2)
Pct_Var_normal_us <- pc_fit_normal_us$sdev**2/sum(pc_fit_normal_us$sdev**2)



## Sample principal components and ellipses of constant distance from mean 
## Unscaled data
plot(Obs_X[,1],Obs_X[,2],  xlab=expression(x[1]), ylab=expression(x[2]), 
     #xlim=c(-2,10),ylim=c(-2,10),
     xlim=c(-10,10),ylim=c(15,35),
     type='p',col="black",bty="n", font.main = 1, main="PCs for Bivariate Normal Data - Unscaled Data") 
abline(h=mean(Obs_X[,2]), v=mean(Obs_X[,1]),lty=2, lwd=2, col="red")
arrows(x0=mean(Obs_X[,1]),y0=mean(Obs_X[,2]),
       x1=mean(Obs_X[,1])+2*pc_fit_normal_us$sdev[1]*pc_fit_normal_us$rotation[1,1],
       y1=mean(Obs_X[,2])+2*pc_fit_normal_us$sdev[1]*pc_fit_normal_us$rotation[2,1],lty=1, lwd=2, length=0.1)
arrows(x0=mean(Obs_X[,1]),y0=mean(Obs_X[,2]),
       x1=mean(Obs_X[,1])-2*pc_fit_normal_us$sdev[1]*pc_fit_normal_us$rotation[1,1],
       y1=mean(Obs_X[,2])-2*pc_fit_normal_us$sdev[1]*pc_fit_normal_us$rotation[2,1],lty=1, lwd=2, length=0.1)
arrows(x0=mean(Obs_X[,1]),y0=mean(Obs_X[,2]),
       x1=mean(Obs_X[,1])+2*pc_fit_normal_us$sdev[2]*pc_fit_normal_us$rotation[1,2],
       y1=mean(Obs_X[,2])+2*pc_fit_normal_us$sdev[2]*pc_fit_normal_us$rotation[2,2],lty=1, lwd=2, length=0.1)
arrows(x0=mean(Obs_X[,1]),y0=mean(Obs_X[,2]),
       x1=mean(Obs_X[,1])-2*pc_fit_normal_us$sdev[2]*pc_fit_normal_us$rotation[1,2],
       y1=mean(Obs_X[,2])-2*pc_fit_normal_us$sdev[2]*pc_fit_normal_us$rotation[2,2],lty=1, lwd=2, length=0.1)
ellipse(mu = c(mean(Obs_X[,1]),mean(Obs_X[,2])), sigma=Obs_S, alpha = .05, npoints = 250, newplot = FALSE,
        draw = TRUE, col="green",type="l")



## Plot with scaled data


plot(Obs_X_Scaled[,1],Obs_X_Scaled[,2],  xlab=expression(x[1]), ylab=expression(x[2]), 
     #xlim=c(-4,4),ylim=c(-4,4),
     xlim=c(-5,5),ylim=c(-5,5),
     type='p',col="black",bty="n", font.main = 1, main="PCs for Bivariate Normal Data - Scaled Data") 
abline(h=mean( Obs_X_Scaled[,2]), v=mean(Obs_X_Scaled[,1]),lty=2, lwd=2, col="red")
arrows(x0=mean(Obs_X_Scaled[,1]),y0=mean(Obs_X_Scaled[,2]),
       x1=mean(Obs_X_Scaled[,1])+2*pc_fit_normal$sdev[1]*pc_fit_normal$rotation[1,1],
       y1=mean(Obs_X_Scaled[,2])+2*pc_fit_normal$sdev[1]*pc_fit_normal$rotation[2,1],lty=1, lwd=2, length=0.1)
arrows(x0=mean(Obs_X_Scaled[,1]),y0=mean(Obs_X_Scaled[,2]),
       x1=mean(Obs_X_Scaled[,1])-2*pc_fit_normal$sdev[1]*pc_fit_normal$rotation[1,1],
       y1=mean(Obs_X_Scaled[,2])-2*pc_fit_normal$sdev[1]*pc_fit_normal$rotation[2,1],lty=1, lwd=2, length=0.1)
arrows(x0=mean(Obs_X_Scaled[,1]),y0=mean(Obs_X_Scaled[,2]),
       x1=mean(Obs_X_Scaled[,1])+2*pc_fit_normal$sdev[2]*pc_fit_normal$rotation[1,2],
       y1=mean(Obs_X_Scaled[,2])+2*pc_fit_normal$sdev[2]*pc_fit_normal$rotation[2,2],lty=1, lwd=2, length=0.1)
arrows(x0=mean(Obs_X_Scaled[,1]),y0=mean(Obs_X_Scaled[,2]),
       x1=mean(Obs_X_Scaled[,1])-2*pc_fit_normal$sdev[2]*pc_fit_normal$rotation[1,2],
       y1=mean(Obs_X_Scaled[,2])-2*pc_fit_normal$sdev[2]*pc_fit_normal$rotation[2,2],lty=1, lwd=2, length=0.1)
ellipse(mu = c(mean(Obs_X_Scaled[,1]),mean(Obs_X_Scaled[,2])), sigma=r, alpha = .05, npoints = 250, newplot = FALSE,
        draw = TRUE, col="green",type="l")
