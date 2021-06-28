z <- 11:30
#x2 <- runif(20,5,95)
#x3 <- rbinom(20,1,.5)

b0 <- 17
b1 <- 0.5
#b2 <- 0.037
#b3 <- -5.2
sigma <- 1.4

eps <- rnorm(z,0,sigma)
y <- b0 + b1*z + eps

#Model 1
plot(y~z)
m1 <- lm(formula = y ~ z)
abline(m1)
summary(m1)

#using linear algebra
Y <- matrix(y) 
Z <- cbind(1, z) #design matrix
beta_hat <- solve(t(Z)%*%Z)%*%(t(Z)%*%Y)
y.hat=Z%*%beta_hat

#Model 2
ybar = mean(y)
newy = y-ybar
plot((y-ybar)~z)
m2 <- lm(formula = (y-ybar) ~ z)
abline(m2)
summary(m2)
hist(residuals(m2),breaks=10,col="light grey")
plot(m2)

# Model 3
plot((y-ybar) ~ z -1)
m3 <- lm(formula = (y-ybar) ~ z -1)
abline(m3)
summary(m3)
hist(residuals(m3),breaks=10,col="light grey")
plot(m3)

#Model 4
c <- 5
plot(c*y~z)
m4 <- lm(formula = c*y ~ z)
abline(m4)
summary(m4)

######## Question 7########

#Find parameter values for the distribution of beta hat and epsilon hat
mean_beta_hat <- rbind(b0,b1)
var_beta_hat <- (sigma^2)*solve(t(Z)%*%Z)

I = diag(20)
H = Z%*%(solve(t(Z)%*%Z))%*%t(Z)
var_eps_hat <- (sigma^2)*(I-H)


#Estimate these parameters through least squares and maximum likelihood.
Y = as.matrix(y)
Z = as.matrix(cbind(1,z)) #design matrix
beta_hat <- solve(t(Z)%*%Z)%*%(t(Z)%*%Y) # LS estimate is the same as ML

n = 20
r = 1
denominator_LS = n-r-1
denominator_ML = n

SSE = (t(Y-Z%*%beta_hat)%*%(Y-Z%*%beta_hat))
sigma_hat_sqrt_LS = SSE/denominator_LS
#sigma_hat_sqrt_LS = (summary(m1)$sigma)**2
sigma_hat_sqrt_ML = SSE/denominator_ML

#ploting 3d

library(MASS)
#bivn <- mvrnorm(1000, mu = c(0, 0), Sigma = matrix(c(1, .5, .5, 1), 2))

bivn <- mvrnorm(1000, mu= t(beta_hat),Sigma = var_beta_hat)

# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)

# now plot your results
contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)
