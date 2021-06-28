library(ggplot2)
library(olsrr)
library(car)
library(ellipse)

data <- read.table("HW1-Prob.csv", header = TRUE, sep = ',')

ggplot(data, aes(x=z_2, y=resp)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')+
  theme_light()

fit <- lm(resp ~ z_1 + z_2, data=data)
summary(fit) # show results

y = data[1]
z_1 = data[2]
z_2 = data[3]
Y = as.matrix(y)
Z = as.matrix(cbind(1,z_1, z_2)) #design matrix
beta_hat <- solve(t(Z)%*%Z)%*%(t(Z)%*%Y)

#8c
confint(fit, level=0.95)

plot(ellipse(fit, which = c('z_1', 'z_2'), level = 0.95), type = 'l')
points(fit$coefficients['z_1'], fit$coefficients['z_2'])

#8d
full.mod <- lm(formula = resp ~ z_1 + z_2, data = data)
reduced.mod <- lm(formula = resp ~ z_1, data = data)
anova(reduced.mod, full.mod, test = "LRT")

#8e
est_resid_var = (summary(fit)$sigma)**2
#this matrix computation also yields the same result
#(t(Y-Z%*%beta_hat)%*%(Y-Z%*%beta_hat))/12

z_0 = matrix(c(1,7,8))
z0prime = t(z_0)
y_0_hat = z0prime%*%beta_hat
t = qt(1-0.025,12) # 95% CI with df = n-r-1 =15-2-1= 12
ZprimeZ_inv = solve(t(Z)%*%Z)
sqrt_component = sqrt(est_resid_var*z0prime%*%ZprimeZ_inv%*%z_0)
right_CI = y_0_hat + (t* sqrt_component)
left_CI = y_0_hat - (t* sqrt_component)

#8f
sqrt_component_unobs = sqrt(est_resid_var*(1+z0prime%*%ZprimeZ_inv%*%z_0))
right_CI_unobs = y_0_hat + (t* sqrt_component_unobs)
left_CI_unobs = y_0_hat - (t* sqrt_component_unobs)

#8g
#outliers
H = round (Z%*%solve(t(Z)%*%Z)%*%t(Z),2)
qqPlot(fit)
outlierTest(fit)

dat_no_outlier <- data[-c(3,6), ]
no_outlier <- lm(resp ~ z_1 + z_2, data=dat_no_outlier)
summary(no_outlier)

#Leverage

hatvalues(fit)

hv <- as.data.frame(hatvalues(fit))
mn <-mean(hatvalues(fit))
hv$warn <- ifelse(hv[, 'hatvalues(fit)']>3*mn, 'x3',
                  ifelse(hv[, 'hatvalues(fit)']>2*mn, 'x3', '-' ))

hv
plot(hatvalues(fit), type = "h")

dat_no_lev<- data[-c(1,2,3,4,6,14,15), ]
no_lev <- lm(resp ~ z_1 + z_2, data=dat_no_lev)
summary(no_lev)

leveragePlots(fit)

#Influential with cooks D
ols_plot_cooksd_chart(fit)

dat_no_inf<- data[-c(3,6,14), ]
no_inf <- lm(resp ~ z_1 + z_2, data=dat_no_inf)
summary(no_inf)
