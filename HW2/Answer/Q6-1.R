library(ggplot2)
library(tidyr)
library(dplyr)

dat <- read.csv("Galtons_Height_Data.csv")
dat <- filter(dat, Kids >=2)
dat <- dat[,1:5]
#dat <- dat[match(unique(dat$Family), dat$Family),]

dat_first <- dat %>% group_by(Family,Father, Mother, Gender) %>% summarise(Height = first(Height))

dat_spread <-dat_first %>%
              gather(variable, value, Height) %>%
              unite("variable", Gender, variable, sep = ".") %>%
              spread(variable, value)

dat_spread <- dat_spread[,2:5]
myData <- dat_spread %>% drop_na()
var(myData)
cor(myData)


## prcomp uses the SVD decomposition (versus princomp)
height_pc <- prcomp(myData[,1:4], 
                  center = TRUE,scale. = TRUE) #scale is true for standardized
#height_pc <- prcomp(myData[,1:4])
plot(height_pc)
biplot(height_pc)
names(height_pc)     
## sdev: sqrt(eigenvalues)
## Rotation: Columns give the Eigenvectors.
## Center & Scale: values of x_bar and sd(x)
## x: Principal Components (Loading values)

## Can Extract PCs through either of these: 
height_PC_Loadings <- height_pc$x


## Plot of Eigenvalues (or Variances of PCs)
plot(height_pc, type='l', main="Scree Plot: Height")

## Computing the percentage of variation explained
Percent_Variation <- height_pc$sdev**2/sum(height_pc$sdev**2)

## Manual PCA (Using Spectral Decomposition): 

height_c <- scale(myData, center = TRUE, scale = TRUE)

# Correlation matrix 
R_height_c <- cor(height_c)

# Eigen Decomposition
eigen(R_height_c) 

# Extract PCs
PC_mat <- height_c %*% eigen(R_height_c)$vectors

#PC_mat <- myData %*% eigen(var(myData))$vectors

height_PC1 <- sort(PC_mat[,1])

height_PC2 <- sort(PC_mat[,2])
pop15_sort <- sort(height_c[,2])

height_pc.df <- data.frame(height_pc$x[,1:2])


plot(height_pc.df$PC2 ~ height_pc.df$PC1, type="p",cex=1, pch=20,
     col="red", xlim=c(-4,4), ylim=c(-4,4),
     xlab="PC1", ylab="PC2", main="Height", 
     sub="PC1 vs. PC2")
text(height_pc.df$PC2 ~ height_pc.df$PC1,labels=rownames(height_pc.df),data=height_pc.df, cex=0.9, font=1)


