library(boot)

data("urine", package="boot")
urine <- urine %>% drop_na()

sub_urine <- filter(urine, r == 1) # no calcium oxalate r = 0 
#sub_urine <-urine

#library(tidyverse)
#urine %>% remove_rownames %>% column_to_rownames(var="r")

head(sub_urine[,2:7])
var(sub_urine[,2:7])
cor(sub_urine[,2:7])

## prcomp uses the SVD decomposition (versus princomp)
urine_pc <- prcomp(urine[,2:7], 
                  center = TRUE,scale. = TRUE)

#urine_pc <- princomp(sub_urine[,2:7], cor = TRUE, scores = TRUE)
print(summary(urine_pc), loadings = TRUE)

urine_PC_Loadings <- urine_pc$x

## Plot of Eigenvalues (or Variances of PCs)
plot(urine_pc, type='l', main="Scree Plot: Height")
biplot(urine_pc)

## Computing the percentage of variation explained
Percent_Variation <- urine_pc$sdev**2/sum(urine_pc$sdev**2)

## Manual PCA (Using Spectral Decomposition): 

urine_c <- scale(sub_urine[,2:7], center = TRUE, scale = TRUE)

# Correlation matrix 
R_urine_c <- cor(urine_c)

# Eigen Decomposition
eigen(R_urine_c) 

# Extract PCs
PC_mat <- urine_c %*% eigen(R_urine_c)$vectors

Life_PC1 <- sort(PC_mat[,1])


urine_pc.df <- data.frame(urine_pc$x[,1:2])
plot(urine_pc.df$PC2 ~ urine_pc.df$PC1, type="p",cex=1, pch=20,
     col="red", xlim=c(-4,4), ylim=c(-4,4),
     xlab="PC1", ylab="PC2", main="Height", 
     sub="PC1 vs. PC2")
text(urine_pc.df$PC2 ~ urine_pc.df$PC1,labels=rownames(urine_pc.df),data=urine_pc.df, cex=0.9, font=1)
