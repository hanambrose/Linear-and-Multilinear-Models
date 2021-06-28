library(tidyr)
library(dplyr)

pheno <- read.csv("pheno.csv")
pheno$X <- NULL
pheno <- pheno %>% drop_na()

head(pheno)
pheno_cor <- cor(pheno)

############ PCA/SPCA ################
pheno_pc <- princomp(pheno, cor = TRUE, scores = TRUE)
print(summary(pheno_pc), loadings = TRUE)

## Plot
plot(pheno_pc, type='l', main="Scree Plot: Height")
biplot(pheno_pc)

#SPCA
require(elasticnet)

pheno_spca <- spca(pheno_cor,K = 7, type = "Gram", sparse = "penalty", para=c(0.06,0.16,0.1,0.5,0.5,0.5,.4))

#pheno_spca <- spca(pheno_cor,K = 7, type = "Gram", sparse = "varnum", para = c(7,4,4,2,1,1,1))

pheno_spca$loadings

#rotation
fa2 <- factanal(pheno, factors = 2, rotation="none")
fa2_vari <- factanal(pheno, factors = 2, rotation="varimax")  

par(mfrow = c(1,2))
plot(fa2$loadings[,1], 
     fa2$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")

plot(fa2_vari$loadings[,1], 
     fa2_vari$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax")

############## clusters #################
library("cluster")
library("factoextra")
library("magrittr")

# Compute hierarchical clustering
res.hc <- pheno %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "centroid")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

#k-means

scale_pheno <- pheno %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()                # Scale variables

set.seed(123)
km.res <- kmeans(scale_pheno, 3, nstart = 25)
# Visualize
fviz_cluster(km.res, data = scale_pheno,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


library("mclust")


pheno_mb <- Mclust(pheno, G = 3, parameters=T)
plot(pheno_mb,what = "classification")
