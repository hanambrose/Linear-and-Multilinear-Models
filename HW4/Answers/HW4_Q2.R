#readLines("T11_9.dat")

library("cluster")
library("factoextra")
library("magrittr")
library("dplyr")

cer_data <- read.table("T11_9.dat",skip=21)
colnames(cer_data)<-c("Brand", "Manufacturer", "Calories", "Protein", "Fat", "Sodium", "Fiber", "Carbohydrates","Sugar", "Potassium")

cer_data <- data.frame(cer_data[,-1], row.names = cer_data[,1])

cer_data <- subset(cer_data, select = -c(Manufacturer) )

# cer_data[] <- lapply(cer_data, function(x) if(is.numeric(x)){
#   scale(x, center=TRUE, scale=TRUE)
# } else x)

# Compute hierarchical clustering
res.hc <- cer_data %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "complete")     # Compute hierachical clustering #cpmplete

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

#k-means

my_data <- cer_data %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()                # Scale variables

# View the firt 3 rows
# head(my_data, n = 3)
# 
# res.dist <- get_dist(cer_data, stand = TRUE, method = "pearson")
# 
# fviz_dist(res.dist, 
#           gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
# 
# fviz_nbclust(my_data, kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(my_data, 4, nstart = 25)
# Visualize
fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#model based clustering

library("mclust")


cereal_mb <- Mclust(cer_data, G = 4, parameters=T)
plot(cereal_mb,what = "classification")

BIC <- mclustBIC(cer_data, parameters=T)
plot(BIC)

cereal_BIC <- mclustBIC(cer_data, prior = priorControl(), modelNames = "VVV")
plot(cereal_BIC)

#############################################################################

data(diabetes)
class <- diabetes$class
table(class)
## class
## Chemical   Normal    Overt 
##       36       76       33
X <- diabetes[,-1]
head(X)
##   glucose insulin sspg
## 1      80     356  124
## 2      97     289  117
## 3     105     319  143
## 4      90     356  199
## 5      90     323  240
## 6      86     381  157
clPairs(X, class)
BIC <- mclustBIC(X)
plot(BIC)
summary(BIC)

mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)

plot(mod1, what = "classification")
