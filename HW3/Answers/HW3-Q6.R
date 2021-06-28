library("MASS")
library(boot)
library(dplyr)
library(magrittr)

data("urine", package="boot")

urine <- na.omit(urine)

urine.lda <- lda(r ~ ., data=urine)
urine.lda


urine.lda.values <- predict(urine.lda)
urine.lda.values$class

sum(urine$r==urine.lda.values$class)/length(urine$r)

ldahist(data = urine.lda.values$x[,1], g= Type)

