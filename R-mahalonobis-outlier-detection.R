## Çoklu değişken outlier saptanması ve işlemleri
## Mahalonobis Uzaklığı

set.seed(1)
df = replicate(10, rnorm(100))
df[1,] = 3
mah_uz = mahalanobis(df,colMeans(df),cov(df))
sınır = qchisq(1-0.1, ncol(df))

alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)

##iris dataset
df = iris[,-5]
mah_uz = mahalanobis(df,colMeans(df),cov(df))
sınır = qchisq(1-0.05, ncol(df))

alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
#####median
df = iris[,-5]

c = apply(df, 2, median)
s = cov(df)
mah_uz = mahalanobis(df,c,s)
sınır = qchisq(1-0.001, ncol(df))

alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
## detMCD
##depth
##ball
##robustbase
##
##mcd 
##mve min value estimTOR
## multivariate median
library(MASS)
library(robustbase)
install.packages("DetMCD")
library(DetMCD)
install.packages("depth")
library(depth)

depth
df= robustbase::Animals2
sınır = qchisq(1-0.001, ncol(df))

par(mfrow=c(2,3))
#1
c = apply(df,2,median)
s = cov(df)
mah_uz = mahalanobis(df,c,s)
alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
#2
c = cov.mcd(df)$center
s = cov.mcd(df)$cov
mah_uz = mahalanobis(df,c,s)
alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
#3

c = DetMCD(df)$raw.center
s = DetMCD(df)$raw.cov
mah_uz = mahalanobis(df,c,s)
alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
#4
c = med(df, method="Liu")$median
s = cov(df)
mah_uz = mahalanobis(df,c,s)
alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
#5
c = covOGK(df, sigmamu=scaleTau2)$center
s = covOGK(df, sigmamu=scaleTau2)$cov
mah_uz = mahalanobis(df,c,s)
alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)
#6
c = cov.mcd(df)$center
s = cov.mcd(df)$cov
mah_uz = mahalanobis(df,c,s)
alt = min(sınır,mah_uz)
ust = max(sınır, mah_uz)
plot(mah_uz, ylim=c(alt,ust),
     col=ifelse(mah_uz>sınır, "blue", "black"),
     pch=ifelse(mah_uz>sınır, 17, 1),#plot in character
     cex = ifelse(mah_uz>sınır, 2, 1))
abline(h=sınır, col="red", lwd=4)

