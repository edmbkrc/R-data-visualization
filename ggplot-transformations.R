#data frame oluşturdum
df <- data.frame(y=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8),
                 x1=c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8),
                 x2=c(3, 3, 6, 6, 8, 9, 9, 8, 8, 7, 4, 3, 3, 2, 7))


# library
library(ggplot2)


#log dönüşümü yapıldı
log_y <- log10(df$y)
df1 = data.frame(y = log_y)
#histogram grafiği
hist(df$y, col='steelblue', main='Original')

#log dönüşümü için histogram
hist(log_y, col='coral2', main='Log Transformed')


p1 <- ggplot(df, aes(x=y)) + 
  geom_histogram( binwidth=3, 
                  fill="#69b3a2", 
                  color="#e9ecef", 
                  alpha=0.9) +
  labs(x = "y değerleri",
       y = "frekans",
       title = "ORIGINAL") +
  theme_dark()
p1

p2 <- ggplot(df1, aes(x=y)) + 
  geom_histogram( binwidth=0.8, 
                  fill="red", 
                  color="blue", 
                  alpha=0.2) +
  labs(x = "y değerleri",
       y = "frekans",
       title = "LOG DÖNÜŞÜMLÜ") +
  theme_bw()
p2

install.packages("cowplot")
library(cowplot)
plot_grid(p1, p2, labels = "AUTO", ncol=2)

##Shapiro-Wilk Test##
shapiro.test(df$y)
W = 0.77225, p-value = 0.001655

##Shapiro-Wilk Test on log-transformed data 
shapiro.test(log_y)
W = 0.89089, p-value = 0.06917

#create data frame
dff <- data.frame(y=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 6, 7, 8),
                 x1=c(7, 7, 8, 3, 2, 4, 4, 6, 6, 7, 5, 3, 3, 5, 8),
                 x2=c(3, 3, 6, 6, 8, 9, 9, 8, 8, 7, 4, 3, 3, 2, 7))

#karekök dönüşümü
sqrt_y = sqrt(dff$y)
dff1 = data.frame(y1 = sqrt_y)

p3 <- ggplot(dff, aes(x=y)) + 
  geom_histogram( binwidth=3, 
                  fill="#69b3a2", 
                  color="#e9ecef", 
                  alpha=0.9) +
  labs(x = "y değerleri",
       y = "frekans",
       title = "ORIGINAL") +
  theme_dark()
p3

p4 <- ggplot(dff1, aes(x=y1)) + 
  geom_histogram( binwidth=0.8, 
                  fill="red", 
                  color="blue", 
                  alpha=0.2) +
  labs(x = "y değerleri",
       y = "frekans",
       title = "KAREKÖK DÖNÜŞÜMLÜ") +
  theme_bw()
p4

plot_grid(p3, p4, labels = "AUTO", ncol=2)

##Shapiro-Wilk Test##
shapiro.test(dff$y)
#W = 0.77225, p-value = 0.001655

##Shapiro-Wilk Test on squareroot-transformed data 
shapiro.test(sqrt_y)
#W = 0.84321, p-value = 0.01394

