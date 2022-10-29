install.packages("ggplot2",dependencies = TRUE)
library(ggplot2)

##BAR GRAFİĞİ##
ggplot(iris, aes(x = Species)) + 
  geom_bar(fill = "darkblue",
           color="yellow") + 
  labs(x = "Türler",
       y = "Frekans",
       title = "Bar Grafiği")

View(mtcars)
arabaVeri = subset(mtcars, (select=c("mpg", "cyl", "wt", "am")))
colnames(arabaVeri) = c("yol", "silindir", "ağırlık", "vites")
str(arabaVeri)

ggplot(arabaVeri, aes(x = vites)) + 
  geom_bar(fill = "blue",
           color = "black",
           size = 2,
           linetype = "dashed") +
  labs(x = "Vites türü",
       y = "Frekans",
       title = "Bar Grafiği")

ggplot(arabaVeri, aes(x = vites)) + 
  geom_bar(fill = "blue",
           color = "black",
           size = 2,
           linetype = "dotdash") +
  labs(x = "Vites türü",
       y = "Frekans",
       title = "Bar Grafiği")
##THYROID##
install.packages("mclust")
library(mclust)
View(thyroid)
ggplot(thyroid, aes(x = Diagnosis)) + 
  geom_bar(fill = "purple",
           color = "black",
           size = 2,
           linetype = "dotdash") +
  labs(x = "Diagnosis",
       y = "Frekans",
       title = "Bar Grafiği")

##Yüzdeliklerle Bar Grafiği##
ggplot(mtcars, aes(x = gear,
                   y = ..count.. / sum(..count..),
                   fill = factor(carb))) +
  geom_bar() + 
  labs(x = "Vites",
       y = "%",
       title = "bar grafiği") + 
  scale_y_continuous(labels=scales::percent)

##FARAWAY##
install.packages("faraway")
library(faraway)
View(dvisits)
str(dvisits)


ggplot(dvisits, aes(x = sex,
                   y = ..count.. / sum(..count..),
                   fill = factor(medicine))) +
  geom_bar() + 
  labs(x = "Cinsiyet",
       y = "%",
       title = "bar grafiği") + 
  scale_y_continuous(labels=scales::percent)

##BOX PLOT##
# sayısal değişkenler için kullanılır
#outlier detection
install.packages("robustbase")
library(robustbase)

ggplot(data = alcohol,
       mapping = aes(x=logSolubility)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip()

##iris##
ggplot(data = iris,
       mapping = aes(x=Sepal.Length)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip() +
  facet_grid(. ~Species)

## mtcars##
View(arabaVeri)
ggplot(data = arabaVeri,
       mapping = aes(x=ağırlık)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip() +
  facet_grid(. ~vites)

View(arabaVeri)
ggplot(arabaVeri,
       aes(x=ağırlık)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip() +
  facet_grid(. ~vites)

ggplot(data = iris,
       mapping = aes(x=Sepal.Length)) +
  stat_boxplot(aes(Sepal.Length),
               geom="errorbar", linetype=1, width=0.5) +
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="purple") +
  coord_flip() +
  facet_grid(. ~Species)

ggplot(data = dvisits,
       mapping = aes(x=age)) +
  stat_boxplot(aes(age),
               geom="errorbar", linetype=1, width=0.5) +
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="purple") +
  coord_flip() +
  facet_grid(. ~illness)

##PLOTLY##
install.packages("plotly")
library(plotly)

p = ggplot(data = iris,
       mapping = aes(x=Sepal.Length)) +
    geom_boxplot(outlier.color="red",
               notch=FALSE)
               
  
ggplotly(p)
##cowplot##
install.packages("cowplot")
library(cowplot)

p1 = ggplot(iris, aes(x = Species)) + 
  geom_bar(fill = "darkblue",
           color="yellow") + 
  labs(x = "Türler",
       y = "Frekans",
       title = "Bar Grafiği")
p2 = 

  ggplot(data = dvisits,
         mapping = aes(x=age)) +
  stat_boxplot(aes(age),
               geom="errorbar", linetype=1, width=0.5) +
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="purple") +
  coord_flip() +
  facet_grid(. ~illness)
p3 = ggplot(dvisits, aes(x = sex,
                         y = ..count.. / sum(..count..),
                         fill = factor(medicine))) +
  geom_bar() + 
  labs(x = "Cinsiyet",
       y = "%",
       title = "bar grafiği") + 
  scale_y_continuous(labels=scales::percent)
p4 = ggplot(arabaVeri, aes(x = vites)) + 
  geom_bar(fill = "blue",
           color = "black",
           size = 2,
           linetype = "dashed") +
  labs(x = "Vites türü",
       y = "Frekans",
       title = "Bar Grafiği")
plot_grid(p1, p2, p3, p4, labels = "AUTO", ncols=2)

##plot_grid##

p1 = ggplot(data = thyroid,
       mapping = aes(x=RT3U)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip()
p2 = ggplot(data = thyroid,
            mapping = aes(x=T4)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip()
p3 = ggplot(data = thyroid,
            mapping = aes(x=T3)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip()
p4 = ggplot(data = thyroid,
            mapping = aes(x=TSH)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip()
p5 = ggplot(data = thyroid,
            mapping = aes(x=DTSH)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip()
plot_grid(p1, p2, p3, p4,p5, labels = "AUTO", ncol=5)


