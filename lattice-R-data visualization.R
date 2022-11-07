##LATTICE Paketi Örnekler##
library(lattice)
install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
View(PimaIndiansDiabetes)


str(PimaIndiansDiabetes)

# Tek değişkenli Nokta Grafiği
dotplot(~pregnant, data = PimaIndiansDiabetes, col = "red")
dotplot(~insulin, PimaIndiansDiabetes, col = "blue")
dotplot(~glucose, PimaIndiansDiabetes, col = "purple")

# Çift Değişkenli Nokta Grafiği
xyplot(diabetes ~ pressure, PimaIndiansDiabetes, col = "red")
xyplot(diabetes ~ glucose, PimaIndiansDiabetes, col = "yellow")
dotplot(diabetes ~ glucose, PimaIndiansDiabetes, col = "blue")
dotplot(diabetes ~ insulin, PimaIndiansDiabetes, col = "purple")

#Üç değişkenli
xyplot(glucose ~ mass | diabetes, PimaIndiansDiabetes, col = "red")

dotplot(insulin ~ age | diabetes, PimaIndiansDiabetes, col = "blue")

# Bar grafikleri
barchart(insulin ~ diabetes, PimaIndiansDiabetes,
         main = "Bar Chart in Diabetes",
         xlab = "Diabetes Value",
         ylab = "Insulin Value",
         col = c("chocolate", "green", "grey", "blue"))

barchart(age ~ mass | diabetes, PimaIndiansDiabetes,
         main = "Bar Chart in Diabetes",
         xlab = "Diabetes Value",
         ylab = "Glucose Value",
         col = c("chocolate", "red", "grey", "blue"),
         horiz = FALSE)
# Pasta Grafikleri
install.packages("ggplot2")
library(ggplot2)
View(PimaIndiansDiabetes)
pregnant = table(PimaIndiansDiabetes$pregnant)
diabetes = table(PimaIndiansDiabetes$diabetes)
pie(pregnant, labels = diabetes, main="Pie Chart of Diabetes")

# 3D Pasta Grafiği
install.packages("plotrix", dependencies = TRUE)
library(plotrix)

pie3D(pregnant,labels=diabetes,explode=0.1,
      main="Pie Chart of Diabetes ")
# Gruplara Göre Ayıarak Grafik Çizme

dotplot(~mass , PimaIndiansDiabetes, col = "blue")
df = data.frame(PimaIndiansDiabetes, GR = factor(1))

dotplot(mass ~ GR, data = df)
  
# Renklendirme İşlemleri

red = rgb(249/255, 254/255, 47/255)
amber = rgb(255/255, 126/255, 0/255)
green = rgb(39/255, 121/255, 51/255)

bwplot(glucose ~ age , data = PimaIndiansDiabetes, col = "blue",
        panel = function(...){
        panel.bwplot()(..., groups = PimaIndiansDiabetes$diabetes,fill = c(red, amber, green))  
        })


