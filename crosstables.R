##ÇAPRAZ TABLO

# Kategorik değerler için kullanılır

install.packages("vcd")
library(vcd)

install.packages("vcdExtra")
library(vcdExtra)

set.seed(1)
cins = sample(c("E","K"), 500, replace = T,  prob = c(0.2,0.8))
spor = sample(c("+","-"), 500, replace = T, prob = c(0.1,0.9))

cap_tab = table(cins,spor)
cap_tab

mosaic(cap_tab, shade = T, legend = T )

veri_f = JobSatisfaction
veri = expand.dft(veri_f)#vektör haline grtirir

yön_mem = veri$management
çalışan_tat = veri$own
cap_tab = table(yön_mem, çalışan_tat)
mosaic(cap_tab, shade = T, legend = T )

install.packages("gplots")
library(gplots)

balloonplot(cap_tab)

veri_f = JobSatisfaction
library(ggplot2)
ggplot(veri_f, aes(x = management,
                   y = Freq,
                   fill = own))+
  geom_col(position = "dodge")

library(corrplot)
veri_f = JobSatisfaction
veri = expand.dft(veri_f)#vektör haline grtirir
yön_mem = veri$management
çalışan_tat = veri$own
cap_tab = table(yön_mem, çalışan_tat)
test_kikare = chisq.test(cap_tab)

corrplot(test_kikare$residuals, is.cor = F)
###########
##
data("Abortion")
?Abortion

mosaic(Abortion, shade = T)

##

str(bacteria)
df = bacteria

ggplot(df, aes(x=trt, y=week, fill=hilo)) + 
  geom_col(position = "dodge")

##
install.packages("CGPfunctions")
library(CGPfunctions)
install.packages("PlotXTabs")
library(PlotXTabs)

PlotXTabs(df, trt, hilo)

PlotXTabs(df, trt, hilo, "stack")

PlotXTabs(df, trt, hilo, "percent")

PlotXTabs(df, trt, hilo, "side")

PlotXTabs(df, trt, ap, "mispelled")

##
a = xtabs(~ trt + hilo, data=df)

mosaic(a)


