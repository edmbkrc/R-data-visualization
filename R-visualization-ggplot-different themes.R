library(tidyverse) > ggplot2::mpg

install.packages("ggthemes")
library(ggthemes)

View(mpg)
str(mpg)

p1 = ggplot(mpg, aes(cty, hwy, color = factor(cyl))) +
  geom_jitter() +
  geom_abline(colour = "grey50", size = 2) +
  theme_economist() +
  labs(x = "Şehir İçi 1 Galonla Gidilen Mil",
       y  = "Şehir Dışı 1 Galonla Gidilen Mil",
       title = "Şehir İçi ve Dışı Yakıt Tüketimi") +
  scale_color_brewer(type = "seq", palette = "Spectral") 

p1

p2 = p1 + theme_bw() + 
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.background = element_rect(fill = "white",
                                         size = 4,
                                         colour = "white"),
        legend.justification = c(0,1),
        legend.position = c(0,1),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.grid.minor = element_blank()
        )
p2

mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V-shaped", "Straight"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
})

p3 = ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(
    title = "Fuel economy - Weight",
    subtitle = "(1973-74)",
    caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 1",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Gears") +
  theme_dark()
  

p3


library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)
 
cyls = as.factor(mtcars$cyl)

p4 = ggplot(mtcars, aes(x = mpg, fill = cyls)) +
     geom_density(alpha = 0.7) + 
     ggtitle("Plot title") +
     theme_ipsum() + # Arial Narrow
     scale_fill_ipsum() +
     theme(legend.position = "top")
p4

str(diamonds)
View(diamonds)


p5 = ggplot(diamonds, aes(x = color)) +
  
  geom_bar(fill = "blue",
           color = "black",
           size = 2,
           linetype = "dashed") +
  labs(x = "Renk",
       y = "Frekans",
       title = "Bar Grafiği") +
  theme_calc()
p5

p6 = ggplot(data = diamonds,
            mapping = aes(x=price)) + 
  geom_boxplot(outlier.color="red",
               notch=FALSE,
               fill="blue") +
  coord_flip() +
  facet_grid(. ~cut) +
  theme_economist()
p6

install.packages("cowplot")
library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6, labels = "AUTO", ncol=3)
