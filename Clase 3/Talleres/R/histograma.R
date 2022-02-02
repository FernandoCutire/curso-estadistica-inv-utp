# Importando librerias
library(ggplot2)

# Cargando datos con ggplot, usando base de datos incorporada en R
ggplot(data = mtcars, aes(x = mpg)) +
geom_histogram(aes(y = ..density.., fill = ..count..)) +
scale_fill_gradient(low = "#DCDCDC", high = "#174153") +
stat_function(fun = dnorm, colour = "firebrick",
args = list(mean = mean(mtcars$mpg),
sd = sd(mtcars$mpg))) +
ggtitle("Histograma con curva normal teórica") +
theme_bw()
