library(dplyr)

data("iris")
iris <- filter(.data = iris, Species %in% c("versicolor", "virginica"))

ggplot(data = iris, aes(x = Species, y = Petal.Length, colour = Species)) +
geom_boxplot() +
geom_point() +
theme_bw() +
theme(legend.position = "none")
aggregate(Petal.Length~Species, data = iris, FUN = var)

# Shapiro Wilk

# Importando librerias
library(ggplot2)

# Cargando datos con ggplot, usando base de datos incorporada en R
shapiro.test(x = mtcars$mpg)

# F - Test
var.test(x = iris[iris$Species == "versicolor", "Petal.Length"],
y = iris[iris$Species == "virginica", "Petal.Length"] )

# Iris dataset
data("iris")

ggplot(data = iris, aes(x = Species, y = Petal.Length, colour = Species)) +
geom_boxplot() +
geom_point() +
theme_bw() +
theme(legend.position = "none")

# Test bartlett
aggregate(Petal.Length~Species, data = iris, FUN = var)
bartlett.test(iris$Sepal.Length ~ iris$Species)

# Test Levene
library(car)
iris <- filter(.data = iris, Species %in% c("versicolor", "virginica"))
leveneTest(y = iris$Petal.Length, group = iris$Species, center = "median")

# T-student
library(openintro)
library(tidyverse)

data(births)
head(births, 4)
smoker <- births %>% filter(smoke == "smoker") %>% pull(weight)
nonsmoker <- births %>% filter(smoke == "nonsmoker") %>% pull(weight)

t.test(
x = smoker,
y = nonsmoker,
alternative = "two.sided",
mu = 0,
var.equal = TRUE,
conf.level = 0.95
)



# Anova