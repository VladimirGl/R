
# скаттерплоты

# по всем группам
plot(iris$Petal.Length, iris$Petal.Width, main="Iris all groups data")

# по каждой в отдельности
for (i in unique(iris$Species)) {
  plot(subset(iris, Species == i)[3:4],  main = i)
}

# оценка плотности
# по всем группам
hist(iris$Petal.Width, prob = 1, breaks = 20, main = "Iris all groups data")
lines(density(iris$Petal.Width, kernel = "gaussian"), col = 2)

# по каждой в отдельности
for (i in unique(iris$Species)) {
  hist(subset(iris, Species == i)$Petal.Width, prob = 1, breaks = 20, main = i)
  lines(density(subset(iris, Species == i)$Petal.Width, kernel="gaussian"), col = 2)
}

# эмпирическая ф-я распределения
# по всем группам
plot(ecdf(iris$Petal.Width), main = "Empirical distribution function of iris data")

# по каждой в отдельности
for (i in unique(iris$Species)) {
  plot(ecdf(subset(iris, Species == i)$Petal.Width), main = i)
}

