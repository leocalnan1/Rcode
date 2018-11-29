install.packages('rgl')

library(rgl)

iris <- data('iris')

open3d()

plot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Width)

plot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Width, type = 's')

plot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Width, type = 's',
       col = as.integer(iris$Species))

