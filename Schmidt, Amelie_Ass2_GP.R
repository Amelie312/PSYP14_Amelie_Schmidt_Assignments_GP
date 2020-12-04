library(MASS)
library(smacof)
Nations <- read.delim("~/Desktop/Uni/Master/1. Semester/Advanced Scientific Methods/Assignments/Nations.txt")
nations_matr <- data.matrix(Nations)

dist<-sim2diss(nations_matr, method = 9,
               to.dist = TRUE)
fit <- isoMDS(dist) 

plot(fit$points[,1], fit$points[,2], xlab="Coordinate 1", ylab="Coordinate 2", 
     xlim=range(fit$points [,1])*1.2, type="n")
text(fit$points[,1], fit$points[,2], labels = colnames(nations_matr), cex=.6)

nations_shep <- Shepard(dist, fit$points)
nations_shep

plot(nations_shep, pch = 20, xlab = "Dissimilarity",
     ylab= "Distance", xlim = range(nations_shep$x),
     ylim=range(nations_shep$x))
lines(nations_shep$x, nations_shep$yf, type = "s")



