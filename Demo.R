setwd('D:/Documents/R/Statistics and Econometrics/NonParameterEstimation')

library(devtools)
load_all()

x <- rd5000$x
y <- rd5000$y

kd.res <- kernel.density(x)
kd.func <- seq2func(kd.res$seq$x, kd.res$seq$d)
print(dnorm(-2:2))
print(kd.func(-2:2))

kr.res <- kernel.regression(y, x)
kr.func <- seq2func(kr.res$seq$x, kr.res$seq$y)
print(sapply(-2:2, function(x){return(x**2-x)}))
print(kr.func(-2:2))

library(ggplot2)
p <- ggplot(data=kr.res$seq, aes(x=x, y=y))
p <- p + geom_line()
print(p)
