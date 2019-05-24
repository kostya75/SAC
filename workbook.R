
library(plyr)

norm_spec<-data.frame(n=c(100,100,100),mean=c(5,5,10),sd=c(1,2,1))
tt<-mdply(norm_spec,rnorm)



mdply(data.frame(mean = 1:5, sd = 1:5), rnorm, n = 2)


mdply(expand.grid(mean = 1:5, sd = 1:5), rnorm, n = 2)

mdply(cbind(mean = 1:5, sd = 1:5), rnorm, n = 5)

mdply(cbind(mean = 1:5, sd = 1:5), as.data.frame(rnorm), n = 5)

tt<-mdply(norm_spec,as.data.frame(rnorm))

x <- array(1:24, 2:4)
shape <- function(x) if (is.vector(x)) length(x) else dim(x)
shape(x)
aaply(x,2,print)

hp_per_cyl <- function(hp, cyl, ...) hp / cyl
splat(hp_per_cyl)(mtcars[1,])
each(min,max)(mtcars)
laply(mtcars,each(min,max))
