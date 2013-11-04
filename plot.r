d0 <- read.table('0.tab', header=T)
x <- data.frame(mean=rowSums(d0)/30)
x$sd <- rep(0, 1503)
for (i in 1:dim(x)[1]) { x$sd[[i]] <- sd(d0[i,]) }
x$idx <- 1:dim(x)[1]
x$lcl <- x$mean - x$sd
x$ucl <- x$mean + x$sd
qplot(idx, mean, data=x) + geom_smooth(aes(ymin=lcl, ymax=ucl), data=x, stat="identity") + ylim(0,1)

d1 <- read.table('1.tab', header=T)
y <- data.frame(mean=rowSums(d1)/30)
y$sd <- rep(0, dim(y)[1])
for (i in 1:dim(y)[1]) { y$sd[[i]] <- sd(d1[i,]) }
y$idx <- 1:dim(y)[1]
y$ucl <- y$mean + y$sd
y$lcl <- y$mean - y$sd
qplot(idx, mean, data=y) + geom_smooth(aes(ymin=lcl, ymax=ucl), data=y, stat="identity") + ylim(0,1)

d2 <- read.table('2.tab', header=T)
z <- data.frame(mean=rowSums(d2)/30)
z$sd <- rep(0, dim(z)[1])
for (i in 1:dim(z)[1]) { z$sd[[i]] <- sd(d2[i,]) }
z$idx <- 1:dim(z)[1]
z$ucl <- z$mean + z$sd
z$lcl <- z$mean - z$sd
qplot(idx, mean, data=z) + geom_smooth(aes(ymin=lcl, ymax=ucl), data=z, stat="identity") + ylim(0,1)
