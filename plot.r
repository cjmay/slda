library(ggplot2)
for (dataset.name in c('diff3', 'rel3', 'sim3')) {
    filename <- paste(dataset.name, '.tab', sep='')
    data.raw <- read.table(filename, header=T)
    data <- data.frame(mean=rowSums(data.raw)/dim(data.raw)[2])
    data$sd <- rep(0, dim(data)[1])
    for (i in 1:dim(data)[1]) { data$sd[[i]] <- sd(data.raw[i,]) }
    data$idx <- 1:dim(data)[1]
    data$lcl <- data$mean - data$sd
    data$ucl <- data$mean + data$sd
    qplot(idx, mean, data=data) + geom_smooth(aes(ymin=lcl, ymax=ucl), data=data, stat="identity") + ylim(0,1)
}
