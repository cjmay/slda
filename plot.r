library(ggplot2)

for (dataset.name in c('diff3', 'rel3', 'sim3')) {
    for (eval.name in c('oos', 'is')) {
        data <- data.frame()
        for (experiment.name in c('1-rs0', '1-rs1k', '1-rs10k', '1-rs100k')) {
            filename.in <- paste(experiment.name, '/', dataset.name, '_', eval.name, '.tab', sep='')
            my.data.raw <- read.table(filename.in, header=T)
            my.data <- data.frame(mean=rowSums(my.data.raw)/dim(my.data.raw)[2])
            my.data$experiment <- rep(experiment.name, dim(my.data)[1])
            my.data$sd <- rep(0, dim(my.data)[1])
            for (i in 1:dim(my.data)[1]) { my.data$sd[[i]] <- sd(my.data.raw[i,]) }
            my.data$idx <- 1:dim(my.data)[1]
            my.data$lcl <- my.data$mean - my.data$sd
            my.data$ucl <- my.data$mean + my.data$sd
            data <- rbind(data, my.data)
        }

        filename.out <- paste(dataset.name, '_', eval.name, '.png', sep='')
        qplot(idx, mean, data=data, group=experiment) + geom_smooth(aes(ymin=lcl, ymax=ucl), data=data, stat="identity")# + ylim(0,1)
        ggsave(filename.out)
    }
}
