library(ggplot2)

plot.experiments <- function(experiment.group.name, experiment.names) {
    for (dataset.name in c('diff3', 'rel3', 'sim3')) {
        for (eval.name in c('oos', 'is')) {
            data <- data.frame()
            for (experiment.name in experiment.names) {
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

            filename.out <- paste(experiment.group.name, '_', dataset.name, '_', eval.name, '.png', sep='')
            qplot(idx, mean, data=data, group=experiment) + geom_smooth(aes(fill=experiment, ymin=lcl, ymax=ucl, color=experiment), data=data, stat="identity") + ylab('nmi (mean +/- one stdev)') + xlab('iteration (starting at end of initialization)') + ggtitle(paste(experiment.group.name, dataset.name, eval.name)) #+ ylim(0,1)
            ggsave(filename.out)
        }
    }
}

plot.experiments('1', c('1-rs0', '1-rs1k', '1-rs10k', '1-rs100k'))
plot.experiments('2', c('2-rs1k-ibs0', '2-rs1k-ibs10', '2-rs1k-ibs100', '2-rs1k-ibs1k'))
plot.experiments('9', c('9-rs0', '9-rs1k', '9-rs10k', '9-rs100k'))
plot.experiments('3-ibs10', c('3-rs0-ibs10', '3-rs1k-ibs10', '3-rs10k-ibs10', '3-rs100k-ibs10'))
plot.experiments('3-ibs100', c('3-rs0-ibs100', '3-rs1k-ibs100', '3-rs10k-ibs100', '3-rs100k-ibs100'))
plot.experiments('3-ibs1k', c('3-rs0-ibs1k', '3-rs1k-ibs1k', '3-rs10k-ibs1k', '3-rs100k-ibs1k'))
