library(ggplot2)

plot.experiments <- function(experiment.group.name, experiment.names) {
    cat(experiment.group.name, '\n')
    for (dataset.name in c('diff3', 'rel3', 'sim3')) {
        cat('*', dataset.name, '\n')
        for (eval.name in c('oos', 'is')) {
            data <- data.frame()
            for (experiment.name in experiment.names) {
                cat('  -', experiment.name, eval.name, '\n')
                filename.in <- paste(experiment.name, '/', dataset.name, '_', eval.name, '.tab', sep='')
                my.data.raw <- read.table(filename.in, header=T)
                my.data <- data.frame(mean=rowSums(my.data.raw)/dim(my.data.raw)[2])
                my.data$experiment <- rep(experiment.name, dim(my.data)[1])
                my.data$sd <- apply(my.data.raw, 1, sd)
                my.data$idx <- 1:dim(my.data)[1]
                my.data$lcl <- my.data$mean - my.data$sd
                my.data$ucl <- my.data$mean + my.data$sd
                data <- rbind(data, my.data)
            }

            dir.create('plots')
            dir.create(paste('plots', experiment.group.name, sep='/'))
            filename.out <- paste('plots', experiment.group.name, paste(dataset.name, '_', eval.name, '.png', sep=''), sep='/')
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
plot.experiments('10_11_12', c('10', '11', '12-rs1k', '12-rs10k'))
plot.experiments('12', c('12-rs100', '12-rs1k', '12-rs10k', '12-rs100k', '12-rs200k'))
plot.experiments('13_14_15', c('13', '14', '15-rs1k', '15-rs10k'))
plot.experiments('15', c('15-rs100', '15-rs1k', '15-rs10k', '15-rs100k', '15-rs200k'))
