library(ggplot2)

dat <- read.table("dotplot.txt", header=TRUE, row.names=NULL, quote = "", sep="\t", check.names=FALSE)

p <- ggplot(dat, aes(x = specie, y = responserate, colour = energetic_level, size = functional_level))
p <- p + geom_point(alpha = .9) + scale_colour_continuous(low = '#32CD32', high = '#ff4040')
p <- p + labs(x = 'Species', y = 'Response rate')
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = .5))
p1 <- p + facet_grid(. ~ panel)
p1
