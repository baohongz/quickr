library(ggplot2)

plot = ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut), width = 1, alpha=0.6) + coord_polar() +facet_wrap( ~ clarity)

png("polar.png", width = 12, height = 12, units = 'in', res = 300)
print(plot)
dev.off()
