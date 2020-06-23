library(ggbiplot)
data(wine)

wine.pca <- prcomp(wine, scale. = TRUE)

g <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, 
              groups = wine.class, ellipse = TRUE, circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')


png("biplot.png", width=600, height=600, res=120)
print(g)
dev.off()

pdf("biplot.pdf", width = 8, height = 8, useDingbats=F)
print(g)
dev.off()
