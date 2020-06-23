library(ggpubr)

expr <- read.delim("https://raw.githubusercontent.com/kassambara/data/master/expr_tcga.txt",stringsAsFactors = FALSE)

# show.legend=FALSE to remove strange "a" in legend
plot = ggscatter(expr, x = "GATA3", y = "ESR1", size = 0.3,color = "dataset", palette = "jco",facet.by = "dataset",add = "reg.line", conf.int = TRUE) + stat_cor(aes(color = dataset), method = "spearman", label.y = 6, show.legend=FALSE)

png("ggscatter.png", width=600, height=600, res=120)
print(plot)
dev.off()
