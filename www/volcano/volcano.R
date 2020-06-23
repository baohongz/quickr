library(ggrepel)

set.seed(42)

genes <- read.table(gzfile("test.txt.gz"), check.names=FALSE, quote="\"", sep="\t", header = TRUE)

genes$Top <- ifelse(genes$FDR > 0.05, "Not Sig", ifelse(genes$logFC>0, "Up", "Down"))

Up <- length(which(genes$Top=="Up"))
Down <- length(which(genes$Top=="Down"))


# only label top 20 genes
cutoff = sort(genes$FDR, decreasing = FALSE)[21]

g <- ggplot(genes, aes(x = logFC, y = -log10(FDR))) +
	geom_point(aes(color = Top)) +
	scale_color_manual(values = c("red", "grey", "green")) +
	theme_bw(base_size = 12) +
	geom_text_repel(data = subset(genes, FDR < cutoff), aes(label = gene_name), size = 3, box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines"))

g <- g + theme(legend.position="none")

g <- g + annotate(geom="text", x=-5, y=-5, label=paste("down-regulated:",Down), color="black", size=3, fontface="bold") 
g <- g + annotate(geom="text", x=5, y=-5, label=paste("up-regulated:",Up), color="black", size=3, fontface="bold") 


png("volcano.png", width=600, height=600, res=120)
print(g)
dev.off()

# too big
#pdf("volcano.pdf", width = 8, height = 8, useDingbats=F)
#print(g)
#dev.off()
