library(phyloseq)

data(GlobalPatterns)
GP <- prune_taxa(taxa_sums(GlobalPatterns) > 0, GlobalPatterns)
GP.chl <- subset_taxa(GP, Phylum=="Chlamydiae")


library(scales)
library(ggtree)
fig <- ggtree(GP.chl, ladderize = FALSE) + geom_text2(aes(subset=!isTip, label=label), hjust=-.2, size=4) +
    geom_tiplab(aes(label=Genus), hjust=-.3) +
    geom_point(aes(x=x+hjust, color=SampleType, shape=Family, size=Abundance),na.rm=TRUE) +
    scale_size_continuous(trans=log_trans(5)) +
    theme(legend.position="right")

df <- fortify(GP.chl)
barcode <- as.character(df$Barcode_full_length)
names(barcode) <- df$label
barcode <- barcode[!is.na(barcode)]
fig = msaplot(fig, Biostrings::BStringSet(barcode), width=.1, offset=.1)

png("ggtree.png", width=1200, height=840, res=120)
print(fig)
dev.off()

pdf("ggtree.pdf", width = 12, height = 8.4, useDingbats=F)
print(fig)
dev.off()
