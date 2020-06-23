library(esetVis)
library(ALL)
data(ALL)

# to get gene annotation from probe IDs (from the paper HGU95aV2 gene chip was used)
library("hgu95av2.db")
library("AnnotationDbi")
probeIDs <- featureNames(ALL)
geneInfo <- AnnotationDbi::select(hgu95av2.db, probeIDs, 
    c("ENTREZID", "SYMBOL", "GENENAME"), "PROBEID")
# 482 on the 12625 probe IDs don't have ENTREZ ID/SYMBOL/GENENAME

# remove genes with duplicated annotation: 1214
geneInfoWthtDuplicates <- geneInfo[!duplicated(geneInfo$PROBEID), ]

# remove genes without annotation: 482
genesWthtAnnotation <- rowSums(is.na(geneInfoWthtDuplicates)) > 0
geneInfoWthtDuplicatesAndWithAnnotation <- geneInfoWthtDuplicates[!genesWthtAnnotation, ]

probeIDsWithAnnotation <- featureNames(ALL)[featureNames(ALL) %in% 
    geneInfoWthtDuplicatesAndWithAnnotation$PROBEID]
ALL <- ALL[probeIDsWithAnnotation, ]

fData <- geneInfoWthtDuplicatesAndWithAnnotation[
    match(probeIDsWithAnnotation, geneInfoWthtDuplicatesAndWithAnnotation$PROBEID), ]
rownames(fData) <- probeIDsWithAnnotation
fData(ALL) <- fData

# grouping variable: B = B-cell, T = T-cell
groupingVariable <- pData(ALL)$BT

# create custom palette
colorPalette <- c("dodgerblue", 
    colorRampPalette(c("white","dodgerblue2", "darkblue"))(5)[-1], 
    "red", colorRampPalette(c("white", "red3", "darkred"))(5)[-1])
color <- groupingVariable; levels(color) <- colorPalette

# reformat type of the remission
remissionType <- ifelse(is.na(ALL$remission), "unknown", as.character(ALL$remission))
ALL$remissionType <- factor(remissionType,
    levels = c("unknown", "CR", "REF"), 
    labels = c("unknown", "achieved", "refractory"))

g = print(esetSpectralMap(eset = ALL, 
        title = paste("Acute lymphoblastic leukemia dataset \n",
            "Spectral map \n Label outlying samples and genes"),
        colorVar = "BT", color = colorPalette,
        shapeVar = "sex",
        sizeVar = "age", sizeRange = c(2, 6),
        alphaVar = "remissionType", alpha = c(0.3, 0.6, 0.9),
        topGenes = 10, topGenesVar = "SYMBOL",
        topGenesCex = 2, topGenesColor = "darkgrey",
        topSamples = 15, topSamplesVar = "cod", topSamplesColor = "chocolate4",
        topSamplesCex = 3
    ))
 
png("SpectralMap.png", width=800, height=800, res=120)
print(g)
dev.off()

pdf("SpectralMap.pdf", width = 8, height = 8, useDingbats=F)
print(g)
dev.off()

