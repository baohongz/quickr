library(pheatmap)

# Read in data files
test = as.matrix(read.table(file="test.txt",header=TRUE,sep="\t",check.names=FALSE))
annotation_col = as.data.frame(read.table(file="annotation_col.txt",header=TRUE,sep="\t",check.names=FALSE))
annotation_row = as.data.frame(read.table(file="annotation_row.txt",header=TRUE,sep="\t",check.names=FALSE))

paletteLength <- 50
myColor <- colorRampPalette(c("navy", "white", "firebrick3"))(paletteLength)
# length(breaks) == length(paletteLength) + 1
# use floor and ceiling to deal with even/odd length pallettelengths
myBreaks <- c(seq(min(test), 0, length.out=ceiling(paletteLength/2) + 1), seq(max(test)/paletteLength, max(test), length.out=floor(paletteLength/2)))

# Draw heatmaps
pheatmap(test, kmeans_k = 2, scale = "row", clustering_distance_rows = "correlation")
pheatmap(test, color = myColor, breaks = myBreaks)

# Show text within cells
pheatmap(test, cluster_row = FALSE, legend_breaks = -1:4, legend_labels = c("0", "1e-4", "1e-3", "1e-2", "1e-1", "1"))

# Fix cell sizes and save to file with correct size
pheatmap(test, cellwidth = 15, cellheight = 12, main = "Example heatmap")

# Display row and color annotations # Gaps in heatmaps

pheatmap(test, color = myColor, breaks = myBreaks, annotation_col = annotation_col, annotation_row = annotation_row, annotation_legend = TRUE , cluster_row = TRUE, legend = TRUE, cutree_col = 2, cutree_row = 3, display_numbers = matrix(ifelse(test > 5, sprintf("%.2f",test), ""), nrow(test)), width = 8, height = 8, filename = "heatmap.pdf")

pheatmap(test, color = myColor, breaks = myBreaks, annotation_col = annotation_col, annotation_row = annotation_row, annotation_legend = TRUE , cluster_row = TRUE, legend = TRUE, cutree_col = 2, cutree_row = 3, display_numbers = matrix(ifelse(test > 5, sprintf("%.2f",test), ""), nrow(test)), width = 8, height = 8, filename = "heatmap.png")
