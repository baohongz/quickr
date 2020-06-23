library(pheatmap)

matrix = read.table("Pathway_Analysis1.txt", header=TRUE,sep="\t",check.names=FALSE)

pheatmap(matrix, display_numbers = TRUE, cluster_cols=FALSE, cluster_rows=FALSE, fontsize_number = 10,fontsize = 10, number_color = "black", width = 8, height = 4.2, filename = "Pathway_Analysis1.pdf")

pheatmap(matrix, display_numbers = TRUE, cluster_cols=FALSE, cluster_rows=FALSE, fontsize_number = 10,fontsize = 10, number_color = "black", width = 8, height = 4.2, filename = "Pathway_Analysis1.png")

