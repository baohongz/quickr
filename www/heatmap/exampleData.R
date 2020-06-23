# Create test matrix
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Sample", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")
write.table(formatC(test, digits = 3, format = "f"), file = "test.txt", sep="\t", quote=FALSE,  row.names = TRUE)

# Read in data file
test = as.matrix(read.table(file="test.txt",header=TRUE,sep="\t",check.names=FALSE))

# Generate annotations for rows and columns
annotation_col = data.frame( CellType = factor(rep(c("CT1", "CT2"), 5)), Time = 1:5)
rownames(annotation_col) = colnames(test);
write.table(annotation_col, file="annotation_col.txt", sep="\t", quote=FALSE,  row.names = TRUE)

annotation_row = data.frame( GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6))))
rownames(annotation_row) = rownames(test);
write.table(annotation_row, file="annotation_row.txt", sep="\t", quote=FALSE,  row.names = TRUE)
