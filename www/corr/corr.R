library(corrplot)

M<-cor(mtcars)
 
cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
            lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
            uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
}
res1 <- cor.mtest(mtcars, 0.95)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


png("corr.png", width=600, height=600, res=120)
corrplot(M, method="color", col=col(200), type="upper", order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, p.mat = res1[[1]], sig.level = 0.01, insig = "blank", diag=FALSE )
dev.off()

pdf("corr.pdf", width = 8, height = 8, useDingbats=F)
corrplot(M, method="color", col=col(200), type="upper", order="hclust", addCoef.col = "black", tl.col="black", tl.srt=45, p.mat = res1[[1]], sig.level = 0.01, insig = "blank", diag=FALSE )
dev.off()
