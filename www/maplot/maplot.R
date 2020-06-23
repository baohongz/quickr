library(ggpubr)

data(diff_express)

# Add rectangle around labesl
g = ggmaplot(diff_express, main = expression("Group 1" %->% "Group 2"),
   fdr = 0.05, fc = 2, size = 0.4,
   palette = c("#B31B21", "#1465AC", "darkgray"),
   genenames = as.vector(diff_express$name),
   legend = "top", top = 20,
   font.label = c("bold", 11), label.rectangle = TRUE,
   font.legend = "bold",
   font.main = "bold",
   ggtheme = ggplot2::theme_minimal())

png("maplot.png", width=600, height=600, res=120)
print(g)
dev.off()

# big file
#pdf("maplot.pdf", width = 8, height = 8, useDingbats=F)
#print(g)
#dev.off()
