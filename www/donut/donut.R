library(ggplot2)
library(ggrepel)

col <- c("white", "black", "transparent", "grey", "blue", "yellow", "green", "red", "pink", "orange", "brown")
freq <- c(101, 68, 34, 18, 14, 5, 5, 3, 2, 1, 1)

## create data frame
colour.df <- data.frame(col, freq)

## calculate percentage 
colour.df$percentage = colour.df$freq / sum(colour.df$freq)* 100
colour.df = colour.df[rev(order(colour.df$percentage)), ]
colour.df$ymax = cumsum(colour.df$percentage)
colour.df$ymin = c(0, head(colour.df$ymax, n = -1))

donut = ggplot(colour.df, aes(fill = col, ymax = ymax, ymin = ymin, xmax = 100, xmin = 80)) +
    geom_rect(colour = "black") + coord_polar(theta = "y") + xlim(c(0, 100)) +
    geom_label_repel(aes(label = paste(round(percentage,2),"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 4) +
    theme(legend.title = element_text(colour = "black", size = 12, face = "bold"), legend.text = element_text(colour = "black", size = 10), panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
    annotate("text", x = 0, y = 0, size = 12, label = "Micro")

png("donut.png", width=600, height=600, res=120)
print(donut)
dev.off()
