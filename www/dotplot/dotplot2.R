library(tidyverse)
library(ggdendro)
library(viridis)
library(cowplot)
library(patchwork)

gene_cluster <- read_tsv('https://github.com/davemcg/davemcg.github.io/raw/master/content/post/scRNA_dotplot_data.tsv.gz')
markers <- gene_cluster$Gene %>% unique()

# make data square to calculate euclidean distance
mat <- gene_cluster %>% 
  filter(Gene %in% markers) %>% 
  select(-cell_ct, -cell_exp_ct) %>%  # drop unused columns to faciliate widening
  pivot_wider(names_from = cluster, values_from = count) %>% 
  data.frame() # make df as tibbles -> matrix annoying
row.names(mat) <- mat$Gene  # put gene in `row`
mat <- mat[,-1] #drop gene column as now in rows
clust <- hclust(dist(mat %>% as.matrix())) # hclust with distance matrix


ddgram <- as.dendrogram(clust) # create dendrogram

ddata <- dendro_data(ddgram, type = 'rectangle') # extract into lists of data
gene_pos_table <- with( ddata$labels, data.frame(y_center = x, gene = as.character(label), height = 1))
# axis munging <- This is where the magic happens
gene_axis_limits <- with( gene_pos_table, c(min(y_center - 0.5 * height), max(y_center + 0.5 * height))) +  0.1 * c(-1, 1)

ddata <- with( segment(ddata), data.frame(x = y, y = x, xend = yend, yend = xend))

fancy_tree_plot <-  ggplot((ddata)) + geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  scale_x_reverse(expand = c(0, 0.5)) + 
  scale_y_continuous(breaks = gene_pos_table$y_center, 
                     labels = gene_pos_table$gene, 
                     limits = gene_axis_limits, 
                     expand = c(0, 0)) + 
  labs(x = "Distance", y = "", colour = "", size = "") + theme_dendro()

dotplot <- gene_cluster %>% filter(Gene %in% markers) %>% 
  mutate(`% Expressing` = (cell_exp_ct/cell_ct) * 100,
         Gene = factor(Gene, levels = gene_pos_table$gene)) %>% 
  filter(count > 0, `% Expressing` > 1) %>% 
  ggplot(aes(x=cluster, y = Gene, color = count, size = `% Expressing`)) + 
  geom_point() + 
  cowplot::theme_cowplot() + 
  theme(axis.line  = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('') +
  theme(axis.ticks = element_blank()) +
  scale_color_gradientn(colours = viridis::viridis(20), limits = c(0,4), oob = scales::squish, name = 'log2 (count + 1)')
# + scale_y_discrete(position = "right")  # move the gene names to the right side 

p1 <- fancy_tree_plot + theme(plot.margin =  margin(t=0,r=-10,b=0,l=0,unit = "pt"))
p2 <- dotplot + theme(plot.margin =  margin(t=0,r=0,b=0,l=-10,unit = "pt"))
p3 <- p1 + p2 +plot_layout(widths = c(1, 3))
print(p3)


