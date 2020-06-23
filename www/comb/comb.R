library(ggplot2)
library(grid)
library(scales)
library(gridExtra)

df <- read.table("test.txt", header=TRUE, sep="\t", check.names=FALSE)
df$Time <- as.Date(df$Time) 

p <- ggplot(data=df[df$MEPSID==1,], aes(x=Time, y=RR, colour=ServiceID, group=ServiceID, label=round(RR)))+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  labs(y="Response Rate")+
  coord_cartesian(ylim=c(0, 110))+
  geom_line(size=.5)+ geom_point()+
  scale_color_manual(values=c("green4","blue4","red4","dodgerblue"))+
  scale_x_date(labels = date_format("%b"), breaks=date_breaks("month"))+
  facet_grid(~Year, scales="free", space="free")+
  ggtitle("Counts")+
  theme(plot.title=element_text(size=18, face="bold", vjust=1, hjust=0.5),
        axis.title=element_text(size=16),
        axis.text.x=element_text(size=10, angle=90),
        axis.line=element_line(colour="black", size=.2),
        legend.background = element_rect(fill="transparent"),
        legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.margin=margin(t = 0, unit='cm'),
    	plot.margin = unit(c(0,0.1, 0, 0.1), "lines"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(colour="gray", linetype="solid", size=.2))
p2 <- ggplot(data=df[df$MEPSID==1,], aes(x = Time, y = ServiceID, label=format(round(RR), nsmall=0), colour = ServiceID)) +
  scale_x_date(labels=c(), breaks=date_breaks("month"), expand=c(0.05,0.05))+
  geom_text(size = 3.5) +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0.1, 0.1, 0, 0.1), "lines")) +
  xlab(NULL) + ylab(NULL)+
  facet_grid(~Year, scales="free", space="free")+
  scale_y_discrete(limits=rev(levels(df$ServiceID)))+
  scale_color_manual(values=c("green4","blue4","red4","dodgerblue"))

comb <- grid.arrange(p + geom_blank(data = data.frame(Time = as.Date(c("2012-09-20", "2012-12-15", "2014-12-20", "2015-04-10")), RR = 1:4, Year = c(2012, 2012, 2015, 2015)), aes(colour = NULL, group = NULL, label = NULL)) , p2 + geom_blank(data = data.frame(Time = as.Date(c("2012-09-20", "2012-12-15", "2014-12-20", "2015-04-10")), ServiceID = 1:4, Year = c(2012, 2012, 2015, 2015)), aes(colour = NULL, group = NULL, label = NULL)), nrow=2, heights=c(5,1))

pdf("comb.pdf", width = 8, height = 8, useDingbats=F)
grid.arrange(comb)
dev.off()

png("comb.png", width=1200, height=1200,res=150)
grid.arrange(comb)
dev.off()



#grid.arrange(arrangeGrob(p,p2, nrow=2, heights=c(5,1)))
