library(MASS)
library(ggplot2)
n <- 1000
x <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))
df = data.frame(x); colnames(df) = c("x","y")

commonTheme = list(labs(color="Density",fill="Density",
                        x="RNA-seq Expression",
                        y="Microarray Expression"),
                   theme_bw(),
                   theme(legend.position=c(0.02,0.98),
                         legend.justification=c(0,1)))


g = ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + commonTheme


png("scatter.png", width=600, height=600, res=120)
print(g)
dev.off()

pdf("scatter.pdf", width = 8, height = 8, useDingbats=F)
print(g)
dev.off()

