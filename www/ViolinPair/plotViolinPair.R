# /hpc/grid/ngsws/btx_clinical/bzhang2/BioTx/MAdCAM/UC/tissue

# EDA_FN1.txt
#Sample,Ratio,SmapleMod,Subject,Dosage,Visit,Source
#US-1485654,0.6764,US-1485654,10561001,Placebo,BL,I
#US-1485657,0.5928,US-1485657,10031001,75mg,BL,NI
#US-1485658,0.9497,US-1485658,11171006,Placebo,W12,NI
#US-1485670,0.2892,US-1485670,10441004,225mg,BL,NI
#US-1485691,1.6107,US-1485691,10011013,Placebo,BL,I
#US-1485694,0.9698,US-1485694,10081002,7.5mg,BL,I
#US-1485700,0.7153,US-1485700,10081003,225mg,BL,I
#US-1485704,1.6840,US-1485704,11171010,22.5mg,BL,NI
#US-1485705,1.2972,US-1485705,10471007,Placebo,BL,I


library(ggplot2)

data <- read.csv("EDA_FN1.txt")


data <- data[data$Dosage!="Control",]

data$Dosage_f <- factor(data$Dosage, levels=c("Placebo","7.5mg","22.5mg","75mg","225mg"))


# violin not filled

p <- ggplot(data, aes(x=Dosage_f, y=Ratio, color=Source)) 

dodge <- position_dodge(width = 0.5)

p <- p + geom_violin(position=dodge) + geom_dotplot(binaxis="y", position=dodge, stackdir="center", binwidth=0.04) + geom_boxplot(width=.2, position =dodge)

p <- p + facet_wrap( ~ Visit, ncol = 2)


# p <- ggplot(data, aes(x=Source, y=Ratio))
# p <- p + geom_point(aes(color=Subject, size=3)) + geom_line(aes(group=Subject)) 
# p+ facet_wrap( ~ Visit+Dosage_f, ncol = 2)

pdf("EDA_FN1.pdf", width=6, height=4)
p
dev.off()

png("EDA_FN1.png", width=600, height=400)
p
dev.off()
