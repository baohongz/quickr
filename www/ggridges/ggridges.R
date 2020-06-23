library(tidyverse)
library(readr)
library(ggjoy)

colpos1 <- c(4, 6, 3, 2, 8, 5, 15, 10, 10, 10, 10, 8, 11, 5, 5, 10)
colnames <- c("congress", "icpsr", "st_code", "cd", "st_name", "party_code", "mc_name", "dim_1", "dim_2", "dim_1_se", "dim_2_se", "dim_1_2_corr,", "log_lik", "num_votes", "num_class_err", "geo_mean_prob")

all_house <- read_fwf("http://localhost/test/niceplot/ggridges/HL01113D21_BSSE.DAT", fwf_widths(colpos1, col_names=colnames))

all_house_88_113 <- all_house %>% filter(congress >= 88 & cd !=0 & cd != 98 & cd != 99) %>% filter(party_code == 100 | party_code == 200) %>% arrange(desc(congress)) %>% mutate(year1 = congress * 2 + 1787) %>% arrange(desc(year1))

fig = ggplot(all_house_88_113, aes(x = dim_1, y = year1, group=year1)) +
        geom_joy(data=filter(all_house_88_113, party_code==100), scale = 7, size = 0.25, rel_min_height = 0.01, fill="blue", alpha=0.2) +
        geom_joy(data=filter(all_house_88_113, party_code==200), scale = 7, size = 0.25, rel_min_height = 0.01, fill="red", alpha=0.2) +
        theme_joy() +
        #theme(axis.text.y = element_blank()) +
        scale_x_continuous(limits=c(-1, 1.3), expand = c(0.01, 0), breaks=c(-1,-.75,-.5,-.25,0,.25,.5,.75,1)) +
        #scale_y_continuous(breaks=c(seq(2010,1970,-5))) +
        scale_y_reverse(breaks=c(seq(2013,1963,-10))) +
        ggtitle("DW-NOMINATE by party of U.S. House: 1963-2013") + 
        ylab("First Year of Each Congress") +
        xlab("Distribution of 1st Dimension DW-NOMINATE by party") 

png("ggridges.png", width=800, height=800, res=120)
print(fig)
dev.off()

pdf("ggridges.pdf", width = 8, height = 8, useDingbats=F)
print(fig)
dev.off()
