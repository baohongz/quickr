library(ggplot2)
library(ggrepel)


mmur <- structure(list(Medgrowth = c(-1.02232983588915, 3.01155115511551,-0.220617729642996, 1.96506550218342, 0.943970767356888, 0.810810810810807,0.0166694449074782, 0.21064457239153, 0.0876731544801004, 0.132216835610393,0.370644922164558,0.23378141437756, 1.27810650887574, 0.42301184433164,0.394880174291941, 0.54216172568924, 1.32690882134916, 0.499722376457527,-0.108885017421599), Medunemp = c(4.430550475, 2.5060469975,4.1239796475, 2.0585977455, 3.846659243, 3.1792594425, 4.0033450105,6.0882984255, 3.091889808,3.7462810695, 2.4038147815, 3.0065393475,2.3331894185, 4.9482480125, 2.0955470885, 1.616694725, 1.873037069,3.060170157, 3.0131425595), Empsize = c(324.2,270.6, 962.1,149, 962.4, 421.1, 1197.8, 777.8, 552.8, 234.8, 421.1, 203.2,915.7, 396.1, 685.9, 904.5, 1366.9, 215.4, 440.5), Eduratio = c(0.1,0.2, 0.1, 0.2, 0.1, 0.2, 0.1, 0.1, 0.1, 0.3, 0.3, 0.2, 0.5, 0.2,0.3, 0.6, 0.4, 0.2, 0.1), Names = structure(c(3L, 12L, 11L, 7L,5L, 19L, 17L, 1L, 18L, 10L, 8L, 16L, 14L, 2L, 15L, 6L, 9L, 4L,13L), .Label = c("Accom", "Admin","Agric", "Arts.", "Const","Educa", "Elect", "Finan", "Healt","Infor","Manuf","Minin","Other", "Profe", "Publi", "Renta", "Retai", "Trans", "Whole"), class = "factor"), colour1 = structure(c(6L, 5L, 6L, 5L, 6L,5L, 6L, 6L, 6L, 4L, 4L, 5L, 2L, 5L, 4L, 1L, 3L, 5L, 6L), .Label = c("#8C2D04","#CC4C02", "#EC7014", "#FE9929","#FEC44F", "#FEE391"), class = "factor")), .Names = c("Medgrowth","Medunemp", "Empsize", "Eduratio", "Names", "colour1"), row.names = c("Agric","Minin", "Manuf", "Elect", "Const", "Whole", "Retai", "Accom","Trans", "Infor", "Finan", "Renta", "Profe", "Admin", "Publi","Educa", "Healt", "Arts.", "Other"), class = "data.frame")



plot = ggplot(mmur, aes(x = Medgrowth, y = Medunemp, size = Empsize, label = Names, fill = colour1)) +
  geom_point(shape=21, alpha=0.5) + 
  scale_size(range = c(5, sqrt(max(mmur$Empsize)/min(mmur$Empsize)*5^2)), name = "Employment in\n2012 (thousands)") +
  geom_text_repel(size = 4, colour = "black" ) + 
  scale_fill_manual(values = levels(mmur$colour1), name = "Per cent with\ntertiary degree", label = c(60, 50, 40, 30, 20, 10)) + 
  xlab("Median employment growth rate 2001 - 2012") +
  ylab("Median unemployment rate 2001 - 2012") + 
  ylim(c(0,7))


png("bubble.png", width=600, height=600, res=120)
print(plot)
dev.off()

