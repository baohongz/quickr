library(ggthemr)
library(survminer)
library(survival)

fit2 <- survfit( Surv(time, status) ~ rx + adhere,
    data = colon )

ggthemr("light")

g = ggsurvplot(fit2, pval = TRUE,
           break.time.by = 800,
           risk.table = TRUE,
           risk.table.col = "strata",
           risk.table.height = 0.5,
           palette = swatch()[1:6],
           ggtheme = NULL,
		   font.x = NULL,
		   font.y = NULL,
           legend.labs = c("A", "B", "C", "D", "E", "F"))

png("survival.png", width=800, height=800, res=120)
print(g)
dev.off()

pdf("survival.pdf", width = 8, height = 8, useDingbats=F)
print(g)
dev.off()

