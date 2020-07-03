library(xlsx)
library(RColorBrewer)

wb <- createWorkbook()
sheet <- createSheet(wb, "Sheet1")

pal.list <- row.names(brewer.pal.info)
for (i in 1:length(pal.list)) {
	pal.name <- pal.list[i]
	color.num <- brewer.pal.info[pal.name,]$maxcolors

	color.codes <- brewer.pal(color.num,pal.name)
	print(color.codes)

	r <- createRow(sheet,i)
	cell <- createCell(r,1:20)
	setCellValue(cell[[1]], pal.name)
	for (j in 1:length(color.codes)) {
		col = j+1
		cellStyle <- CellStyle(wb) + Fill(foregroundColor=color.codes[j], pattern="SOLID_FOREGROUND")
		setCellStyle(cell[[col]], cellStyle)
	}
}

saveWorkbook(wb, "Rcolors.xlsx")
