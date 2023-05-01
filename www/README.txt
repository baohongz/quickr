* Put into a directory on the HTTP server that can be accessed.

* A bug in opencpu-0.4.js to generate wrong location url for CORS call, e.g. http://xyz.comhttp://xyz.com/ocpu/tmp/x0b76faef0045bf

* opencpu server added "\" before slash of html tage, e.g. <\/hr>, then c("<\/hr>") in R can't handle it. Workaround, use htmltools, e.g. plotly_html.html

	or use javascript to generate HTML tags
	
	```{js}
	some javascript code in here
	```
	
	# The following alternatives won't work
	~~~{=html}
	```{r, echo=FALSE, results='asis'}
	input <- data.frame(text = c("a", "b", "c"), 
	                    page_number = c(3, 5, 6))
	links <- paste('<a href="', input$text, '">', input$page_number, "</a>", sep="")
	cat(links, sep = "\n")
	```
	~~~
	
	
	<!--html_preserve--> 
	<div><select id=“selector”></select></div> 
	<div class=“equation”></div> 
	<div class=“equation”></div> 
	<div class=“chart”></div> 
	<!--/html_preserve-->

* cat opencpu-server.js 
$(function() {
//  ocpu.seturl("https://opencpu.bxgenomics.com/ocpu/library/QuickR/R");
//  ocpu.seturl("https://3.22.86.41/ocpu/library/QuickR/R");
ocpu.seturl("http://quickr.ai/ocpu/library/QuickR/R");
});
