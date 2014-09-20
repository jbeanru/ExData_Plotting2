#
# Description:
#	plot 4 - showing  the trends in emissions from coal combustion-related 
#	sources, from 1999–2008  
#
# Limitations:
#	The data files to be loaded should exist in the working directory, and 
#	the plot4.png will be generated in the working directory too.
#
# Related information:
#	http://www.epa.gov/ttn/chief/eiinformation.html
#	https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#

library(plyr)
library(ggplot2)

# load the data
SCC <- readRDS("Source_Classification_Code.rds")
NEI <- readRDS("summarySCC_PM25.rds")

# determine the "coal combustion-related sources"
# 	It is vague here in the project instructions. 
#	I choose the following rule, but I admit there are others.
#	e.g. according to SCC#EI.Sector etc.
coalCombSCC <- SCC[grepl("(comb.*coal)|(coal.*comb)", SCC$Short.Name, 
			 ignore.case = T),]
coalCombNEI <- NEI[NEI$SCC %in% coalCombSCC$SCC,]

# calculate totals, grouped by years
#	I prefer to use the total and show points or bars, because the emissions 
#	are from different sources. Aggregation is more meaningful.
yearlyTotals <- ddply(coalCombNEI, .(year), summarize, totalEmissions = 
			sum(Emissions))

# create plot using ggplot2
p <- ggplot(yearlyTotals, aes(x = year, y = totalEmissions))

# draw points and trend line.
p <- p + geom_point(size = 3)  + geom_smooth(method = "lm", linetype = 2) 

# set labels and theme
p <- p + labs(title = "Plot 4 - PM2.5 Emission of from Coal Combustion", 
	      x = "Years", y ="Total PM2.5 Emission(Tons)") + theme_bw() 

# open a PNG device of reasonable size
png(filename = "plot4.png", type = "cairo", bg = "white", height=640, width=640)

# print plot object p to the PNG device
print(p)
dev.off()

# should clear the local varibles here in real solution
