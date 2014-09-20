#
# Description:
#	plot 5 - showing  the trends in emissions from motor vehicle sources,
#	from 1999–2008, in Baltmore City.
#
# Limitations:
#	The data files to be loaded should exist in the working directory, and 
#	the plot5.png will be generated in the working directory too.
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

# determine the "motor vehicle sources"
# 	It is vague here in the project instructions. 
#	I use the definition of "motor vehicle = on road", please see:
#		http://en.wikipedia.org/wiki/Motor_vehicle
#		http://www.ucair.org/sources-of-emissions/

motorSCC <- SCC[SCC$Data.Category == "Onroad",]

# get emission data of motor vehicle, Baltmore City

motorNEI <- NEI[NEI$fips == "24510" & NEI$SCC %in% motorSCC$SCC,]

# calculate totals, grouped by years
#	I prefer to use the total and show points or bars, because the emissions 
#	are from different motor sources. Aggregation is more meaningful.

yearlyTotals <- ddply(motorNEI, .(year), summarize, totalEmissions = 
			sum(Emissions))

# create plot using ggplot2
p <- ggplot(yearlyTotals, aes(x = year, y = totalEmissions))
p <- p + geom_point(size = 3)  + geom_smooth(method = "lm", linetype = 2) 
p <- p + labs(title = "Plot 5 - PM2.5 Emission from Motor Vehicle in Baltmore", 
	      x = "Years", y ="Total PM2.5 Emission(Tons)") + theme_bw() 

# open a PNG device of reasonable size
png(filename = "plot5.png", type = "cairo", bg = "white", height=640, width=640)

# print plot object p to the PNG device
print(p)
dev.off()

# should clear the local varibles here in real solution
