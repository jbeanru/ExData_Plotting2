#
# Description:
#	plot 6 - Compare the emissions from motor vehicle sources of in Baltmore 
#	City with Los Angeles County, from 1999–2008.
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
#
motorSCC <- SCC[SCC$Data.Category == "Onroad",]

# get emission data of motor vehicle, Baltmore City and Los Angeles County
motorNEI <- NEI[NEI$fips %in% c("24510","06037") & NEI$SCC %in% motorSCC$SCC,]

# calculate totals, grouped by years, fips
#	I prefer to use the total and show points or bars, because the emissions 
#	are from different motor sources. Aggregation is more meaningful.
yearlyTotals <- ddply(motorNEI, .(year,fips), summarize, totalEmissions = 
			sum(Emissions))

# name the two cities with meaningful names, according to fips
yearlyTotals$City <- factor(yearlyTotals$fips, levels = c("24510","06037"), 
			    labels = c("Baltmore", "Los Angeles"))

# calculate yearly changes from 1999
# 	I'll compare the changes from 1999 to find which of the 2 cities has
#	greater changes. I admit there are many other reasonable solutions.
yearlyTotals$Changes <- yearlyTotals$totalEmissions - 
	yearlyTotals[yearlyTotals$year== "1999",]$totalEmissions

# using different color to plot the yearly changes of 2 cities
p <- ggplot(yearlyTotals, aes(x = year, y = Changes, color = City))

# draw points for changes of the cities over years
# 	the point size reflect the absolute value of changes
p <- p + geom_point(aes(size = abs(Changes)))

# draw lines to connect the points
p <- p + geom_line()

# draw x Axis explicitly
p <- p + geom_hline(yintercept=0, colour="grey", size = 1)
# set title and axis labels
p <- p + labs(title = "Plot 6 - Comparison of Changes Over Time in Motor Vehicle Emissions", 
	      x = "Years", y ="Changes in PM2.5 Emission(Tons)") + theme_bw() 

# draw a mirrorr in dotted line for Baltmore, using absolute value, to make 
# the comparison of changes more easily
p <- p + geom_point(aes(y = abs(Changes), size = abs(Changes))) + 
	geom_line(aes(y = abs(Changes)), linetype = 2)
p <- p + geom_text(aes(2004, 170, color="Baltmore"), size = 4, fontface =3,
		   label="Mirror of Baltmore to show the\nabsolute changes of Baltmore\nfor better comparison")

# set legend labels
p <- p + labs(color = " Cities \nin Comparison", size = "Absolute Value\n of Changes")

# show conclustion text
p <- p + geom_text(aes(2004, -80), size = 4, color = "black", fontface = 3, 
	      label="Absolute changes of Los Angeles is greater.\nBut if we consider changes as efforts in \ndecreasing emission, it will be Baltmore")

# open a PNG device of reasonable size
png(filename = "plot6.png", type = "cairo", bg = "white", height = 720, width = 720)

# print plot object p to the PNG device
print(p)
dev.off()

# should clear the local varibles here in real solution