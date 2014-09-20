#
# Description:
#	plot 3 - showing  the trends in emissions from 1999–2008 for each type 
#	of the sources.
#
# Limitations:
#	The data files to be loaded should exist in the working directory, and 
#	the plot3.png will be generated in the working directory too.
#
# Related information:
#	http://www.epa.gov/ttn/chief/eiinformation.html
#	https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#

library(plyr)
library(ggplot2)

# load the data
NEI <- readRDS("summarySCC_PM25.rds")

# only keep the data of Baltimore City
baltmoreNEI <- NEI[NEI$fips == "24510",]

# calculate totals, grouped by years, types
totals <- ddply(baltmoreNEI, .(type,year), summarize, totalEmissions = 
			sum(Emissions))

# create plot 3 using ggplot2
p <- ggplot(totals, aes(x = year, y = totalEmissions, color = type, shape = type))
p <- p + geom_point(size = 3)  + geom_smooth(method = "lm", linetype = 2) + 
	facet_wrap(~type)
p <- p + labs(title = "Plot 3 - PM2.5 Emission by Type of Sources in Baltimore", 
	      x = "Years", y ="Total PM2.5 Emission(Tons)") + theme_bw() 

# open a PNG device of reasonable size
png(filename = "plot3.png", type = "cairo", bg = "white", height=800, width=800)

# print plot object p to the PNG device
print(p)
dev.off()

# should clear the local varibles here in real solution