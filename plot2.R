#
# Description:
#	plot 2 - showing the total PM2.5 emission of Baltimore City for each of 
#	the years 1999, 2002, 2005, and 2008
# Limitations:
#	The data files to be loaded should exist in the working directory, and 
#	the plot2.png will be generated in the working directory too.
#
# Related information:
#	http://www.epa.gov/ttn/chief/eiinformation.html
#	https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
#

# load the data
NEI <- readRDS("summarySCC_PM25.rds")

# only keep the data of Baltimore City
baltmoreNEI <- NEI[NEI$fips == "24510",]

# calculate totals for each year
totals <- tapply(baltmoreNEI$Emissions, baltmoreNEI$year, sum)

# start PNG graphics device to save chart output to png file
png(filename = "plot2.png", type = "cairo", bg = "white",
    height=480, width=480)

# draw a bar graphic
barplot(totals, main = "Plot 2 - PM2.5 Emission from All Sources in Baltimore", 
	xlab = "Years", ylab = "Total PM2.5 Emission(Tons)", col = "aliceblue")

# use liner regression model to show the trend
#	use bar positions because there are no exact x values in bar plot
xAxis <- 1:length(totals)
regressionModel <- lm(totals ~ xAxis)
abline(regressionModel, col = "red", lty = "dotted")

# turn off graphics device to flush output to png file
dev.off()

# should clear the local varibles here in real solution
