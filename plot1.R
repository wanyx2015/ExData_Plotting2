##
## Question 1: Have total emissions from PM2.5 decreased in the United States from 
## from 1999 to 2008? Using the base plotting system, make a plot showing the total
## PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
##

library(dplyr)

# preparing the data
rm(list = ls())
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# calculate the sum of emissions group by year
data <- summarize(group_by(NEI, year), emissions = sum(Emissions))

# Let us plot
png("./plot1.png")

# set xaxt = n to disable x-axis
plot(data$year, data$emissions/1000, type = "b",
     xaxt = "n", xlab = "Year", ylab = "PM2.5 Emissions (thousands of tons)",
     main = "Total US PM2.5 Emissions (1999 ~ 2008)", 
     lwd = 2, col = "blue")

# customize the x-axis, because default axis has no 1999
axis(side = 1, at = data$year)

dev.off()