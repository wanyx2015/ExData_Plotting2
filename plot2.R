##
## Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to
## make a plot answering this question.
##

library(dplyr)

rm(list = ls())
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# filter the Baltimore data
baltimore <- filter(NEI, fips == "24510")

# calculate the sum of emissions group by year
data <- summarize(group_by(baltimore, year), emissions = sum(Emissions))

# Let us plot
png("./plot2.png")

# set xaxt = n to disable x-axis
plot(data$year, data$emissions, type = "b",
     xaxt = "n", xlab = "Year", ylab = "PM2.5 Emissions (tons)",
     main = "Total Baltimore PM2.5 Emissions (1999 ~ 2008)", 
     lwd = 2, col = "blue")

# customize the x-axis, because default axis has no 1999
axis(side = 1, at = data$year)

dev.off()
