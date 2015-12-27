##
## Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city
## has seen greater changes over time in motor vehicle emissions?
##
## Note: This script use gridExtra to plot.
## Purpose: To make legend for EACH Facet.
##

## preparing the data
library(dplyr)
library(ggplot2)
library(gridExtra)

rm(list = ls())
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## filter the emissions by "On-ROAD" and LA (fips = 06037) or Baltimore (fips = 24510)
filted <- filter(NEI, type == "ON-ROAD", fips == "06037" | fips == "24510")
data <- select(filted, fips, year, Emissions)

## calculate the total emissions group by year
data1 <- summarize(group_by(data, fips, year), Emissions = sum(Emissions))

data1$fips <- as.factor(data1$fips)

## change factor names
levels(data1$fips)[levels(data1$fips) == "06037"] <- "Los Angeles"
levels(data1$fips)[levels(data1$fips) == "24510"] <- "Baltimore"


## start the plot

# grid.arrange has problem saving plot to png, use pdf instead
pdf(file = "plot6new.pdf", width = 7, height = 10)

# extract city factor
city <- split(data1, f = data1$fips)

p1 <- ggplot(city$"Baltimore", aes(x = factor(year), y = Emissions)) +
  geom_point(aes(colour = factor(round(Emissions, 2))), size = 5) +
  facet_grid(fips~. , scales = "free") + 
  geom_smooth(method = "lm", aes(group = 1)) +
  labs(x = "Year") + 
  labs(y = "Total Emissions (tons)") +
  labs(title ="Emissions From Motor Vehicle in Baltimore and LA (1999 ~ 2008)") +
  labs(colour="Emissions") +
  theme(legend.position = "bottom") 

# grid.arrange trick
p2 <- p1 %+% city$"Los Angeles"

grid.arrange(p1, p2)

# g <- arrangeGrob(grobs = c( p1, p2), nrow = 2)

dev.off()
