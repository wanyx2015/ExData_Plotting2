##
## Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions 
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city
## has seen greater changes over time in motor vehicle emissions?
##

## preparing the data
library(dplyr)
library(ggplot2)

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
ggplot(data1, aes(x = factor(year), y = Emissions)) +
  geom_point(aes(colour = factor(round(Emissions, 2))), size = 5) +
  facet_grid(fips~. , scales = "free") + 
  geom_smooth(method = "lm", aes(group = 1)) +
  labs(x = "Year") + 
  labs(y = "Total Emissions (tons)") +
  labs(title ="Emissions From Motor Vehicle in Baltimore and LA (1999 ~ 2008)") +
  #theme(plot.title = element_text(size = rel(1)))
  labs(colour="Emissions") +
  theme(legend.position = "bottom") 

# save the plot, default unit is inch
ggsave("./plot6.png", width = 7, height = 6, dpi = 300)
