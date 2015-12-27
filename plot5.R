##
## Question 5: How have emissions from motor vehicle sources changed from 1999¨C2008
## in Baltimore City?
##

## preparing the data
library(dplyr)
library(ggplot2)

rm(list = ls())
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## filter the emissions by "On-ROAD" and Baltimore (fips = 24510) or Baltimore
data <- select(filter(NEI, type == "ON-ROAD", fips == "24510"), year, Emissions)

## calculate the total emissions group by year
data1 <- summarize(group_by(data, year), Emissions = sum(Emissions))

## start the plot
ggplot(data1, aes(x = factor(year), y = Emissions)) +
  geom_point(aes(colour = factor(round(Emissions,2))), size = 5) +
  geom_path(aes(group = 1)) +
  geom_smooth(method = "lm", aes(group = 1)) +
  #geom_bar(stat = "identity", fill="steelblue") +
  labs(x = "Year") + 
  labs(y = "Total Emissions (tons)") +
  labs(title ="Emissions From Motor Vehicle in Baltimore (1999 ~ 2008)") +
  labs(colour="Emissions")

# save the plot, default unit is inch
ggsave("./plot5.png", width = 7, height = 6, dpi = 300)