##
## Question 1: Of the four types of sources indicated by the type (point, 
## nonpoint, onroad, nonroad) variable, which of these four sources have seen
## decreases in emissions from 1999¨C2008 for Baltimore City? Which have seen 
## increases in emissions from 1999¨C2008? Use the ggplot2 plotting system to 
## make a plot answer this question.
##


library(dplyr)
library(ggplot2)

rm(list = ls())
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# filter the Baltimore data
baltimore <- filter(NEI, fips == "24510")

# calculate the sum of Emissions group by type and year
data <- summarize(group_by(baltimore, type, year), Emissions = sum(Emissions))

## Let us plot
ggplot(data, aes(x = factor(year), y = Emissions)) +
  geom_point() +
  facet_wrap(~type, scales = "free") +
  geom_smooth(method = "lm", aes(group = 1)) +
  labs(x = "Year") + 
  labs(y = "Total PM2.5 Emissions (tons)") +
  labs(title ="Total PM2.5 Emissions in Baltimore (1999 - 2008)")

ggsave("./plot3.png", width = 7, height = 6, dpi = 300)

dev.off()