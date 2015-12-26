##
## Question 4: Across the United States, how have emissions from coal 
## combustion-related sources changed from 1999¨C2008?
##

library(dplyr)
library(ggplot2)

rm(list = ls())
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# choose the 4 levels to screen coal related data which I think meaningful
scc.levels <- select(SCC, SCC.Level.One:SCC.Level.Four)

# search "coal" in each column, return a list with matching index
coaldata <- sapply(scc.levels, function(e) grep("coal", e, ignore.case = TRUE))

# get the unique index for coal related data in SCC
idx <- unique(unlist(coaldata))

# get the SCC by idx
coalscc <- SCC[idx, "SCC"]

# convert SCC from factor/df to char vector
coalscc.v <- as.character(coalscc)

data <- NEI[NEI$SCC %in% coalscc.v, c("year", "Emissions")]

data1 <- summarize(group_by(data, year), Emissions = sum(Emissions)/1000)

## start the plot
ggplot(data1, aes(x = factor(year), y = Emissions)) +
  geom_point(aes(colour = factor(round(Emissions, 2))), size = 5) +
  geom_path(aes(group = 1)) +
  geom_smooth(method = "lm", aes(group = 1)) +
  #geom_bar(stat = "identity", fill="steelblue") +
  labs(x = "Year") + 
  labs(y = "Total Emissions (thousands of tons)") +
  labs(title ="Coal Combustion Related Emissions in US (1999 ~ 2008)") +
  labs(colour="Emissions")

ggsave("./plot4.png", width = 7, height = 6, dpi = 300)