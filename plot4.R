########################################################################################################
# Coursera Data Science: Exploratory Data Analysis course
#
# Project 2: plot 4
# 1. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#  
#
############################################################################################################### 
#  
library(dplyr)
library(scales)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#
# select combustion-relateds emissions
coal <- SCC %>% filter(str_detect(EI.Sector, "Fuel Comb.*Coal"));
neicoal <- merge(NEI,coal, by="SCC")
# group by
neicoaltype <- neicoal %>% group_by(type, year) %>% summarize(total= sum(Emissions)) %>% select(total, type, year);
neicoalyear <- neicoal %>% group_by(year) %>% summarize(total = sum(Emissions)) %>% mutate(type = "TOTAL");
#
neicoalAll <- bind_rows(neicoaltype, neicoalyear);
#
png("plot4.png", width=880, height=480)
ggplot(neicoalAll, aes(x = factor(year), y = total, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Coal Combustion-Related Sources in United States"))) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)
  dev.off();