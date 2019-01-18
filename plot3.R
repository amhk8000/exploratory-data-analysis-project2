########################################################################################################
# Coursera Data Science: Exploratory Data Analysis course
#
# Project 2: plot 3
# 1. which of the sources (point, nonpoint, onroad, nonroad) have seen decreases in emissions from 1999 to2008
#  for Baltimore City? 
#
############################################################################################################### 
#  
# plot 3 
#
library(ggplot2);
library(dplyr);
library(scales);
library(tidyverse);
#
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## get baltimore emission and aggregate by year and sources
baltimore <- NEI %>% filter(fips == "24510") %>% select(year, type, Emissions);
aggbaltimore <- baltimore %>% 
			    group_by(year, type)  %>% 
				summarise(total=sum(Emissions))

aggbaltimore$type 
				# 
png("plot3.png", width=480, height=480)

ggplot(aggbaltimore, aes(x = factor(year), y = total, fill = type)) + 
  geom_bar(stat = "identity") + 
  facet_grid(. ~ type) + 
  xlab("Year") + 
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = comma) +
  guides(fill = FALSE)
dev.off();






