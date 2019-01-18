########################################################################################################
# Coursera Data Science: Exploratory Data Analysis course
#
# Project 2: plot 5
# 1. Emissions from motor vehicle sources in Baltimore city
#  
#
############################################################################################################### 
# #Question 5
#
library(ggplot2);
library(dplyr);
library(scales);

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get baltimore data 
baltimore <- NEI %>% filter(fips == "24510");

mobile <-  SCC %>% filter(str_detect(SCC.Level.Two, regex('vehicle', ignore_case=T)));

baltem <- merge(baltimore,mobile, by="SCC")

# find totals by year or type, year groups
baltemtype <- baltem %>% group_by(type, year) %>% summarize(total= sum(Emissions)) %>% select(total, type, year);
baltemyear <- baltem %>% group_by(year) %>% summarize(total= sum(Emissions)) %>% mutate(type = "TOTAL");
baltemtotal <- bind_rows(baltemtype, baltemyear);

png("plot5.png",width=880,height=480)

ggplot(baltemtotal, aes(x = factor(year), y = total, fill = type )) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions from Motor Vehicle Sources in Baltimore City"))) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = FALSE)  
  dev.off()
 
 