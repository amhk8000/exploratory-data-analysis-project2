########################################################################################################
# Coursera Data Science: Exploratory Data Analysis course
#
# Project 2: plot 6
# 1. compare emission from motor vehicle source in Baltimore City and Los Angeles county
#  
#
############################################################################################################### 
# 
#
library(ggplot2);
library(dplyr);
library(scales);

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get baltimore and Los Angeles emissions
sources  <- NEI %>% filter(fips == "24510" | fips == "06037");
# get motor vehicle SCC
vehicle <-  SCC %>% filter(str_detect(SCC.Level.Two, regex('vehicle', ignore_case=T)));

emcity <- merge(sources, vehicle, by="SCC")

emcity$fips[emcity$fips=="24510"] <- "Baltimore"

emcity$fips[emcity$fips=="06037"] <- "Los Angeles"

emtot1  <- emcity %>% group_by(fips, type, year) %>% summarize(total= sum(Emissions))
emtot2  <- emcity %>% group_by(fips, year) %>% summarize(total= sum(Emissions)) %>% mutate(type = "total");
emtotal <- bind_rows(emtot1, emtot2);
    title.theme = element_text(
      size = 15,
      face = "italic",
      colour = "red",
      angle = 0
##
png("plot6.png", width=1040, height=480)
ggplot(emtotal, aes(factor(year), total, fill = type )) +
facet_grid(~ fips ) +
geom_bar(stat="identity")  +
xlab("year") +
ylab(expression('Total PM'[2.5]*" Emissions")) +
ggtitle('Total Emissions from motor vehicle source in Baltimore City, MD  vs Los Angeles, CA  for 1999-2008') +
theme(plot.title = element_text(hjust = 0.5)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill =
  guide_legend(reverse = TRUE
    )
  )
  
dev.off()