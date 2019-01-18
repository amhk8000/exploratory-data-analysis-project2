###########################################################################################################
# Coursera Data Science: Exploratory Data Analysis course
#
# Project 2: plot 1
# 1.	Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#
############################################################################################################### 
#  
#
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get baltimore data and aggregate
em <- NEI %>% select(year, Emissions);
emUS <- aggregate(em$Emissions/1000000, list(em$year), FUN = sum)
colnames(emUS) <- c("year","total")
#
yrange = pretty(emUS$total); 

png("plot1.png", width=480, height=480)
#
plot(emUS$year,emUS$total, type = "l",lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions  From All US Sources"));
axis(1, at = emUS$year, labels = paste(emUS$year));
axis(2, at = yrange,  labels = paste(yrange, "M"));
box();
dev.off();

