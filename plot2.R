###########################################################################################################
# Coursera Data Science: Exploratory Data Analysis course
#
# Project 2: plot 2
# 1. show if total emissions from PM2.5 ecreased in Baltimore City, Maryland (fips == “24510”) from 1999 to 2008?  
#
############################################################################################################### 
#  
#
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get baltimore data and aggregate
baltimore <- NEI %>% filter(fips == "24510") %>% select(year, Emissions);
batotal <- aggregate(baltimore$Emissions/1000, list(baltimore$year), FUN = sum)
colnames(batotal) <- c("year","totalem")
#
yrange <- pretty(batotal$totalem)
#
png("plot2.png", width=480, height=480)
#
plot(batotal$year,batotal$totalem, type = "l", lwd = 2, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions (Thousands)"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in Baltimore, Maryland"));
axis(1, at = batotal$year, labels = paste(batotal$year));
axis(2, at = yrange,  labels = paste(yrange));
box();
dev.off();


