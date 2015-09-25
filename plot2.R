## Code for constructing plot 1 for the second course project in the Exploratory Data Analysis Course


## Description of columns of Source_Classification_Code.rds
#  fips: A five-digit number (represented as a string) indicating the U.S. county
#  SCC: The name of the source as indicated by a digit string (see source code classification table)
#  Pollutant: A string indicating the pollutant
#  Emissions: Amount of PM2.5 emitted, in tons
#  type: The type of source (point, non-point, on-road, or non-road)
#  year: The year of emissions recorded

## read file from exdata-data-NEI_data folder and store in NEI 
NEI <- readRDS("exdata-data-NEI_data\\summarySCC_PM25.rds")

## Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
## to make a plot answering this question.

# Sum the emissions by year for fips=24510
NEI.Baltimore <- subset(NEI, fips=="24510", select=c(Emissions,year))
em.by.year<-tapply(NEI.Baltimore$Emissions, NEI.Baltimore$year, FUN=sum)
years <- unique(NEI.Baltimore$year)
  
# open png device 
png(filename="plot2.png")

# Plot how the sum changes by year
plot(years,
     em.by.year, 
     type="b", lwd=3, col="red", pch=20, xaxt="n",
     ylab="Total Emissions (tons)", 
     xlab="Year", 
     main="Total PM2.5 emissions by year in Baltimore City",
     ylim=c(0,max(em.by.year))
)
abline(lm(em.by.year~ years), lwd=2, col="grey")
axis(1, years)
legend("bottomleft",c("PM2.5 emissions (tons)", "Regression line"), col=c("red","grey"), lwd=c(3,2))

# close graphics device to save file
dev.off()








