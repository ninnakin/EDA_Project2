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

## Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission from all 
## sources for each of the years 1999, 2002, 2005, and 2008.

## Solution: 
# Sum all the emissions and plot by year
em.by.year<-tapply(NEI$Emissions, NEI$year, FUN=sum)
em.by.year<-0.001*em.by.year # get result in kilotons instead of tons
years <- unique(NEI.Baltimore$year)

# open png device 
png(filename="plot1.png")

# Plot how the sum changes by year
plot(years,
     em.by.year, 
     type="b", lwd=3, col="red", pch=20, xaxt="n",
     ylab="Emissions (kilotons)", 
     xlab="Year", 
     main="Total PM2.5 emissions by year",
     ylim=c(0,max(em.by.year))
)
abline(lm(em.by.year~ years), lwd=2, col="grey")
axis(1, years)

# close graphics device to save file
dev.off()



