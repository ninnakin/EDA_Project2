## Code for constructing plot 3 for the second course project in the Exploratory Data Analysis Course


## Description of columns of Source_Classification_Code.rds
#  fips: A five-digit number (represented as a string) indicating the U.S. county
#  SCC: The name of the source as indicated by a digit string (see source code classification table)
#  Pollutant: A string indicating the pollutant
#  Emissions: Amount of PM2.5 emitted, in tons
#  type: The type of source (point, non-point, on-road, or non-road)
#  year: The year of emissions recorded

## read file from exdata-data-NEI_data folder and store in NEI 
NEI <- readRDS("exdata-data-NEI_data\\summarySCC_PM25.rds")

## Question 3: Of the four types of sources indicated by the type 
## (point, nonpoint, onroad, nonroad) variable, which of these four 
## sources have seen decreases in emissions from 1999-2008 for 
## Baltimore City? Which have seen increases in emissions from 1999-2008? 
## Use the ggplot2 plotting system to make a plot answer this question.

# Solution: 
# Make a plot containing 4 panels, one for each 'type'
library(ggplot2)
# Create subset of emission data for Baltimore
NEI.Baltimore <- subset(NEI, fips=="24510", select=c(Emissions, year, type))
# Sum emisisons by year and type 
em.by.year.type<-aggregate(Emissions~year+type, data=NEI.Baltimore, sum, na.rm=TRUE)

# open png device 
png(filename="plot3.png")

# create plot
g <- ggplot(data=em.by.year.type, aes(x=factor(year),y=Emissions))
g <- g + geom_point(size=4, col="blue")                                    
g <- g + facet_grid(.~type)                                                    # One panel for each type
g <- g + geom_smooth(method="lm",se=TRUE,aes(group=1),color="black")           # Add regression line to visualize trend
g <- g + ggtitle("Emissions by year and type in Baltimore City")
g <- g + xlab("Year")+ylab("Emisisons (tons)")
g <- g + coord_cartesian(ylim=c(0,1.1*max(em.by.year.type$Emissions)))         # make y axis start at 0
g

# close graphics device to save file
dev.off()

## Answer
#  The following types have seen increased emissions: POINT
#  The following types have seen decreased emissions: NON-ROAD, NONPOINT, ON-ROAD
