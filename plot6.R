## Code for constructing plot 6 for the second course project in the Exploratory Data Analysis Course


## Description of columns of Source_Classification_Code.rds
#  fips: A five-digit number (represented as a string) indicating the U.S. county
#  SCC: The name of the source as indicated by a digit string (see source code classification table)
#  Pollutant: A string indicating the pollutant
#  Emissions: Amount of PM2.5 emitted, in tons
#  type: The type of source (point, non-point, on-road, or non-road)
#  year: The year of emissions recorded

## read file from exdata-data-NEI_data folder and store in NEI 
NEI <- readRDS("exdata-data-NEI_data\\summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data\\Source_Classification_Code.rds")


## Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen 
## greater changes over time in motor vehicle emissions?
sectors <- unique(SCC$EI.Sector)
vehichle.sectors <- sectors[grepl("Mobile",sectors)]
SCC.vehichle <- subset(SCC, EI.Sector %in% vehichle.sectors, select = c(SCC, EI.Sector))

# Get emission data for vehichle sources from NEI by linking on column 'SCC'
# And limit to data from Baltimore City by setting fips=24510
NEI.vehichle.compare <- subset(NEI, SCC %in% SCC.vehichle$SCC & (fips=="24510" | fips=="06037"), select = c(Emissions, year, fips))

NEI.vehichle.compare$fips[NEI.vehichle.compare$fips=="24510"]="Baltimore"
NEI.vehichle.compare$fips[NEI.vehichle.compare$fips=="06037"]="Los Angeles"
# sum by year 
em.by.year<-aggregate(Emissions~year+fips, data=NEI.vehichle.compare, sum, na.rm=TRUE)

## How to answer question: Which city has seem greatest change?
## Absolute increase/decrease?
## Increase/decrease relative to starting point? 
## Most variation over time? 

## Show change as percentage of original value
base <- em.by.year[em.by.year$year=="1999",]

# normalize by dividing by initial value for both cities
Balt_norm = em.by.year[em.by.year$fips=="Baltimore",]$Emissions/base[base$fips=="Baltimore",]$Emissions
LA_norm = em.by.year[em.by.year$fips=="Los Angeles",]$Emissions/base[base$fips=="Los Angeles",]$Emissions

em.by.year$normalize=0;
em.by.year[em.by.year$fips=="Baltimore",]$normalize=Balt_norm 
em.by.year[em.by.year$fips=="Los Angeles",]$normalize=LA_norm

# open png device 
png(filename="plot6.png")

g <- ggplot(em.by.year , aes(x=factor(year),y=normalize))
g <- g + geom_point(size=4, color="blue")
g <- g + ggtitle("Relative change in emissions from vehichles over time") +  xlab("Year")
g <- g + facet_grid(.~fips)                               # One panel for each region
g <- g + geom_smooth(method="lm",se=TRUE,aes(group=1),lwd=1, color="black")# add regression line to visualize trend
g

# close graphics device to save file
dev.off()

# From this it looks like the relative difference is bigger in Baltimore 