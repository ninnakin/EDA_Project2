## Code for constructing plot 5 for the second course project in the Exploratory Data Analysis Course


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

## Question 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# Step 1: Identify motor vehicle sources
# From looking at the data it seems like the type of source is stored in SCC$EI.Sector
# Sectors starting with 'mobile' seem to include motor vehichles 
sectors <- unique(SCC$EI.Sector)
vehichle.sectors <- sectors[grepl("Mobile",sectors)]
SCC.vehichle <- subset(SCC, EI.Sector %in% vehichle.sectors, select = c(SCC, EI.Sector))

# Get emission data for vehichle sources from NEI by linking on column 'SCC'
# And limit to data from Baltimore City by setting fips=24510
NEI.vehichle.Balt <- subset(NEI, SCC %in% SCC.vehichle$SCC & fips=="24510", select = c(Emissions, year))
# sum by year 
em.by.year<-aggregate(Emissions~year, data=NEI.vehichle.Balt, sum, na.rm=TRUE)

# open png device 
png(filename="plot5.png")

# Construct plot using ggplot
g <- ggplot(em.by.year, aes(x=factor(year),y=Emissions))
g <- g + geom_point(size=4, col="blue")
g <- g + coord_cartesian(ylim=c(0,1.8*max(em.by.year$Emissions)))     
g <- g + geom_smooth(method="lm",se=TRUE,aes(group=1),lwd=1, color="black")# add regression line to visualize trend
g <- g + ggtitle("Emissions from motor vevichle-related sources in Baltimore by year
                  Including regression line")
g <- g + xlab("Year")+ylab("Emisisons (tons)")
g

# close graphics device to save file
dev.off()