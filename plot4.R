## Code for constructing plot 4 for the second course project in the Exploratory Data Analysis Course


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

## Question 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# Solution:
# Step 1: Identify coal-combustion-related sources
# From looking at the data it seems like the type of source is stored in SCC$EI.Sector
# Find sources including the words "comb" (for combusiton) and "coal"
sectors <- unique(SCC$EI.Sector)
coal.sectors <- sectors[grepl("Coal",sectors) & grepl("Comb",sectors)]
SCC.coal <- subset(SCC, EI.Sector %in% coal.sectors, select = c(SCC, EI.Sector))

# Get emission data for combustion coal sources from NEI by linking on column 'SCC'
NEI.coal <- subset(NEI, SCC %in% SCC.coal$SCC, select = c(Emissions, year))
# sum by year 
em.by.year<-aggregate(Emissions~year, data=NEI.coal, sum, na.rm=TRUE)
em.by.year$Emissions = 0.001*em.by.year$Emissions

# open png device 
png(filename="plot4.png")

# Construct plot using ggplot
g <- ggplot(em.by.year, aes(x=factor(year),y=Emissions))
g <- g + geom_point(size=4, col="blue")
g <- g + coord_cartesian(ylim=c(0,1.5*max(em.by.year$Emissions)))          # start y axis at 0
g <- g + geom_smooth(method="lm",se=TRUE,aes(group=1),lwd=1, color="black")# add regression line to visualize trend
g <- g + ggtitle("Emissions from coal combustion-related sources in the US by year
                  Including regression line")
g <- g + xlab("Year")+ylab("Emisisons (kilotons)")
g

# close graphics device to save file
dev.off()

# Answer: Regression line suggests that emissions are decreasing though the standard error is large 




