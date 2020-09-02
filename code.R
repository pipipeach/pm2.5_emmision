library(ggplot2)
library(RColorBrewer)
library(dplyr)
# Download and unzip the file:
dir.create("./air_pollution")
urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(urlzip, destfile = "./air_pollution.zip" )
unzip("./air_pollution.zip", exdir = "./air_pollution" )

# Load the data:
NEI <- readRDS("./air_pollution/summarySCC_PM25.rds")
SCC <- readRDS("./air_pollution/Source_Classification_Code.rds")

# Check NEI data
str(NEI)
# Check SCC data
str(SCC)

# I
years <- group_by(NEI,year)
total_emissions_per_year <- summarize (years, total_emission = sum(Emissions))
plot(total_emissions_per_year$year, total_emissions_per_year$total_emission,
     type = "o",
     main="Total emissions per year", xlab="Year", 
     ylab="Total emissions")

# II
Baltimore <- subset(NEI, fips == "24510")
Baltimore_years <- group_by(Baltimore,year)
Baltimore_total_emissions_per_year <- summarize (Baltimore_years, total_emission = sum(Emissions))
plot(Baltimore_total_emissions_per_year$year, 
     Baltimore_total_emissions_per_year$total_emission,
     type = "o",
     main="Total emissions per year in Baltimore", xlab="Year", 
     ylab="Total emissions")

# III
types <- group_by(NEI, year, type)
total_emission_year_type <- summarize (types, total_emission = sum(Emissions))
g <- ggplot(total_emission_year_type, aes(year, total_emission))
g + geom_point() + geom_line() + facet_grid(. ~ type) + 
        labs(title = "total emission by type each year") + 
        labs(x = "Year", y = "Total emissions")

# IV
SCC_coal <- SCC[grepl("coal",SCC$Short.Name, ignore.case = TRUE),]
NEI_coal <- subset(NEI, NEI$SCC %in% SCC_coal$SCC)
NEI_coal_years <- group_by(NEI_coal, year,type)
coal_total_emissions_per_year <- summarize (NEI_coal_years, total_emission = sum(Emissions))
g <- ggplot(coal_total_emissions_per_year, aes(year, total_emission, col = type))
g + geom_point() + geom_line() + labs(title = "total emission by coal") + 
        labs(x = "Year", y = "Total emissions")

# V 
Baltimore_Motor <- subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")
Baltimore_Motor_years <- group_by(Baltimore_Motor,year)
Baltimore_Motor_total_emissions_per_year <- summarize (Baltimore_Motor_years, total_emission = sum(Emissions))
g <- ggplot(Baltimore_Motor_total_emissions_per_year , aes(year, total_emission))
g + geom_point() + geom_line() + labs(title = "total emission by motor") + 
        labs(x = "Year", y = "Total emissions")

# VI
Bal_LA <- subset(NEI, (fips == "24510" | fips == "06037") & NEI$type == "ON-ROAD")
Bal_LA_fips_years <- group_by(Bal_LA, fips, year)
Bal_LA_total_emissions_per_year <- summarize (Bal_LA_fips_years, total_emission = sum(Emissions))
g <- ggplot(Bal_LA_total_emissions_per_year, aes(year, total_emission))
g + geom_point() + geom_line() + facet_grid(. ~ fips) + 
        labs(title = "total emission by fips each year") + 
        labs(x = "Year", y = "Total emissions")














