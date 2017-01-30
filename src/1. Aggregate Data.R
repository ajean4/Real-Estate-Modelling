#' Projct: Property Price Imutation by Postal Code. Ontario only
#' File:   Create Data for Analysis
#' 
#' Arielle Jean
#' 
#' Last Updated Sept 24, 2016
#' 
#' Description: Load disparate data sources, and combine in a single file
#'              for analysis. This file combines postal code data, population
#'              data, and housing price data (removed and replaced with 
#'              synthetic information due to data sensitivity)

# ------------------------------------------------------------------------------
# Inputs - File Locations for reading and writing
# ------------------------------------------------------------------------------
working_directory <- ""

#Canada shape file by postal code
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
shpfile_in        <- "data/pshape/CanadaPostalCodePolygons.shp"
shpfile_out       <- "Shape Files/Ontario.rdata"
#
#Canada population and dwelling counts
# https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/hlt-fst/pd-pl/Table-Tableau.cfm?LANG=Eng&T=1201&S=22&O=Ahousing_in        <- "data/Population and Dwelling Count.csv"
housing_out       <- "Shape Files/Ontario.rdata"

#Combined File to be output
final_out         <- "Shape Files/transformed_data.csv"

# ------------------------------------------------------------------------------
# Set up required packages packages and set wd
# ------------------------------------------------------------------------------

setwd(working_directory)
library(RColorBrewer)
library(maptools)
library(ggmap)
library(rgeos)
library(dplyr)

source("src/Common.R")

# ------------------------------------------------------------------------------
# Load postal codes, and filter for Ontario only because this is a laptop.
# ------------------------------------------------------------------------------
#If shpfile_out exists, go to line 88
sh           <- readShapePoly(shpfile_in)

sh@data      <- sh@data %>%
                  mutate(ZIP = as.character(ZIP))    %>%
                  mutate(FSA    = substr(ZIP, 1, 3)) %>%
                  mutate(City   = substr(ZIP, 1, 1)) 
sh           <- subset(sh, City %in% c("K", "L", "M", "N", "P"))
plot(sh, main="Original Shape File")

save(sh, file=shpfile_out)

# ------------------------------------------------------------------------------
# Get dwelling counts at the FSA level because that is the granularity available
# ------------------------------------------------------------------------------

##Aggregate by FSA
FSA.Count     <- aggregate(sh@data$FSA, list(sh@data$FSA), length) 
colnames(FSA.Count) = c("Geographic.name", "Count")

##Import Housing Data
Housing       <- read.csv(housing_in)
FSA.Count     <- merge(FSA.Count, Housing, by="Geographic.name", all.x=T)
rm(Housing)

FSA.Count     <- FSA.Count %>%
                  select(Geographic.name, 
                         Count, 
                         Population..2011, 
                         Total.private.dwellings..2011) %>%
  
                  mutate(Normalized = Population..2011 / Count, 
                         Dwellings = Total.private.dwellings..2011 / Count) %>%
  
                  select(Geographic.name, 
                         Normalized, 
                         Dwellings)

colnames(FSA.Count) = c("FSA", "Average_Population", "Average_Dwellings") 

#Merge Normalized Population
sh@data       <- merge(sh@data, FSA.Count, by="FSA")

#load(shpfile_out)

# ------------------------------------------------------------------------------
# Extrapolate population at FSA level to postal code level. 
# ------------------------------------------------------------------------------

#' I assumed a uniform distribution by number of postal codes per FSA, and 
#' allocated uniformly across these. There is room to improve, but this was a 
#' quick workaround in the abscence of more data. Additionally, area was 
#' calculated based on the area of the polygon, which results in some weird 
#' values. This area has the most room for improvement in the preprocessing
#' stage.

#Average population
density_area    <- sapply(slot(sh, "polygons"), slot, "area") * 100000
sh@data$Density <- sh@data$Average_Population / density_area

#Imputing density with median population density. Can be improved.  
sh@data$Density[is.infinite(sh@data$Density)] <- NA
sh@data$Density[is.na(sh@data$Density)]       <- median(sh@data$Density, na.rm=T)
sh@data$Density                               <- log(sh@data$Density + 1)
sh@data$Density                               <- sh@data$Density / 
                                                  mean(sh@data$Density, na.rm=T)
save(sh, file=housing_out)

# ------------------------------------------------------------------------------
# Simulate known prices per postal code. This is the 
#   data file.
# ------------------------------------------------------------------------------
set.seed(12345)
house_prices <- sh@data
house_prices <- expand.grid(ZIP=house_prices$ZIP, Year=2010:2015)
house_prices <- merge(house_prices, sh@data, by="ZIP", all.x=T)

df <- house_prices %>%
        mutate(ZIP     = as.factor(ZIP))  %>%
        mutate(FSA     = as.factor(FSA))  %>%
        mutate(City    = as.factor(City)) %>%
        mutate(Density = -log(Density))   %>%
        mutate(Year_t  = year2yr(Year))

df$Density[is.infinite(df$Density)] <- median(df$Density)

#Simulate the average house price per postal code
df$Home_Price <-   600000 +
                    100 * df$Year_t + 
                    100 * df$Year_t^2 + 
                    1000 * df$Density +
                    1000 * df$Density * df$Year_t + 
                    5000 * df$Density * df$Year_t * df$Year_t +
                    rnorm(nrow(df), 0, 100000)

#Simulate the number of houses sold per postal code
df$Home_Price_t <- log(df$Home_Price) - kScale
df$n            <- sample(10:1000, nrow(df), replace=T)

#Aggregate by level, and save for later.
df$Level_1      <- as.numeric(as.factor(df$ZIP))
df$Level_2      <- as.numeric(as.factor(df$FSA))
df$Level_3      <- as.numeric(as.factor(df$City))
write.csv(df, final_out, row.names=F)
