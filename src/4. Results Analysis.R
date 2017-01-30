#' Projct: Property Price Imutation by Postal Code. Ontario onle
#' File:   Analyze Stan Model Results
#' 
#' Arielle Jean
#' 
#' Last Updated Sept 24, 2016
#' 
#' Description: Analyze results from the model, and ensure they are reasonable. 

# ------------------------------------------------------------------------------
# Inputs - File Locations for reading and writing
# ------------------------------------------------------------------------------

working_directory <- ""

#Data for processing
transformed_data  <- "Shape Files/transformed_data.csv"
res_long          <- "Shape Files/Res Long.rds"
sens_rds          <- "Shape Files/Res 20 80.rds"
predictions_file  <- "Shape Files/predictions.rds"
ontario           <- "Shape Files/Ontario.RData"

#Output
fsa_aggregate     <- "Shape Files/FSA Data.csv"


# ------------------------------------------------------------------------------
# Set up required packages packages and set wd
# ------------------------------------------------------------------------------

setwd(working_directory)

library("sorvi")
library("dplyr")
library("ggplot2")

# ------------------------------------------------------------------------------
# Load Model Data 
# ------------------------------------------------------------------------------

load(ontario)
data        <- read.csv(transformed_data)
predictions <- readRDS(predictions_file)
res_long    <- readRDS(res_long)
quantiles   <- readRDS(sens_rds)

# ------------------------------------------------------------------------------
# Load Model Data 
# ------------------------------------------------------------------------------

aggregate.data <- quantiles %>%
                    inner_join(predictions) %>%
                    inner_join(res_long,
                               by=c("ZIP", "Year")) %>%
                    select(ZIP, 
                           FSA, 
                           City, 
                           Density.x, 
                           price2017, 
                           price2017.20,
                           price2017.80,
                           trend2017, 
                           trend2017.20,
                           trend2017.80) %>%
                    mutate(price2017mean  = round(price2017, d=-1),
                           price2017min   = round(price2017.20, d=-1),
                           price2017max   = round(price2017.80, d=-1),
                           trend2017mean  = round(100 * trend2017, d=1),
                           trend2017min   = round(100 * trend2017.20, d=1),
                           trend2017max   = round(100 * trend2017.80, d=1)) %>%
                    select(ZIP, 
                           FSA, 
                           City, 
                           Density.x, 
                           price2017mean, 
                           price2017min,
                           price2017max, 
                           trend2017mean, 
                           trend2017min, 
                           trend2017max)

aggregate.data %>%
  arrange(FSA, 
          desc(price2017min)) %>%
  mutate(ZIP = as.character(ZIP)) %>%
  write.csv(fsa_aggregate, quote=T, row.names=F, fileEncoding="ISO-8859-1")

## COMPARISON ANALYSIS ##########
##I didn't do 2015 vs 2016

theme_set(theme_bw(24))

# Density
ggplot(aggregate.data, aes(x=Density.x, y=trend2017mean)) +
  geom_point() + 
  geom_smooth(method="lm") +
  ggtitle("Trend as a Function of Density") +
  xlab("Density") +
  ylab("Trend")

m2017 <- lm(trendi2017keski ~ Density.x,
            data=aggregate.data)
summary(m2017)
#Year has a significant trend

