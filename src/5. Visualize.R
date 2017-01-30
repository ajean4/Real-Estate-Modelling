#' Projct: Property Price Imutation by Postal Code. Ontario onle
#' File:   Visualize Model Results
#' 
#' Arielle Jean
#' 
#' Last Updated Sept 24, 2016
#' 
#' Description: Visualize Model resuls

# ------------------------------------------------------------------------------
# Inputs - File Locations for reading and writing
# ------------------------------------------------------------------------------

working_directory <- ""

#Data for processing
transformed_data  <- "Shape Files/transformed_data.csv"
res_long          <- "Shape Files/Res Long.rds"
ontario           <- "Shape Files/Ontario.RData"

#Output
combined_image    <- "Shape Files/model_vs_raw_plotdata_temp.rds"
mod_v_raw_gif     <- "Plots/model_vs_raw_log_temp.gif"
toronto_gif       <- "Plots/Toronto.gif"
# ------------------------------------------------------------------------------
# Set up required packages packages and set wd
# ------------------------------------------------------------------------------

setwd(working_directory)

library("dplyr")
library("tidyr")
library("rgdal")
library("maptools")
library("ggplot2")
library("viridis")

# devtools::install_github("dgrtwo/gganimate")
library("gganimate")

source("source_2016/common.R")

# ------------------------------------------------------------------------------
# Load Model Data and Set Theme
# ------------------------------------------------------------------------------

# Create almost empty theme
new_theme_empty            <- theme_bw()
new_theme_empty$line       <- element_blank()
new_theme_empty$rect       <- element_blank()
new_theme_empty$axis.text  <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin<- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
theme_set(new_theme_empty)

##Data
raw_df   <- read.csv(transformed_data)
res.long <- readRDS(res_long)
load(ontario)

# ------------------------------------------------------------------------------
# Mutate data for plots
# ------------------------------------------------------------------------------

pred_df <- lapply(2005:2015, function(year)
                              res.long %>%
                                mutate(price_pred = exp(kScale + 
                                                          lprice + 
                                                          trend * year2yr(year) + 
                                                          quad * year2yr(year)**2),
                                       trend_pred = (trend + 2 * quad * year2yr(year))/10) %>%
                                group_by(ZIP, 
                                         Density) %>% 
                                summarise(price_pred_mean = mean(price_pred),
                                          trend_pred_mean = mean(trend_pred)) %>%
                                mutate(Year = year)) %>% 
            bind_rows()

# Transform to dataframe wiht ggplot2::fortify
shape <- ggplot2::fortify(sh, region="ZIP")

# Combine everything together
combined_df <- raw_df %>%
                select(ZIP, 
                       Year, 
                       Home_Price) %>%
                mutate(source = "Raw") %>%
                bind_rows(pred_df %>%
                            select(ZIP, 
                                   Year, 
                                   price = price_pred_mean) %>%
                            mutate(source = "Model")) %>%
                mutate(source = factor(source, levels=c("Raw", 
                                                        "Model"))) %>%
                left_join(shape %>% 
                            rename(ZIP = id))

saveRDS(combined_df, file=combined_image)

# ------------------------------------------------------------------------------
# Generate Plots
# ------------------------------------------------------------------------------

##2015 Plot
combined_df %>%
  filter(Year == 2015) %>%
  
  ggplot(aes(x=long, y=lat)) + 
    geom_polygon(aes(group=group, fill=Home_Price), colour=NA) +
    scale_fill_viridis() +
    theme(legend.position="bottom") +
    facet_wrap(~ source, nrow=1) +
    ggtitle("Shape File and Prices")

p <- combined_df %>%
  
      ggplot(aes(x=long, y=lat, frame=Year)) + 
        geom_polygon(aes(group=group, 
                         fill=Home_Price), 
                     color=NA) +
        scale_fill_viridis(trans = "log") +
        theme(legend.position="none") +
        facet_wrap(~ source, nrow=1) +
        ggtitle("Mean apartment prices:") +
        theme(plot.title = element_text(size=20),
              strip.text.x = element_text(size=14))

#Note make sure Imagemagick is installed
gg_animate(p = p, filename=mod_v_raw_gif)

# Another one on Toronto
p2 <- combined_df %>%
        filter(substr(ZIP, 1, 1) == "M") %>%
  
        ggplot(aes(x=long, y=lat, frame=Year)) + 
          geom_polygon(aes(group=group, 
                           fill=price), 
                       color=NA) +
          scale_fill_viridis() +
          theme(legend.position="none") +
          facet_wrap(~ source, nrow=1) +
          ggtitle("Mean apartment prices:") +
          theme(plot.title = element_text(size=20),
                strip.text.x = element_text(size=14))

gg_animate(p = p2, filename=toronto_gif)
