#' Projct: Property Price Imutation by Postal Code. Ontario onle
#' File:   Process Stan Model Results
#' 
#' Arielle Jean
#' 
#' Last Updated Sept 24, 2016
#' 
#' Description: Process the linear model computed in 2. Run Model, and compute
#'              values for mean and sensitivity by region. 

# ------------------------------------------------------------------------------
# Inputs - File Locations for reading and writing
# ------------------------------------------------------------------------------
working_directory <- ""

years             <- 2010:2017 #Years of interest for modelling. Data to 2015

#Model and data for processing
stan_model        <- "Shape Files/Stan.rds"
model_data        <- "Shape Files/Model Data.csv"
transformed_data  <- "Shape Files/transformed_data.csv"

#Ouputs
res_long          <- "Shape Files/Res Long.rds"
res_mean          <- "Shape Files/Res.rds"

sens_table        <- "Shape Files/Res 20 80.txt"
sens_rds          <- "Shape Files/Res 20 80.rds"

predictions_file  <- "Shape Files/predictions.rds"

# ------------------------------------------------------------------------------
# Set up required packages packages and set wd
# ------------------------------------------------------------------------------

setwd(working_directory)

library(RColorBrewer)
library(maptools)
library(ggmap)
library(rgeos)
library(dplyr)
library(rstan)

source("src/Common.R")

# ------------------------------------------------------------------------------
# Load Model Data 
# ------------------------------------------------------------------------------

s         <- readRDS(stan_model)
df        <- read.csv(transformed_data)
modelling <- read.csv(model_data )

# ------------------------------------------------------------------------------
# Create Functions for Data Processing 
# ------------------------------------------------------------------------------

raise6 <- function (a) {
  dim.a <- dim(a); N <- length(dim.a)
  apply(a, 1:(N-1), function (v) c(v, rep(0, 6-dim.a[N]))) %>%
    aperm(c(2:N, 1))
} 

beta.prm.mean <- function (v) apply(extract(s, v)[[1]], c(2, 3), mean) %>% raise6
beta.prm      <- function (v) extract(s, v)[[1]] %>% raise6

beta.names    <- c("lprice", 
                   "trend", 
                   "quad", 
                   "k.lprice", 
                   "k.trend", 
                   "k.quad")

par.tbl <- function(d, v.name, b.name, name.postfix) 
  data.frame(levels(d[[v.name]]), beta.prm.mean(b.name)) %>% 
  setNames(c(v.name, 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df()

par.tbl.long <- function(d, v.name, b.name, name.postfix) {
  samples <- beta.prm(b.name)
  data.frame(expand.grid(1:dim(samples)[[1]], levels(d[[v.name]])), 
             array(samples, c(dim(samples)[[1]]*dim(samples)[[2]], dim(samples)[[3]]))) %>% 
    setNames(c("sample", v.name, 
               paste(beta.names, name.postfix, sep=""))) %>%
    tbl_df() }

mean.tbl.long <- function (name.postfix="4") 
  extract(s, "mean_beta")[[1]] %>% { 
    data.frame(sample=1:dim(.)[[1]], .) } %>% 
  setNames(c("sample", 
             paste(beta.names, name.postfix, sep=""))) %>%
  tbl_df() 

n.samples <- length(extract(s, "lp__")[[1]])

# ------------------------------------------------------------------------------
# Make Tables 
# ------------------------------------------------------------------------------

#Get parameter by region
res.long <- df%>%
  select(ZIP, 
         Year, FSA, City, Density, Home_Price_t) %>%
  merge(data.frame(sample=1:n.samples)) %>% 
  filter(is.finite(Density)) %>%
  left_join(par.tbl.long(mod, "ZIP",  "beta",  ""),  by=c("ZIP", "sample")) %>%
  left_join(par.tbl.long(mod, "FSA",  "beta1", "1"), by=c("FSA", "sample")) %>% 
  left_join(par.tbl.long(mod, "City", "beta2", "2"), by=c("City","sample")) %>% 
  left_join(mean.tbl.long(                     "4"), by=c(       "sample")) %>% 
  
  mutate(ZIP    = ZIP, 
         Density= Density,
         lprice = sum.0na(lprice, lprice1, lprice2, lprice4) + 
                    sum.0na(k.lprice, k.lprice1, k.lprice2,  k.lprice4) * Density, 
         trend  = sum.0na(trend, trend1, trend2,  trend4) +
                    sum.0na(k.trend, k.trend1, k.trend2,  k.trend4) * Density, 
         quad   = sum.0na(quad, quad1, quad2,  quad4) +
                    sum.0na(k.quad, k.quad1, k.quad2,  k.quad4) * Density) %>%
  
  mutate(price       = exp(kScale + lprice), 
         trend       = trend, 
         quad_trend  = 2*quad, 
         price2017   = exp(kScale + 
                             lprice + 
                             trend * year2yr(2017) + 
                             quad * year2yr(2017)**2),
         trend2017   = (trend + 2*quad*year2yr(2017))) %>%
  tbl_df()

saveRDS(res.long, res_long)

#Group by Density and E(X)
res <- res.long %>% 
        group_by(ZIP, 
                 Density) %>% 
        summarise(lprice    = mean(Home_Price_t), 
                  price2017 = mean(price2017), 
                  trend2017 = mean(trend2017), 
                  quad_trend= mean(quad_trend)) %>%
        ungroup()

saveRDS(res, res_mean)

#Sensitivivity Analysis
res2080 <- res.long %>% 
            group_by(ZIP, 
                     Density) %>% 
            summarise(lprice        = mean(Home_Price_t), 
                      price2017.20  = quantile(price2017,  .2), 
                      trend2017.20  = quantile(trend2017,  .2), 
                      quad_trend.20 = quantile(quad_trend, .2), 
                      price2017.80  = quantile(price2017,  .8), 
                      trend2017.80  = quantile(trend2017,  .8), 
                      quad_trend.80 = quantile(quad_trend, .8)) %>%
            ungroup()

write.table(res2080,  sens_table, row.names=F, quote=F)
saveRDS(res2080, sens_rds)

#Compute Predictions - This takes an hour
predictions <- expand.grid(sample= unique(res.long$sample), 
                            Year = years, 
                            ZIP  = unique(res.long$ZIP)) %>% 
                tbl_df %>%
                left_join(res.long %>% 
                            select(ZIP, 
                                   sample, 
                                   Home_Price_t, 
                                   trend, 
                                   quad), 
                          by=c("sample", "ZIP")) %>%
                mutate(price = exp(kScale + 
                                     Home_Price_t + 
                                     trend * year2yr(Year) + 
                                     quad * year2yr(Year)**2)) %>%
                group_by(ZIP, 
                         Year) %>% 
                do(data.frame(price   = mean(.$price),  
                              price10 = quantile(.$price, .1), 
                              price25 = quantile(.$price, .25), 
                              price50 = quantile(.$price, .5), 
                              price75 = quantile(.$price, .75), 
                              price90 = quantile(.$price, .9))) %>%
                ungroup() %>%
                left_join(df %>% 
                            select(ZIP, 
                                   Year, 
                                   obs_price = Home_Price, 
                                   n_sold    = n), 
                          by=c("Year", "ZIP"))

saveRDS(predictions, predictions_file)

#Compare to Actual
# Compare predictions to those from year 2015
predictions %>%
  select(ZIP, 
         Year, 
         obs_price) %>%
  left_join(predictions %>%
              select(ZIP, 
                     Year, 
                     price),
            by = c("ZIP", "Year")) %>%

    ggplot(aes(x=pmin(price, 100000000), 
               y=obs_price)) + 
      geom_point(aes(colour=factor(Year))) +
      ggtitle("Predictions vs Observed") + 
      xlab("Predicted") +
      ylab("Observed")