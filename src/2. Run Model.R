#' Projct: Property Price Imutation by Postal Code. Ontario onle
#' File:   Run Stan model for house price imputation.
#' 
#' Arielle Jean
#' 
#' Last Updated Sept 24, 2016
#' 
#' Description: Take descriptive data file generated in the 1. Aggregate Data.R
#'              file, and run a Stan model to recover pricing mechanism. I had
#'              to scale back the data used because of system constraints. 
#'              
#' Note: assumes rstudioapi is installed install.packages("rstudioapi) if run in
#'        parallel

# ------------------------------------------------------------------------------
# Inputs - File Locations for reading and writing
# ------------------------------------------------------------------------------
working_directory <- ""

#Combined File to be output
data_in           <- "Shape Files/transformed_data.csv"

#Model data to be run through Stan
model_data        <- "Shape Files/Model Data.csv"

#Stan file 
stan              <- "src/Model.stan"

#Final Model
stan_out          <- "Shape Files/Stan.rds"
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
library(parallel)

# ------------------------------------------------------------------------------
# Load Data, and modify
# ------------------------------------------------------------------------------

df <- read.csv(data_in)

#Down sample a lot to simulate missing. Also, working from laptop. 
#Data is near random, so method isn't super important. 
set.seed(12345)
modified <- split(df, df$Level_3)
modified <- lapply(modified, function(x){
                                z = unique(x$Level_2)
                                z = sample(z, floor(length(z) / 5))
                                x = x[x$Level_2 %in% z, ]
                                
                                z = unique(x$Level_1)
                                z = sample(z, floor(length(z) / 2))
                                x = x[x$Level_1 %in% z, ]
                                return(x)
                              })

modified         <- do.call("rbind", modified)
modified$Level_1 <- as.numeric(as.factor(modified$Level_1))
modified$Level_2 <- as.numeric(as.factor(modified$Level_2))

write.csv(modified, model_data, row.names=F)
#mod = read.csv(model_data)

m  <- stan_model(file=stan)

s.f <- function (nchains=2, iter=10, warmup=5, thin=1, refresh=1){
  # library(doParallel)
  # registerDoParallel(nchains)
  sampling(m, data=with(modified, list(N     = nrow(modified), 
                                       M     = length(unique(Level_1)), 
                                       M1    = length(unique(Level_2)), 
                                       M2    = length(unique(Level_3)), 
                                       lprice= Home_Price_t, 
                                       count = n, 
                                       yr    = Year_t, 
                                       z     = Density,
                                       zip   = Level_1, 
                                       L_1   = Level_2,
                                       L_2   = Level_3)), 
           iter=iter, warmup=warmup, thin=thin, init=0, chains=nchains, cores= nchains, refresh=refresh, seed=4)
}

options(mc.cores = parallel::detectCores())

#Some tests before running the real model
s <- s.f(nchains=1, iter=10, warmup=0, thin=1, refresh=0) #Can we run on 1 chain?

s <- s.f(nchains=4, iter=10, warmup=0, thin=1, refresh=0) #Can we run on 4 chains?

#The model used. Needs to be much bigger if applicable. 
rm(s)
s <- s.f(nchains=4, iter=100, warmup=25, thin=3, refresh=1)
saveRDS(s, stan_out)
