# Center & scale for modeling. Set constants in single location
year2yr <- function (year) (year-2010)
kScale  <- 11

# Prefixes for postal codes
prefix.factor <- function (zip, n) as.factor(substr(zip, 1, n))
l3 <- { . %>% prefix.factor(., 1) }
l2 <- { . %>% prefix.factor(., 2) }
l1 <- { . %>% prefix.factor(., 3) }

first.nna2 <- function (c1, c2)  ifelse(!is.na(c1), c1, c2)
first.nna <- function (...) Reduce(first.nna2, list(...))

sum.0na2 <- function (c1, c2)  ifelse(is.na(c1), 0, c1) + ifelse(is.na(c2), 0, c2)
sum.0na <- function (...) Reduce(sum.0na2, list(...))