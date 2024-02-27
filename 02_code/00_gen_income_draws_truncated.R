## Author:  Sophia Houhamdi
## Date:    6/16/2023
## Purpose: Generate HH income draws and remove first and last percentile

## bring in parameters
rm(list=ls())
set.seed(02139)
date <- Sys.Date()
setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
source("./02_code/00_universal_parameters.R")

## generate hh income draws
fgamma <- function(phi) { gini - (1 / (phi * 4^phi)) * 1 / beta(phi, phi + 1) } # income distribution function
phi <- uniroot(fgamma, lower = 0.000001, upper = 100)$root
beta <- (1 / phi) * (ave_hh_consumption)

gen.draws <- function(y) { rgamma(y, shape = phi, scale = beta) }
draws <- gen.draws(total.draws)

## Remove first and last percentiles
sorted_draws <- sort(draws)
lower_percentile <- round(total.draws * 0.02)  # First percentile index
upper_percentile <- round(total.draws * 0.98)  # Last percentile index

print(sorted_draws[lower_percentile])
print(sorted_draws[upper_percentile])

modified_draws <- sorted_draws[(lower_percentile + 1):(upper_percentile - 1)]

hist(modified_draws, breaks =1000)


## save file with income draws
write.table(modified_draws, "./03_output_data/income_draws_truncated.csv", sep = ",", row.names = FALSE)
