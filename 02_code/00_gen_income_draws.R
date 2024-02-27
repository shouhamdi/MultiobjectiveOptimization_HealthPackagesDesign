## Author:  Kate Lofgren (klofgren@g.harvard.edu)
## Date:    6/16/2023
## Purpose: Generate HH income draws

## bring in parameters
  rm(list=ls())
  set.seed(02139)
  date <- Sys.Date()
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  source("./02_code/00_universal_parameters.R")

## generate hh income draws
  fgamma <- function(phi){gini-(1/(phi*4^phi))*1/beta(phi,phi + 1)} #income distribution function
  phi <- uniroot(fgamma,lower=0.000001,upper=100)$root
  beta <- (1/phi)*(ave_hh_consumption)

  gen.draws <- function(y){rgamma(y,shape=phi,scale=beta)}
  draws <- gen.draws(total.draws)
  
 hist(draws, breaks =1000)
  
  
## save file with income draws
  write.table(draws,"./03_output_data/income_draws.csv",sep=",",row.names=F)
