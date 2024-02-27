## Author:  Kate Lofgren (klofgren@g.harvard.edu)
## Date:    8/13/2018
## Purpose: Universal parameters to pull across EHBP scripts

## parameter file to pull
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  file <- "./01_input_data/parameters_FINAL.xlsx"
  
## income parameters
  gini <- 0.391 ## source 2015 -- World Bank
  
  ave_hh_consumption <- 443.63
      ## source 2015/16 -- Ethiopian HH expenditure survey 
      ## Table B1.1 9,626.78 Birr per person total converted to 2016 USD 
  
  total.draws <- 1000000 ## number of household income's to generate
  
  ## define income cutoffs
  cutoffs <- c(0.1,0.25)
  
## coverage parameters
  cov.delta <- 0.05 
  
## budgets
  budgets <- seq(1000000,100000000,length.out =20)
  
## net monetary benefit parameters
  wtp <- c(716.88) ## 1x GDP per capita in 2016 based on WB data
  
  
