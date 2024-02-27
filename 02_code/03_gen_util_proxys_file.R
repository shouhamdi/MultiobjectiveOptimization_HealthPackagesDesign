## Author:  Sophia Houhamdi
## Date:    06/22/2023
## Purpose: Prep the utilization per quintile file

## set up
rm(list = ls())
date <- Sys.Date()

## libraries
library(foreign)
library(gdata)

## set seed
set.seed(02139)

## set directory
setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
source("./02_code/00_universal_parameters.R")

## Interventions linked with proxies 
int_proxy <- read.xls("./01_input_data/00_raw_files/interventions_proxys.xlsx", stringsAsFactors = FALSE)
names(int_proxy) <- c("int_n", "intervention", "intervention_name", "intervention_short", "Description_of_utilization")

## Sarah's paper data
util <- read.csv("./01_input_data/00_raw_files/utilization_raw.csv", header = TRUE, stringsAsFactors = FALSE)

util_subset <- subset(util, country == "Ethiopia", select = c("intervention_code", "intervention_name", "util_code", "util_name", "util", "group"))
util_subset$quintile <- util_subset$group
util_subset$group <- NULL

#print(colnames(int_proxy))
#print(colnames(util_subset))

data <- merge(int_proxy, util_subset, by = "intervention_name", all.x = TRUE)
#print(colnames(data))
data <- subset(data, select = c("int_n", "intervention", "Description_of_utilization", "util_code", "util_name", "util", "quintile"))

## order
data <- data[, c("int_n", "Description_of_utilization", "util_code", "util_name", "util", "quintile")]


## Replace 'Q' with an empty string and convert to numeric
data$quintile <- as.numeric(gsub("Q", "", data$quintile))


## Duplicate rows where "util" is NA and assign quintile values and names
for(r in 1:dim(data)[1]) {
  if (is.na(data$util[r])){
    for (q in 1:5){
      duplicated_row <- do.call(rbind, replicate(1, data[r,], simplify = FALSE))
      duplicated_row$quintile <-q
      duplicated_row$util <-100
      print(duplicated_row)
      data <- rbind(data, duplicated_row)
    }
  }
}

## Create the quintile values vector
quintile_values <- c(1, 2, 3, 4, 5, "Total")

## Assign the quintile values to the duplicated rows
data$quintile[is.na(data$util)] <- quintile_values

## Assign value 100 when "util" is NA
data$util[is.na(data$util)] <- 100

## Remove rows where "quintile" is NA
data <- data[complete.cases(data$quintile), ]

## Order the rows by "int_n" and "quintile"
data <- data[order(data$int_n, data$quintile), ]

## write out master file
write.csv(data, "./01_input_data/03_util_proxys.csv", row.names = FALSE)
