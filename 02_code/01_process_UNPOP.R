## Author:  Kate Lofgren (klofgren@g.harvard.edu)
## Date:    1/30/2019
## Purpose: Prep UN population figures that we need

## set up
  rm(list=ls())
  date <- Sys.Date()

## libraries
  library(foreign)
  library(reshape2)

## set seed
  set.seed(02139)

## set directory
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")

## holding dataset for all values
  master <- data.frame(age=NA,sex=NA,population=NA)
  
#################################
## Births #######################
#################################
  data <- read.table( 
    text = readLines("./01_input_data/00_raw_files/UNPOP_births_abbreviated.csv", warn = FALSE), 
    header = TRUE,  
    sep = ",",
    stringsAsFactors=F)
  data <- as.data.frame(data)
  data <- data[,c("X2010.2015")]
  data <- as.character(data)
  data <- gsub(" ","",data)
  data <- as.numeric(data)
  births <- data*1000 # UNPOP reported in 1000's
  births <- births/5 # UNPOP reportes in 5 year intervals
  data <- NULL
  master[1,1] <- "births"
  master[1,2] <- "both"
  master[1,3] <- births
  
##################################
## Pop estimates  ################
##################################
  ## bring in the file
  data <- read.table("./01_input_data/00_raw_files/UNPOP_population_formatted.csv",sep=",",header=T,stringsAsFactors=F)
  ## based on an average of 2015 and 2020 estimates
  
  # first multiply by 1000s -- reported in 1000s
  data$both <- data$both*1000
  data$female <- data$female*1000
  data$male <- data$male*1000
  
  ## under-5 all
  all.under5 <- data$both[data$start.age==0]
  master[2,1] <- "under-5"
  master[2,2] <- "both"
  master[2,3] <- all.under5
  
  ## 10 year old all
  all.10 <- data$both[data$start.age==10]/5 # reported in 5 year age groups
  master[3,1] <- 10
  master[3,2] <- "both"
  master[3,3] <- all.10
  
  ## 12 year old females
  female.12 <- data$female[data$start.age==10]/5
  master[4,1] <- 12
  master[4,2] <- "female"
  master[4,3] <- female.12
  
  ## 15 year old males
  male.15 <- data$male[data$start.age==15]/5
  master[5,1] <- 15
  master[5,2] <- "male"
  master[5,3] <- male.15
  
  ## 30 year old females
  female.30 <- data$female[data$start.age==30]/5
  master[6,1] <- 30
  master[6,2] <- "female"
  master[6,3] <- female.30
  
  ## 15 + all
  all.15.plus <- data$both[data$start.age==15] +
                 data$both[data$start.age==20] +
                 data$both[data$start.age==25] + 
                 data$both[data$start.age==30] +
                 data$both[data$start.age==35] + 
                 data$both[data$start.age==40] +
                 data$both[data$start.age==45] + 
                 data$both[data$start.age==50] +
                 data$both[data$start.age==55] +
                 data$both[data$start.age==60] +
                 data$both[data$start.age==65] + 
                 data$both[data$start.age==70] +
                 data$both[data$start.age==75] + 
                 data$both[data$start.age==80] +
                 data$both[data$start.age==85] +
                 data$both[data$start.age==90] + 
                 data$both[data$start.age==95] +
                 data$both[data$start.age==100] 
  master[7,1] <- "15+"
  master[7,2] <- "both"
  master[7,3] <- all.15.plus
  
  ## 15-49 female
  female.15.49 <- data$female[data$start.age==15] +
                  data$female[data$start.age==20] +
                  data$female[data$start.age==25] + 
                  data$female[data$start.age==30] +
                  data$female[data$start.age==35] + 
                  data$female[data$start.age==40] +
                  data$female[data$start.age==45] 
  master[8,1] <- "15-49"
  master[8,2] <- "female"
  master[8,3] <- female.15.49
  
  ## 15-69 both
  all.15.69 <- data$both[data$start.age==15] +
                data$both[data$start.age==20] +
                data$both[data$start.age==25] + 
                data$both[data$start.age==30] +
                data$both[data$start.age==35] + 
                data$both[data$start.age==40] +
                data$both[data$start.age==45] + 
                data$both[data$start.age==50] +
                data$both[data$start.age==55] +
                data$both[data$start.age==60] +
                data$both[data$start.age==65] 
  master[9,1] <- "15-69"
  master[9,2] <- "both"
  master[9,3] <- all.15.69
  
  ## 30 + all
  all.30.plus <- 
                data$both[data$start.age==30] +
                data$both[data$start.age==35] + 
                data$both[data$start.age==40] +
                data$both[data$start.age==45] + 
                data$both[data$start.age==50] +
                data$both[data$start.age==55] +
                data$both[data$start.age==60] +
                data$both[data$start.age==65] + 
                data$both[data$start.age==70] +
                data$both[data$start.age==75] + 
                data$both[data$start.age==80] +
                data$both[data$start.age==85] +
                data$both[data$start.age==90] + 
                data$both[data$start.age==95] +
                data$both[data$start.age==100] 
  master[10,1] <- "30+"
  master[10,2] <- "both"
  master[10,3] <- all.30.plus
  
  ## 30-69 all
  all.30.69 <- 
    data$both[data$start.age==30] +
    data$both[data$start.age==35] + 
    data$both[data$start.age==40] +
    data$both[data$start.age==45] + 
    data$both[data$start.age==50] +
    data$both[data$start.age==55] +
    data$both[data$start.age==60] +
    data$both[data$start.age==65] 
  master[11,1] <- "30-69"
  master[11,2] <- "both"
  master[11,3] <- all.30.69
  
  ## 50-69 all
  all.50.69 <- 
    data$both[data$start.age==50] +
    data$both[data$start.age==55] +
    data$both[data$start.age==60] +
    data$both[data$start.age==65] 
  master[12,1] <- "50-69"
  master[12,2] <- "both"
  master[12,3] <- all.50.69
  
  ## 0-69 all 
  all.0.69 <-     data$both[data$start.age==0] +
    data$both[data$start.age==5] +
    data$both[data$start.age==10] + 
    data$both[data$start.age==15] +
    data$both[data$start.age==20] +
    data$both[data$start.age==25] + 
    data$both[data$start.age==30] +
    data$both[data$start.age==35] + 
    data$both[data$start.age==40] +
    data$both[data$start.age==45] + 
    data$both[data$start.age==50] +
    data$both[data$start.age==55] +
    data$both[data$start.age==60] +
    data$both[data$start.age==65] 

  master[13,1] <- "0-69"
  master[13,2] <- "both"
  master[13,3] <- all.0.69
  
  ## All ages
  all.ages <-     data$both[data$start.age==0] +
                  data$both[data$start.age==5] +
                  data$both[data$start.age==10] + 
                  data$both[data$start.age==15] +
                  data$both[data$start.age==20] +
                  data$both[data$start.age==25] + 
                  data$both[data$start.age==30] +
                  data$both[data$start.age==35] + 
                  data$both[data$start.age==40] +
                  data$both[data$start.age==45] + 
                  data$both[data$start.age==50] +
                  data$both[data$start.age==55] +
                  data$both[data$start.age==60] +
                  data$both[data$start.age==65] + 
                  data$both[data$start.age==70] +
                  data$both[data$start.age==75] + 
                  data$both[data$start.age==80] +
                  data$both[data$start.age==85] +
                  data$both[data$start.age==90] + 
                  data$both[data$start.age==95] +
                  data$both[data$start.age==100] 
  master[14,1] <- "All ages"
  master[14,2] <- "both"
  master[14,3] <- all.ages
  
  
###############################
## write out master file ######
###############################
  write.csv(master,"./01_input_data/01_processed_UNPOP.csv",row.names=F)
  
  
  
  

  