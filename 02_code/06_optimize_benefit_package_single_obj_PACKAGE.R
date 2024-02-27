## Author:  Kate Lofgren (klofgren@g.harvard.edu)
## Date:    8/18/2018
## Purpose: Calculate the optimal intervention package

## set up
  rm(list=ls())
  date <- Sys.Date()
  Sys.setenv(R_MAX_VSIZE = 16e9)

## libraries
  library(foreign)
  library(dplyr)
  library(lpSolve)
  library(reshape2)
  library(viridis)
  library(gdata)

## set seed
  set.seed(02139)

## set directory, bring in universal parameters
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  source("./02_code/00_universal_parameters.R")

############################################
## Loop over average, LB and UB scenarios ##
############################################
  ## bring in intervention data
  data <- read.table(paste0("./03_output_data/04_combined_decision_vars.csv"), sep=",", header=T)
  
  ## merge in WHO SDG Target Group
  ## Intervention Main Info
  names <- read.xls(file,sheet="Interventions - Basics")
  names$X <- NULL
  names <- names[,1:2]
  names(names) <- c("WHO","int_n")
  data <- merge(data,names,by=c("int_n"),all.X=T)
  # change so int_n = WHO group
  data$int_n <- NULL
  data$int_n <- data$WHO
  data$WHO <- NULL
  temp <- aggregate(data=data,delta.deaths ~ int_n,FUN=sum)
  temp2 <- aggregate(data=data,che.10 ~ int_n,FUN=sum)
  temp3 <- aggregate(data=data,total_cost ~ int_n,FUN=sum)
  d <- merge(temp,temp2,by=c("int_n"))
  d <- merge(d,temp3,by=c("int_n"))
  data <- d
  
  ## make a master holding file
  results <- expand.grid(int_n=unique(data$int_n),
                         objective=c("delta.deaths","che.10"),
                         budget=unique(budgets),
                         fund=NA)
  
  
  for(o in unique(results$objective)) {
    ## optimize package testing
    # o <- "che.10"
    # b <- 6000000
    print("**********************")
    print(paste0("on objective ",o))
    for(b in unique(results$budget)) {
      #print(paste0("on budget ",b))
      #####################################
      ## Define Optimization Components ###
      #####################################
      ## make sure there is 1 row for each intervention
      9 == dim(data)[1] # 9 instead of 20 here b/c these are packages
      
      ## step 1: vector of objective
      objective.in  <- data[[o]]
      
      ## step 2: create constraint martix 
      ## constraint: cost can't exceed budget
      c1 <- data$total_cost 
      
      ## combine all constraints into a matrix
      const.mat <- matrix(c(c1),nrow=1, byrow=TRUE)
      
      ## define the constraint bounds
      const.rhs <- c(b)
      
      ## define the direction of the constraints
      const.dir  <- c("<=") 
      
      ##########################
      ## Run the Optimization ##
      ##########################
      ## Find the optimal solution (includes a binary decision variable argument)
      optimum <-  lp(direction="max",  objective.in, const.mat, const.dir, const.rhs, all.bin=TRUE)
      
      ## save the results
      results[results$objective==o & results$budget==b,c("fund")] <- optimum$solution
    } # budget loop 
  } # objective loop
  
  ## save files
  write.table(results,paste0("./03_output_data/05_optimized_interventions_single_obj_package.csv"),sep=",",row.names=F)

  ## create a file with the total cost, deaths averted, and lives saved for benefit packages
  data <- data[,c("int_n","total_cost","delta.deaths","che.10")]
  
  data <- unique(data)
  9 == dim(data)[1] # number of packages (instead of 20 interventions)
  results <- merge(results,data,by="int_n",all.x=T)
  results <- results[order(results$int_n,results$objective,
                           results$budget),]
  
  # cost of programs and total deaths/che averted
  results$package.cost                <- results$total_cost*results$fund
  results$package.deaths              <- results$delta.deaths*results$fund
  results$package.che.10              <- results$che.10*results$fund
 

  ## package-level dataset
  pc <- aggregate(cbind(package.cost,package.deaths,
                        package.che.10) ~ 
                    objective + budget,FUN=sum,data=results)
  
  ## get rid of the zeros
  pc <- pc[pc$package.cost!=0,] # if 0 then didn't pick anything, not useful
  pc <- pc[,c("objective","budget",
              "package.deaths","package.che.10")]
  pc <- unique(pc)
  
  ## save files
  write.table(pc,paste0("./03_output_data/05_optimized_package_counts_single_obj_package.csv"),sep=",",row.names=F)



  
