
## Date:    17/07/2023
## Purpose: Calculate the optimal intervention package according to a single objective

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

## set seed
  set.seed(02139)

## set directory, bring in universal parameters
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  source("./02_code/00_universal_parameters.R")

## bring in intervention data
  data <- read.table(paste0("./03_output_data/04_combined_decision_vars.csv"), sep=",", header=T)
  
# ## create a social benefit combined variables 
#   data$sb.0.01 <- data$delta.deaths + data$che.10/0.01
#   data$sb.0.2 <- data$delta.deaths + data$che.10/0.2
#   data$sb.0.5 <- data$delta.deaths + data$che.10/0.5
#   data$sb.0.8 <- data$delta.deaths + data$che.10/0.8
#   data$sb.0.85 <- data$delta.deaths + data$che.10/0.85
#   data$sb.0.9 <- data$delta.deaths + data$che.10/0.9
#   data$sb.0.95 <- data$delta.deaths + data$che.10/0.95
#   data$sb.1 <- data$delta.deaths + data$che.10
#   data$sb.2 <- data$delta.deaths + data$che.10/2
#   data$sb.3 <- data$delta.deaths + data$che.10/3
#   data$sb.4 <- data$delta.deaths + data$che.10/4
#   data$sb.5 <- data$delta.deaths + data$che.10/5
#   data$sb.6 <- data$delta.deaths + data$che.10/6
#   data$sb.7 <- data$delta.deaths + data$che.10/7
#   data$sb.8 <- data$delta.deaths + data$che.10/8
#   data$sb.9 <- data$delta.deaths + data$che.10/9
#   data$sb.10 <- data$delta.deaths + data$che.10/10
#   data$sb.100 <- data$delta.deaths + data$che.10/100
#   data$sb.200 <- data$delta.deaths + data$che.10/200
#   data$sb.250 <- data$delta.deaths + data$che.10/250
#   data$sb.300 <- data$delta.deaths + data$che.10/300
  
  ## make a master holding file
  results <- expand.grid(int_n=unique(data$int_n),
                         objective=c("delta.deaths","che.10","che.25",
                                     "delta.deaths.LB","che.10.LB","delta.deaths.UB","che.10.UB",
                                     "delta.deaths.npv","E","E.LB", "E.UB"
                                     ),
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
      print(22 == dim(data)[1])
      
      ## step 1: vector of objective
      objective.in  <- data[[o]]
      
      ## step 2: create constraint martix 
      ## constraint 1: cost can't exceed budget
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
  write.table(results,paste0("./03_output_data/05_optimized_interventions_single_obj.csv"),sep=",",row.names=F)

  ## create a file with the total cost, che averted, and lives saved for benefit packages
  data <- data[,c("int_n","int_des","total_cost","delta.deaths","che.10","che.25",
                  "delta.deaths.LB","che.10.LB","delta.deaths.UB","che.10.UB",
                  "delta.deaths.npv", "E","E.LB", "E.UB")]
  
  data <- unique(data)
  20 == dim(data)[1]
  results <- merge(results,data,by="int_n",all.x=T)
  results <- results[order(results$int_n,results$int_des,results$objective,
                           results$budget),]
  
  # cost of programs and total deaths/che averted
  results$package.cost                <- results$total_cost*results$fund
  results$package.deaths              <- results$delta.deaths*results$fund
  results$package.che.10              <- results$che.10*results$fund
  
  results$package.deaths.LB           <- results$delta.deaths.LB*results$fund
  results$package.che.10.LB           <- results$che.10.LB*results$fund
  results$package.deaths.UB           <- results$delta.deaths.UB*results$fund
  results$package.che.10.UB           <- results$che.10.UB*results$fund
  
  results$package.che.25              <- results$che.25*results$fund
  results$package.deaths.npv          <- results$delta.deaths.npv*results$fund
  
  results$package.E           <- results$E*results$fund
  results$package.E.UB           <- results$E.UB*results$fund
  results$package.E.LB           <- results$E.LB*results$fund
  
  # results$package.sb.0.01             <- results$sb.0.01*results$fund
  # results$package.sb.0.2              <- results$sb.0.2*results$fund
  # results$package.sb.0.5              <- results$sb.0.5*results$fund
  # results$package.sb.0.8              <- results$sb.0.8*results$fund
  # results$package.sb.0.85             <- results$sb.0.85*results$fund
  # results$package.sb.0.9              <- results$sb.0.9*results$fund
  # results$package.sb.0.95             <- results$sb.0.95*results$fund
  # results$package.sb.1                <- results$sb.1*results$fund
  # results$package.sb.2                <- results$sb.2*results$fund
  # results$package.sb.3                <- results$sb.3*results$fund
  # results$package.sb.4                <- results$sb.4*results$fund
  # results$package.sb.5                <- results$sb.5*results$fund
  # results$package.sb.6                <- results$sb.6*results$fund
  # results$package.sb.7                <- results$sb.7*results$fund
  # results$package.sb.8                <- results$sb.8*results$fund
  # results$package.sb.9                <- results$sb.9*results$fund
  # results$package.sb.10               <- results$sb.10*results$fund
  # results$package.sb.100              <- results$sb.100*results$fund
  # results$package.sb.200              <- results$sb.200*results$fund
  # results$package.sb.250              <- results$sb.250*results$fund
  # results$package.sb.300              <- results$sb.300*results$fund
  # 
  # results$package.nb.1              <- results$nb.1*results$fund
  # results$package.nb.2              <- results$nb.2*results$fund
  # results$package.nb.3              <- results$nb.3*results$fund
  # results$package.nb.4              <- results$nb.4*results$fund
  
  ## package-level dataset
  pc <- aggregate(cbind(package.cost,package.deaths,
                        package.che.10,package.che.25,
                        package.deaths.LB, package.che.10.LB,
                        package.deaths.npv, package.deaths.UB, package.che.10.UB,
                        package.E,package.E.LB,package.E.UB) ~ 
                        objective + budget, FUN = sum,data=results)
  
  ## get rid of the zeros
  pc <- pc[pc$package.cost!=0,] # if 0 then didn't pick anything, not useful
  pc <- pc[,c("objective","budget",
              "package.deaths","package.che.10","package.che.25",
              "package.deaths.LB","package.che.10.LB","package.deaths.UB","package.che.10.UB",
              "package.deaths.npv","package.E","package.E.LB","package.E.UB")]
  pc <- unique(pc)
  
  ## make sure NPV variables are rounded to full numbers
  pc$package.deaths.npv <- round(pc$package.deaths.npv)
  pc$package.deaths <- round(pc$package.deaths)
  pc$package.deaths.LB <- round(pc$package.deaths.LB)
  pc$package.deaths.UB <- round(pc$package.deaths.UB)

  
  ## save files
  write.table(pc,paste0("./03_output_data/05_optimized_package_counts_single_obj.csv"),sep=",",row.names=F)

  
  

