
## Date:    17/07/2023
## Purpose: Calculate the optimal intervention package

## Note:    In this version focusing on an exhaustive feasability creation. 
###         This adds computation time so is limited to one budget and just deaths / che.10 right now
###         This version does not include temporal dependency constraints 

## set up
rm(list=ls())
date <- Sys.Date()

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

for(x in c(200)) {
  for(y in c(50)) {
    
    
    ## bring in intervention data
    data <- read.table("./03_output_data/04_combined_decision_vars.csv", sep=",", header=T)
    
    ## figure out required K values based on constraint
    k.range <- read.table("./03_output_data/05_optimized_package_counts_single_obj.csv",sep=",",header=T)
    k.range <- k.range[,c("objective","package.deaths","package.che.10","package.che.25","package.E")]
    k.range <- k.range[k.range$objective %in% c("che.10","E"),]
    
    
    k.che <- c(0,max(k.range$package.che.10[k.range$objective=="che.10"])) # want the max when che max on che
    k.equity <- c(0,max(k.range$package.E[k.range$objective=="E"])) # want the max when deaths max on deaths

    epsilon.che<-x
    epsilon.equity<-y
    
    
    budget <- seq(10000000, 100000000,length.out= 5)
    
    results.deaths <- expand.grid(int_n=unique(data$int_n),objective=c("delta.deaths"),
                                  budget=budget,
                                  k=seq(k.che[1],k.che[2],length.out=epsilon.che), k_bis=seq(k.equity[1],k.equity[2],length.out=epsilon.equity),
                                  constraint=c("<="),
                                  fund=NA)

    
    o <-"delta.deaths"
    
    print("**********************")
    print(paste0("on objective ",o))
    
    for(b in unique(results.deaths$budget)) { # doesn't matter that this references a death specific dataset
      print("***********")
      print(paste0("on budget ",b))
      
      ## assign the secondary and third objective
      
      o.2 <- "che.10" # second objective
      k.vec <- seq(k.che[1],k.che[2],length.out=epsilon.che) 
      o.3 <- "E" #third objective 
      k.vec_bis <- seq(k.equity[1],k.equity[2],length.out=epsilon.equity)
      
      for(k in k.vec) {
        for(k_bis in k.vec_bis) {
          
          ## to keep track of progress
          if(k %in% seq(k.che[1],k.che[2],length.out=epsilon.che/10) & k_bis %in% seq(k.equity[1],k.equity[2],length.out=epsilon.equity/10)) {
            print(paste0("on K constraint ",k, "and K_bis constraint ", k_bis))
          }
          
          
          
          for(c in unique(results.deaths$constraint)) { # doesn't matter that this references a death specific dataset
           
     
            #####################
            ######## P1 #########
            #####################
            
            #####################################
            ## Define Optimization Components ###
            #####################################
            
            ## make sure there is 1 row for each intervention
            20 == dim(data)[1]
            
            ## step 1: vector of objective
            objective.in  <- data[[o]]
            
            ## step 2: create constraint martix 
            ## constraint 1: cost can't exceed budget
            c1 <- data$total_cost 
            
            ## constraint 2: secondary objective
            c2 <- data[[o.2]]
            
            ## constraint 3: third objective
            c3 <- data[[o.3]]
            
            ## combine all constraints into a matrix
            const.mat <- matrix(c(c1,c2,c3),nrow=3, byrow=TRUE)
            
            ## define the constraint bounds for P1
            const.rhs <- c(b,k,k_bis)
            
            ## define the direction of the constraints
            if(c == "<=") {
              const.dir  <- c("<=",">=",">=") 
            } else {
              const.dir  <- c("<=",">=",">=") 
            }
            
            
            ##########################
            ## Run the Optimization ##
            ##########################

            ## Find the optimal solution (includes a binary decision variable argument)
            optimum1 <-  lp(direction="max",  objective.in, const.mat, const.dir, const.rhs, all.bin=TRUE)
            
            ### calculate the optimal solution for P1
            results.P1 <- data[,c("int_n","int_des","total_cost","delta.deaths","che.10","che.25","E")]

            
            results.P1$fund <- optimum1$solution
            results.P1$ag <- 1
            results.P1$package.cost <- results.P1$total_cost*results.P1$fund
            results.P1$package.deaths <- results.P1$delta.deaths*results.P1$fund
            results.P1$package.che.10 <- results.P1$che.10*results.P1$fund
            results.P1$package.che.25 <- results.P1$che.25*results.P1$fund
            results.P1$package.E <- results.P1$E*results.P1$fund
            
            results.P1 <- aggregate(cbind(package.cost,package.deaths,package.che.10,package.che.25,package.E) ~ 
                              ag ,FUN = sum,data=results.P1)
            
            Obj.1.P1 <-results.P1$package.deaths
            Obj.2.P1 <-results.P1$package.che.10
            Obj.3.P1 <-results.P1$package.E
            
            
            ####################
            ######## P2 ########
            ####################
            
            #####################################
            ## Define Optimization Components ###
            #####################################
            
            ## step 1: vector of objective
            objective.in  <- data[[o.2]]
            
            ## step 2: create constraint martix 
            ## constraint 1: cost can't exceed budget
            c1 <- data$total_cost 
            
            ## constraint 2: secondary objective
            c2 <- data[[o]]
            
            ## constraint 3: third objective
            c3 <- data[[o.3]]
            
            ## combine all constraints into a matrix
            const.mat <- matrix(c(c1,c2,c3),nrow=3, byrow=TRUE)
            
            ## define the constraint bounds for P1
            const.rhs <- c(b,Obj.1.P1,Obj.3.P1)
            
            ## define the direction of the constraints
            if(c == "<=") {
              const.dir  <- c("<=",">=",">=") 
            } else {
              const.dir  <- c("<=",">=",">=") 
            }
            
            
            ##########################
            ## Run the Optimization ##
            ##########################
            
            ## Find the optimal solution (includes a binary decision variable argument)
            optimum2 <-  lp(direction="max",  objective.in, const.mat, const.dir, const.rhs, all.bin=TRUE)

            ### calculate the optimal solution for P2
            results.P2 <- data[,c("int_n","int_des","total_cost","delta.deaths","che.10","che.25","E")]

            results.P2$fund <- optimum2$solution
            results.P2$ag <- 1
            results.P2$package.cost <- results.P2$total_cost*results.P2$fund
            results.P2$package.deaths <- results.P2$delta.deaths*results.P2$fund
            results.P2$package.che.10 <- results.P2$che.10*results.P2$fund
            results.P2$package.che.25 <- results.P2$che.25*results.P2$fund
            results.P2$package.E <- results.P2$E*results.P2$fund
            
            results.P2 <- aggregate(cbind(package.cost,package.deaths,package.che.10,package.che.25,package.E) ~ 
                                      ag ,FUN = sum,data=results.P2)
            
            Obj.1.P2 <-results.P2$package.deaths
            Obj.2.P2 <-results.P2$package.che.10
            Obj.3.P2 <-results.P2$package.E
            
            ####################
            ######## P3 ########
            ####################
            
            #####################################
            ## Define Optimization Components ###
            #####################################
            
            ## make sure there is 1 row for each intervention
            20 == dim(data)[1]
            
            ## step 1: vector of objective
            objective.in  <- data[[o.3]]
            
            ## step 2: create constraint martix 
            ## constraint 1: cost can't exceed budget
            c1 <- data$total_cost 
            
            ## constraint 2: secondary objective
            c2 <- data[[o]]
            
            ## constraint 3: third objective
            c3 <- data[[o.2]]
            
            ## combine all constraints into a matrix
            const.mat <- matrix(c(c1,c2,c3),nrow=3, byrow=TRUE)
            
            ## define the constraint bounds for P1
            const.rhs <- c(b,Obj.1.P2,Obj.2.P2)
            
            ## define the direction of the constraints
            if(c == "<=") {
              const.dir  <- c("<=",">=",">=") 
            } else {
              const.dir  <- c("<=",">=",">=") 
            }
            
            
            ##########################
            ## Run the Optimization ##
            ##########################

            ## Find the optimal solution (includes a binary decision variable argument)
            optimum3 <-  lp(direction="max",  objective.in, const.mat, const.dir, const.rhs, all.bin=TRUE)
            
            
            ############ save the final results

              results.deaths[results.deaths$objective==o & 
                               results.deaths$budget==b & 
                               results.deaths$k==k & 
                               results.deaths$k_bis==k_bis &
                               results.deaths$constraint==c,c("fund")] <- optimum3$solution
            
            
          } # constraint direction loop
        } # (k) constant value loop
      } # (k_bis) constant value loop
    } # budget loop 
    

    
    
    # create a result file 
    results <- results.deaths
    
    ## save files
    write.table(results, paste("./03_output_data/optimization_runs/interventions/05_optimized_interventions_full_frontier_with0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_200to700M_NEW.csv"),sep=",",row.names=F)
    
    ## create a file with the total cost, deaths averted, and lives saved for benefit packages
    data <- data[,c("int_n","int_des","total_cost","delta.deaths","che.10","che.25","E")]
    # data <- unique(data)
    print(dim(data)[1])
    20 == dim(data)[1]
    results <- merge(results,data,by="int_n",all.x=T)
    results <- results[order(results$int_n,results$int_des,results$objective,results$budget,results$k,results$k_bis,results$constraint),]
    
    # cost of programs and total deaths/che averted +equity
    results$package.cost <- results$total_cost*results$fund
    results$package.deaths <- results$delta.deaths*results$fund
    results$package.che.10 <- results$che.10*results$fund
    results$package.che.25 <- results$che.25*results$fund
    results$package.E <- results$E*results$fund
    
    pc <- aggregate(cbind(package.cost,package.deaths,package.che.10,package.che.25,package.E) ~ 
                      objective + budget + k + k_bis + constraint,FUN = sum,data=results)
    
    pc<-pc[order(pc$objective,pc$budget,pc$k,pc$k_bis,pc$constraint),]
    
    ## save files
    write.table(pc,paste("./03_output_data/optimization_runs/package_with_0/05_optimized_package_counts_full_frontier_with0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_200to700M_NEW.csv"),sep=",",row.names=F)

    ## get rid of the zeros
    pc <- pc[pc$package.cost!=0,] # if 0 then didn't pick anything, not useful
    
    ## save files
    write.table(pc,paste("./03_output_data/optimization_runs/package_without_0/05_optimized_package_counts_full_frontier_without0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_200to700M_NEW.csv"),sep=",",row.names=F)
    
  } #closes the epsilon.equity loop
} #closes the epsilon.cheloop

