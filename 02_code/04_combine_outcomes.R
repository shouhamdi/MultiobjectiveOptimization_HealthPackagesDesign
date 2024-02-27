
## Date:      17/08/2023
## Purpose:   (1) combine independent and linked files with the 3 objectives
##            (2) combine CHE, Death outcomes and Equity into a same file

## This file specifically deals with uncertainty across treatment effect and disease burden

## set up
  rm(list=ls())
  date <- Sys.Date()

## libraries
  library(foreign)
  library(dplyr)

## set seed
  set.seed(02139)

## set directory, bring in universal parameters
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  source("./02_code/00_universal_parameters.R")

############################################
## Loop over average, LB and UB scenarios ##
############################################
ticker <- 0
  
for(e in c("MEAN","LB","UB")) {
  for(s in c("MEAN","LB","UB")) {

    ticker <- ticker + 1
    
    ## bring in independent and linked files and combine
    ind <- read.table(paste0("./03_output_data/03_IND_deaths_che_index.csv"), sep=",", header=T)
    ## subset to the right uncertainty scenario
    ind <- ind[ind$int_eff_uncert==e & ind$pop_burden_uncert==s,]
    
    ind$type <- "independent"
    # ind$delta.deaths.npv <- ind$delta.deaths
    # ind$che.10.direct <- ind$che.10
    # ind$che.10.npv <- ind$che.10
    # ind$che.25.direct <- ind$che.25
    # ind$che.25.npv <- ind$che.25
    linked  <- read.table(paste0("./03_output_data/03_LINKED_deaths_che_index.csv"), sep=",", header=T)
    ## subset to the right uncertainty scenario
    linked <- linked[linked$int_eff_uncert==e & linked$pop_burden_uncert==s,]
    linked$type <- "linked"
    data <- rbind(ind,linked)
    
    ## bring in equity file and combine
    equity <- read.table(paste0("./03_output_data/03_equity.csv"), sep=",", header=T)
    ## subset to the right uncertainty scenario
    equity <- equity[equity$int_eff_uncert==e & equity$pop_burden_uncert==s,]
    ##keep the relevant variable
    equity <- equity[,c("int_n","E")]
    ##add equity to data
    data <- merge(data,equity, by="int_n",all.x = T)
    
    
    ## aggregate results so each row is one intervention (BEmOC and pentavalent are in both)
    int.3 <- aggregate(cbind(delta.deaths,delta.deaths.npv,
                             che.10,
                             che.25, E) ~ int_n,  FUN = sum, data[data$int_n==3,])
    data[data$int_n==3 & data$type=="independent",c("delta.deaths","delta.deaths.npv",
                                                    "che.10",
                                                    "che.25","E")] <- int.3[1,2:6]
    data$remove <- 0
    data$remove[data$int_n==3 & data$type=="linked"] <- 1
    data <- data[data$remove==0,]
    data$remove <- NULL
    print(dim(data)[1])
    
    int.10 <- aggregate(cbind(delta.deaths,delta.deaths.npv,
                              che.10,
                              che.25,E) ~ int_n,  FUN = sum, data[data$int_n==10,])
    data[data$int_n==10 & data$type=="independent",c("delta.deaths","delta.deaths.npv",
                                                     "che.10",
                                                     "che.25","E")] <- int.10[1,2:6]
    data$remove <- 0
    data$remove[data$int_n==10 & data$type=="linked"] <- 1
    data <- data[data$remove==0,]
    data$remove <- NULL
    
    # ## Add net monetery benefit variables as outcomes
    # data$nb.1 <- data$delta.deaths*wtp - data$total_cost 
    # data$nb.2 <- data$delta.deaths*wtp*2 - data$total_cost
    # data$nb.3 <- data$delta.deaths*wtp*3 - data$total_cost
    # data$nb.4 <- data$delta.deaths*wtp*4 - data$total_cost
    
    ## make sure all the interventions are there
    print(length(unique(data$int_n))==20)
    
    ## add variables to track uncertainty scenario
    data$int_eff_uncert <- e
    data$pop_burden_uncert <- s
    
    ## save file with all vars
    if(ticker==1) {
      save <- data
    } else {
      save <- rbind(save,data)
    }
  } # closes the pop/burden uncertainty loop
} # closes the int eff uncertainty loop

## save the results across all uncertainty scenarios
  write.table(save,paste0("./03_output_data/04_combined_decision_vars_all_scenarios.csv"),sep=",",row.names=F)

## for the analysis -- only need the mean + the most extreme high and low scenarios
  ## find the extreme values
  min.deaths <- aggregate(delta.deaths ~ int_n, data=save, function(x) min(x))
  names(min.deaths)[2] <- "delta.deaths.LB"
  max.deaths <-  aggregate(delta.deaths ~ int_n, data=save, FUN=max)
  names(max.deaths)[2] <- "delta.deaths.UB"
  
  min.che <- aggregate(che.10 ~ int_n, data=save, function(x) min(x))
  names(min.che)[2] <- "che.10.LB"
  max.che <-  aggregate(che.10 ~ int_n, data=save, FUN=max)
  names(max.che)[2] <- "che.10.UB"
  
  min.E <- aggregate(E ~ int_n, data=save, function(x) min(x))
  names(min.E)[2] <- "E.LB"
  max.E <-aggregate(E ~ int_n, data=save, FUN=max)
  names(max.E)[2] <- "E.UB"
  
  ## just keep MEAN/MEAN scenario with min/max added on
  temp <- save[save$int_eff_uncert=="MEAN" & save$pop_burden_uncert=="MEAN",]
  temp <- merge(temp,min.deaths,by="int_n")
  temp <- merge(temp,max.deaths,by="int_n")
  temp <- merge(temp,min.che,by="int_n")
  temp <- merge(temp,max.che,by="int_n")
  temp <- merge(temp,min.E,by="int_n")
  temp <- merge(temp,max.E,by="int_n")
  
## save the results
  write.table(temp,paste0("./03_output_data/04_combined_decision_vars.csv"),sep=",",row.names=F)


  
  
