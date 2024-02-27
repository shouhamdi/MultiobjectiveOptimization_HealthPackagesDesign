
## Date:    6/23/2023
## Purpose: Calculate health gains equity 

## set up
rm(list=ls())
date <- Sys.Date()
options(scipen = 9999)

## libraries
library(foreign)
library(gdata)
library(dplyr)

## set seed
set.seed(02139)

## set directory
setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
source("./02_code/00_universal_parameters.R")

## bring in income draws
hh.incomes <- read.table("./03_output_data/income_draws_truncated.csv",sep=",",header=T)
names(hh.incomes) <- "hh.incomes"

#class(hh.incomes)
hh.incomes<- hh.incomes[order(hh.incomes$hh.incomes), , drop = FALSE]
#class(hh.incomes)

total.draws_truncated = floor(9/10*total.draws)
q_len <- floor(total.draws_truncated/5)
names(hh.incomes) <- c("incomes")

# Calculate the quintile of each income in hh.incomes
hh.incomes$quintile <- cut(hh.incomes$incomes, quantile(hh.incomes$incomes, probs = 0:5/5), labels = FALSE)


## bring in utilization per quintile
util_proxys <- read.csv("./01_input_data/03_util_proxys.csv",sep = ";", header=T,stringsAsFactors=F)

util_proxys <- util_proxys %>%
  mutate(util = as.numeric(gsub(",", ".", util)))


## bring in temporal spacing to map intervention information
print("USING FILE:")
print(file)

map <- read.xls(file,sheet="Temporal Spacing",stringsAsFactors=F)
names(map)[1] <- "gbd_n"
names(map)[2] <- "gbd_des"

## for this, keep every intervention
map$int_n <- NA
map$int_des <- NA
map$int_n[is.na(map$int_2_n)] <- map$int_1_n[is.na(map$int_2_n)]
map$int_des[is.na(map$int_2_n)] <- map$int_1_des[is.na(map$int_2_n)]
map$int_n[!is.na(map$int_2_n)] <- map$int_2_n[!is.na(map$int_2_n)]
map$int_des[!is.na(map$int_2_n)] <- map$int_2_des[!is.na(map$int_2_n)]

# Create a new data frame containing rows where 'int_2_n' is not NA
map_not_na <- map[!is.na(map$int_2_n), ]
map_not_na$int_n[!is.na(map_not_na$int_2_n)] <- map$int_1_n[!is.na(map$int_2_n)]
map_not_na$int_des[!is.na(map_not_na$int_2_n)] <- map$int_1_des[!is.na(map$int_2_n)]
# Create a new data frame by appending the 'map_not_na' data frame to gather all interventions
map <- rbind(map, map_not_na)

map <- map[,c("gbd_n","gbd_des","int_n")]

## create a list of all the interventions we should have at end of code for error checking
check <- unique(map$int_n)
check[which(check %in% c(3.1,3.3))] <- 3
check[which(check %in% c(4.1,4.2))] <- 4
check <- unique(check)

## bring in input parameter master file(s)
data <- read.csv("./01_input_data/02_final_input_parameters.csv",header=T,stringsAsFactors=F)

## merge intervention info onto mapping file
data <- merge(map,data,by=c("gbd_n","int_n"),all.x=T)

## make a indicator for cases where incidence = 0
data$zero.incidence <- 0
data$zero.incidence[data$incidence==0] <- 1

# create a variable for OOP cost as a percent
data$oop.cost.per <- data$oop.cost
data$oop.cost <- NA 

# keep a master file to pull variables from
master <- data

############################################
## Loop over potential LB/UB scenarios    ##
############################################
# create a ticker
ticker <- 0

for(e in c("MEAN","LB","UB")) {
  if(e=="MEAN") {
    int_eff <- master$int_eff
  } else if(e=="LB") {
    int_eff <- master$int_eff_LB
  } else {
    int_eff <- master$int_eff_UB
  }
  for(s in c("MEAN","LB","UB")) {
    ## assign the right varaibles - set up
    if(s=="MEAN") {
      ## average
      target_n <- master$target_n
      incidence <- master$incidence
      CFR <- master$CFR
      deaths <- master$deaths
    } else if(s=="LB") {
      # LB
      target_n <- master$target_n_LB
      incidence <- master$incidence_LB
      CFR <- master$CFR_LB
      deaths <- master$deaths_LB
    } else {
      # UB
      target_n <- master$target_n_UB
      incidence <- master$incidence_UB
      CFR <- master$CFR_UB
      deaths <- master$deaths_UB
    }
    
    # add 1 to ticker
    ticker <- ticker + 1
    
    ## calculate the newly treated population
    data$new_tx_pop <- NA
    ## 1 - when intervention target is incidence or prevalence -- include burden addressable variable
    data$new_tx_pop[data$int_target != "population"] <- cov.delta *
      target_n[data$int_target != "population"] *
      data$burden_amenable[data$int_target != "population"] 
    ## 2 - when intervention target is population -- goes to everyone -- don't want addressable %
    data$new_tx_pop[data$int_target == "population"] <- cov.delta *
      target_n[data$int_target == "population"] 
    ## 3 - CEmOC is an exception where the "population" is all births and only 17% need CEmOC
    data$new_tx_pop[data$int_n == 4] <- cov.delta *
      target_n[data$int_n == 4] *
      data$burden_amenable[data$int_n == 4] 
    
    ## calculate the population previously seeking care -- that are now covered (i.e. the baseline coverage pop)
    data$baseline_coverage_pop <- NA
    ## 1 - when intervention target is incidence or prevalence -- include burden addressable variable
    data$baseline_coverage_pop[data$int_target != "population"] <- data$baseline_cov[data$int_target != "population"] *
      target_n[data$int_target != "population"] *
      data$burden_amenable[data$int_target != "population"] 
    
    ## 2 - when intervention target is population -- goes to everyone -- don't want addressable %
    data$baseline_coverage_pop[data$int_target == "population"] <- data$baseline_cov[data$int_target == "population"] *
      target_n[data$int_target == "population"] 
    
    ## 3 - CEmOC is an exception where the "population" is all births and only 17% need CEmOC
    data$baseline_coverage_pop[data$int_n == 4] <- data$baseline_cov[data$int_n == 4] *
      target_n[data$int_n == 4] *
      data$burden_amenable[data$int_n == 4] 
    
    ## calculate the total cost of scaling up coverage
    ## whole unit cost for newly covered, just OOP for baseline coverage
    data$total_cost <- (data$new_tx_pop*data$unit.cost) + (data$baseline_coverage_pop*data$oop.cost.per*data$unit.cost)
    
    ## covert OOP to dollars instead of %
    data$oop.cost <- data$oop.cost.per*data$unit.cost
    
    ## covert OOP for screen/treat costs to dollars instead of %
    data$oop.cost.screen <- data$oop.cost.per*data$uc.screen
    data$oop.cost.treat <- data$oop.cost.per*(data$uc.treat + data$uc.screen) # both screened and treated
    
    #################################
    ## Deaths averted per $ spent  ##
    ################################# 
    ## Change in deaths is just for the newly covered
    ## Calculations for when efficacy estimate is for INCIDENCE
    data$delta.incidence <- NA
    i <- which(data$int_eff_target=="incidence")
    data$delta.incidence[i] <- cov.delta*int_eff[i]*data$burden_amenable[i]*incidence[i]
    
    
    ## for changes in incidence, now calculate delta deaths based on change in case fatality ratio
    data$delta.deaths <- NA
    data$delta.deaths[i] <- CFR[i]*data$delta.incidence[i]
    
    ## Calculations for when efficacy estimate is for MORTALITY
    m <- which(data$int_eff_target=="mortality")
    data$delta.deaths[m] <-   cov.delta*
      int_eff[m]*
      data$burden_amenable[m]*
      deaths[m]
    
    
    ############################
    ## Assess health gain equity 
    ############################
    data$E<-NA
    
    n_seed<-2
    n_iter <- n_seed*dim(data)[1]# Number of iterations of the loop
    
    # Initializes the progress bar
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = n_iter, # Maximum value of the progress bar
                         style = 3,    # Progress bar style (also available style = 1 and style = 2)
                         width = 50,   # Progress bar width. Defaults to getOption("width")
                         char = "=")   # Character used to create the bar
    
    ## average over several runs
    temp <- data.frame(gbd_n = double(),
                       int_n = double(),
                       E = double(),
                       seed = double())
    for(n in 1:n_seed){
      set.seed(n)
      if (n==1){
        # Create a PDF device
        pdf("Equity_plots.pdf")
        # Set the number of rows and columns for plot arrangement
        nrows <- 3  # Number of rows
        ncols <- 1  # Number of columns
      }
      
      for(r in 1:dim(data)[1]) {
        intervention <- data$int_n[r]
        
        ### Allocate the utilization per quintile 
        hh.incomes$prob <-NA
        util_proxys_subset<- subset(util_proxys, int_n == intervention, select = c( "util", "quintile"))
        #Identify duplicated rows
        duplicated_rows <- duplicated(util_proxys_subset)
        # Remove duplicated rows
        util_proxys_subset_no_duplicates <- util_proxys_subset[!duplicated_rows, ]
        #print(length(util_proxys_subset_no_duplicates))
        merged <- merge(hh.incomes, util_proxys_subset_no_duplicates, by = "quintile", all.x = T)
        #print(length(merged))
        hh.incomes$prob <-merged$util
        #change the last row value which is NA
        len <- length(hh.incomes$prob)
        hh.incomes$prob[len] <- hh.incomes$prob[len-1]
        

        ### draw the sample according to these utilization
        I <- seq(1, len)
        n_ante <- deaths[r]
        n_post <- deaths[r] - data$delta.deaths[r]
        
        i.ante <- sample(I, n_ante, replace = TRUE, prob = (1 - hh.incomes$prob*0.002*int_eff[r]/q_len))
        tx.incomes.ante<- hh.incomes$incomes[i.ante]
        i.post <- sample(I, n_post, replace = TRUE, prob = (1 - hh.incomes$prob*0.002*int_eff[r]/q_len))
        tx.incomes.post<- hh.incomes$incomes[i.post]
        
        
        int_des <- data[r, "int_des"]
        gbd_des <- data[r, "gbd_des"]  

        ### Calculate the empirical cumulative distribution function of number of deaths per income ante and post
        cdf_ante <- ecdf(tx.incomes.ante)
        cdf_post <- ecdf(tx.incomes.post)

        # Plot the CDF
        par(mfrow = c(nrows, ncols), mar = c(4, 4, 2, 1))  # Set the layout and margins
        plot(cdf_ante, col = "blue",
             xlab = "Increasing income", ylab = "Cumulative Probability of deaths before",
             main = paste("Cumulative Distribution Function of deaths for ", gbd_des, "using", int_des))

        lines(cdf_post, col = "red",
              xlab = "Increasing income", ylab = "Cumulative Probability of deaths after")
        # Add gridlines
        grid()


        
        # Calculate area between the two functions
        f <- function(x) {cdf_post(x) - cdf_ante(x)}
        E <- integrate(f, 0, max(tx.incomes.ante,tx.incomes.post) ,subdivisions = 200000)$value
        #normalize <- integrate(function(x) abs(sigmoid(x)), 0, max(i.per))$value
        normalize <- 1
        
        E <- E/normalize
        
        ## Store the Equity value in a vector by adding a prevalence weight
        E_value <- E*n_ante/total.draws_truncated
  
        
        ## Append the values to the temp data frame
        temp <- rbind(temp, data.frame(gbd_n = data$gbd_n[r],int_n = data$int_n[r], E=E_value, seed = n))
        
    }  # closes row loop
      if (n==1){
        # Close the PDF device and save the plots
        dev.off()
      }
  } # closes the seed loop
    
    close(pb) # Close the connection
    
  
  ## Average the different runs 
  averaged_temp <- temp %>%
    group_by(gbd_n, int_n) %>%
    summarize(E = mean(E))
  
  
  data$E<-averaged_temp$E
  
  
  
  
  ## generate time-lag discounted versions
  data$t <- NA
  data$t[data$time_lag=="Immediate"] <- 0
  data$t[data$time_lag=="5 years"] <- 5
  data$t[data$time_lag=="10 years"] <- 10
  data$t[data$time_lag=="20 years"] <- 20
  data$delta.deaths.npv <- data$delta.deaths/(1.03^data$t)
  
  
  # add variables to track what uncertainty scenarios are being used
  data$int_eff_uncert <- e
  data$pop_burden_uncert <- s
  
    
    ## save file with all vars
    if(ticker==1) {
      save <- data
    } else {
      save <- rbind(save,data)
    }
  } # closes the loop for burden/pop uncertainty scenarios
} # closes loop for intervention risk reduction uncertainty scenarios

## save aggregate file with all possible scenarios
write.table(save,paste0("./03_output_data/03_all_vars_equity.csv"),sep=",",row.names=F)

###### aggregate the results for each scenario ##########
ticker2 <- 0
for(e in c("MEAN","LB","UB")) {
  for(s in c("MEAN","LB","UB")) {
    ticker2 <- ticker2 + 1
    
    ## keep just the relevant data for a specific scenario
    data <- save[save$int_eff_uncert==e & save$pop_burden_uncert==s,]
    
    ## collapse the dataset so that each intervention is one row
    ## for maternal -- want just overall cost of SBA, BEmOC, CEmOC with added deaths etc. from 3.1-3.3 and 4.1-4.2
    mat <- data[data$int_n %in% c(3.1,3.2,3.3,4.1,4.2),]
    mat$int_n[mat$int_n %in% c(3.1,3.2,3.3)] <- 3
    mat$int_n[mat$int_n %in% c(4.1,4.2)] <- 4
    i.3.4 <- aggregate(cbind(delta.deaths, delta.deaths.npv, E) ~ int_n,
                       FUN = sum,
                       data = mat)
    
    
    # combine maternal and neonatal for BEmOC and CEmOC
    data$delta.deaths[data$int_n==3] <- data$delta.deaths[data$int_n==3] + i.3.4$delta.deaths[i.3.4$int_n==3]
    data$delta.deaths[data$int_n==4] <- data$delta.deaths[data$int_n==4] + i.3.4$delta.deaths[i.3.4$int_n==4]
    
    data$delta.deaths.npv[data$int_n==3] <- data$delta.deaths.npv[data$int_n==3] + i.3.4$delta.deaths.npv[i.3.4$int_n==3]
    data$delta.deaths.npv[data$int_n==4] <- data$delta.deaths.npv[data$int_n==4] + i.3.4$delta.deaths.npv[i.3.4$int_n==4]
    
    data$E[data$int_n==3] <- (data$E[data$int_n==3] + i.3.4$E[i.3.4$int_n==3])/2
    data$E[data$int_n==4] <- (data$E[data$int_n==4] + i.3.4$E[i.3.4$int_n==4])/2
    
  
    
    ## save master data for interventions 3 and 4
    i.mat <- data[data$int_n %in% c(3,4),c("int_n","int_des","total_cost",
                                           "delta.deaths",
                                           "delta.deaths.npv",
                                           "E")]
    
    ## for disease targets -- want to add total costs (b/c specific target pops) + delta.deaths + che
    i.disease <- aggregate(cbind(total_cost,
                                 delta.deaths,
                                 delta.deaths.npv,
                                 E) ~ int_n + int_des,
                           FUN = sum ,data[data$int_target=="incidence" | data$int_target=="prevalence",], na.rm=T)
    
    ## get rid of BEmOC and CEmOC since we already have those from above
    i.disease <- i.disease[i.disease$int_n != 3.1 & i.disease$int_n != 3.2
                           & i.disease$int_n != 3.3 &
                             i.disease$int_n != 4.1 & i.disease$int_n != 4.2,]
    
    ## for population targets -- just want overall cost (don't double count) + additive delta.deaths + che across disease categories
    i.population <- aggregate(cbind(delta.deaths,
                                    delta.deaths.npv,
                                    E) ~ int_n + int_des,
                              FUN = sum,data[data$int_target=="population",], na.rm=T)
    
    i.pop.cost <- unique(data[data$int_target=="population",c("int_n","total_cost")]) # merging in total cost here to avoid double count
    i.population <- merge(i.population,i.pop.cost,by=c("int_n"),all.x=T)
    
    ## get rid of BEmOC and CEmOC since we already have those from above
    i.population <- i.population[i.population$int_n != 3 & i.population$int_n != 4,]
    
    ## put all these files together
    data <- rbind(i.mat,i.disease,i.population)
    data <- data[order(data$int_n),]
    
    # check to make sure all the interventions are accounted for
    sort(check) == sort(data$int_n)
    
    ## add variables to track uncertainty scenario
    data$int_eff_uncert <- e
    data$pop_burden_uncert <- s
    
    ## save file with all vars
    if(ticker2==1) {
      save2 <- data
    } else {
      save2 <- rbind(save2,data)
    }
  } # closes the file aggregation loop for pop/burden uncertainty
} # closes the aggregation loop for int effect uncertainty

## save file with key vars
write.table(save2,paste0("./03_output_data/03_equity.csv"),sep=",",row.names=F)



