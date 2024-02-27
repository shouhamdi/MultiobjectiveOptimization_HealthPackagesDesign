## Date:    7/13/2023
## Purpose: Calculate deaths and CHE averted for upstream interventions with downstream interventions

## set up
  rm(list=ls())
  date <- Sys.Date()

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
  hh.incomes <- read.table("./03_output_data/income_draws.csv",sep=",",header=T)
  names(hh.incomes) <- "hh.incomes"

  hh.incomes<- hh.incomes[order(hh.incomes$hh.incomes), , drop = FALSE]
  
  total.draws_truncated = floor(9/10*total.draws)
  q_len <- floor(total.draws_truncated/5)
  names(hh.incomes) <- c("incomes")
  
  # Calculate the quintile of each income in hh.incomes
  hh.incomes$quintile <- cut(hh.incomes$incomes, quantile(hh.incomes$incomes, probs = 0:5/5), labels = FALSE)
  
  
  ## bring in utilization per quintile
  util_proxys <- read.csv("./01_input_data/03_util_proxys.csv",sep = ";", header=T,stringsAsFactors=F)
  
  util_proxys <- util_proxys %>%
    mutate(util = as.numeric(gsub(",", ".", util)))
  
## bring in input parameter master file
  ## bring in temporal spacing to map intervention information
  print("USING FILE:")
  print(file)
  
  map <- read.xls(file,sheet="Temporal Spacing",stringsAsFactors=F)
  names(map)[1] <- "gbd_n"
  names(map)[2] <- "gbd_des"
  
  ## for this, keep int=1 with int 2's
  map <- map[,c("gbd_n","gbd_des","int_1_n","int_2_n")]
  map <- map[!is.na(map$int_2_n),]
  
  ## create a list of all the interventions we should have at end of code for error checking
  check <- unique(map$int_1_n)
  check[which(check==3.2)] <- 3
  
  ## bring in input parameter master file
  data <- read.csv("./01_input_data/02_final_input_parameters.csv",header=T,stringsAsFactors=F)
  
  ## change data variables for downstream interventions
  data.down <- data[,c("int_n","int_target","int_eff_target",
                       "target_n","target_n_LB","target_n_UB",
                       "population","baseline_cov",
                       "int_eff","int_eff_LB","int_eff_UB",
                       "gbd_n","burden_amenable",
                       "unit.cost","oop.cost","time_lag","screen","p.treat","uc.screen","uc.treat")]
  names(data.down) <- c("int_2_n","int_2_target","int_2_eff_target",
                        "int_2_target_n","int_2_target_n_LB","int_2_target_n_UB",
                        "int_2_population","int_2_baseline_cov",
                        "int_2_eff","int_2_eff_LB","int_2_eff_UB",
                        "gbd_n","int_2_burden_amenable",
                        "int_2_unit.cost","int_2_oop.cost","int_2_time_lag")
  
## merge intervention info onto mapping file
  data <- merge(map,data,by.x=c("gbd_n","int_1_n"),by.y=c("gbd_n","int_n"),all.x=T)
  data <- merge(data,data.down,by=c("gbd_n","int_2_n"),all.x=T)
  
# create a variable for OOP cost as a percent
  data$oop.cost.per <- data$oop.cost
  data$oop.cost <- NA 
  data$int_2_oop.cost.per <- data$int_2_oop.cost
  data$int_2_oop.cost <- NA
  
# keep a master file to pull variables from
  master <- data

############################################
## Loop over average, LB and UB scenarios ##
############################################
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
    
    ## calculate the population previously seeking care -- that are now covered (i.e. the baseline coverage pop)
        data$baseline_coverage_pop <- NA
        ## 1 - when intervention target is incidence or prevalence -- include burden addressable variable
        data$baseline_coverage_pop[data$int_target != "population"] <- data$baseline_cov[data$int_target != "population"]  *
                                                                        target_n[data$int_target != "population"] *
                                                                        data$burden_amenable[data$int_target != "population"] 
        
        ## 2 - when intervention target is population -- goes to everyone -- don't want addressable %
        data$baseline_coverage_pop[data$int_target == "population"] <- data$baseline_cov[data$int_target == "population"]  *
                                                                        target_n[data$int_target == "population"] 
        
    ## calculate the total cost of scaling up coverage
    ## whole unit cost for newly covered, just OOP for baseline coverage
    data$total_cost <- (data$new_tx_pop*data$unit.cost) + (data$baseline_coverage_pop*data$oop.cost.per*data$unit.cost)
    
    ## covert OOP to dollars instead of %
    data$oop.cost <- data$oop.cost.per*data$unit.cost
    data$int_2_oop.cost <- data$int_2_oop.cost.per*data$int_2_unit.cost
    
    ## covert OOP for screen/treat costs to dollars instead of %
    data$oop.cost.screen <- data$oop.cost.per*data$uc.screen
    data$oop.cost.treat <- data$oop.cost.per*(data$uc.treat + data$uc.screen) # both treated and screened
    
    #################################
    ## Deaths averted per $ spent  ##
    ################################# 
    ## deaths averted just for newly covered population
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
    
    
    ## calculate the change in incidence using change in mortality
    data$new.incidence <- NA
    data$new.incidence[m] <- (deaths[m]-data$delta.deaths[m])/CFR[m]
    data$delta.incidence[m] <- incidence[m] - data$new.incidence[m]
    
    #############################
    ## CHE averted per $ spent ##
    #############################
    # CHE averted for both newly and baseline covered populations
    data$che.10<-NA
    data$che.25<-NA
    
    ## adjust the changes in incidence to the amenable percent of disease and intervention coverage
    ## for a downstream intervention (int_2, any change is dependent on the % of that incidence amenable and baseline coverage)
    data$int_2_delta.incidence <- data$delta.incidence*data$int_2_burden_amenable*data$int_2_baseline_cov 
    
    ## calculate the direct FRP from changes in the OOP expenditures of preventative care
    temp <- data.frame(gbd_n = double(),
                       int_n = double(),
                       che.10 = double(),
                       che.25 = double(),
                       seed = double())
    
    
    
    ## average over several runs
    n_seed <-2
    n_iter <- n_seed*dim(data)[1]# Number of iterations of the loop
    
    # Initializes the progress bar
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = n_iter, # Maximum value of the progress bar
                         style = 3,    # Progress bar style (also available style = 1 and style = 2)
                         width = 50,   # Progress bar width. Defaults to getOption("width")
                         char = "=")   # Character used to create the bar
    
    for(n in 1:n_seed){
      set.seed(n)
      
      if (n==1){
        # Create a PDF device
        #pdf("CHE_index_plots.pdf")
        
        # Create a PNG device and plot the data
        #png("plot.png", width = 800, height = 600)
      }
      
      for(r in 1:dim(data)[1]) {
        intervention <- data$int_1_n[r]
        
        hh.incomes$prob <-NA
        
        util_proxys_subset<- subset(util_proxys, int_n == intervention, select = c( "util", "quintile"))
        # Identify duplicated rows
        duplicated_rows <- duplicated(util_proxys_subset)
        # Remove duplicated rows
        util_proxys_subset_no_duplicates <- util_proxys_subset[!duplicated_rows, ]
        # View the resulting data frame
        #print(length(util_proxys_subset_no_duplicates))
        merged <- merge(hh.incomes, util_proxys_subset_no_duplicates, by = "quintile", all.x = T)
        #print(length(merged))
        hh.incomes$prob <-merged$util
        
        len <- length(hh.incomes$prob)
        #print(len)
        hh.incomes$prob[len] <- hh.incomes$prob[len-1]
        I <- seq(1, len)
        
        ## two calcs for screen/treat interventions
        
        if(data$screen[r] == 1) {
          ## draw subset of incomes for both screen and treat
          n_screen <- round((data$new_tx_pop[r] + data$baseline_coverage_pop[r])*(1-data$p.treat[r]))
          n_treat <- round((data$new_tx_pop[r] + data$baseline_coverage_pop[r])*(data$p.treat[r]))
          i.screen <- sample(I, n_screen, replace = TRUE, prob = hh.incomes$prob)
          tx.incomes.screen <- hh.incomes$incomes[i.screen]
          i.treat <- sample(I, n_treat, replace = TRUE, prob = hh.incomes$prob)
          tx.incomes.treat <- hh.incomes$incomes[i.treat]
          
          ## calculate the percent of income OOP represents
          i.per.screen <- data$oop.cost.screen[r]/tx.incomes.screen
          i.per.treat <- data$oop.cost.treat[r]/tx.incomes.treat
          n_target <- n_screen+n_treat
          i.per <- c(i.per.screen, i.per.treat)
          
        } else {
          ## one calc for most interventions
          ## draw subset of incomes
          n_target <- round((data$new_tx_pop[r] + data$baseline_coverage_pop[r]))
          i.target <- sample(I, n_target, replace = TRUE, prob = hh.incomes$prob)
          tx.incomes<- hh.incomes$incomes[i.target]
          
          
          ## calculate the percent of income OOP represents
          i.per <- data$oop.cost[r]/tx.incomes
        }
        
        ## add downstream intervention
        intervention_2 <- data$int_2_n[r]
        hh.incomes$prob_2 <-NA
        
        util_proxys_subset_2<- subset(util_proxys, int_n == intervention_2, select = c( "util", "quintile"))
        # Identify duplicated rows
        duplicated_rows_2<- duplicated(util_proxys_subset_2)
        # Remove duplicated rows
        util_proxys_subset_no_duplicates_2 <- util_proxys_subset_2[!duplicated_rows_2, ]
        merged_2<- merge(hh.incomes, util_proxys_subset_no_duplicates_2, by = "quintile", all.x = T)
        #print(length(merged))
        hh.incomes$prob_2 <-merged_2$util
        
        len_2 <- length(hh.incomes$prob_2)
        hh.incomes$prob_2[len_2] <- hh.incomes$prob_2[len_2-1]
        
        # draw subset of incomes
        n_target_2 <- round((data$int_2_delta.incidence[r]))
        i.target_2 <- sample(I, n_target_2, replace = TRUE, prob = hh.incomes$prob_2)
        tx.incomes_2<- hh.incomes$incomes[i.target_2]
        
        
        ## calculate the percent of income OOP represents
        i.per_2 <- data$int_2_oop.cost[r]/tx.incomes_2
        
        ##add these additional cases 
        i.per <- c(i.per, i.per_2)
        
        
        
        # Set the number of rows and columns for plot arrangement
        #nrows <- 2  # Number of rows
        #ncols <- 3  # Number of columns
        
        
        ## generate CHE index at all cutoffs
        che.index <- vector()
        
        index <- 0
        for(c in cutoffs) {
          index <- index + 1
          
          ### Calculate the empirical cumulative distribution function (CDF)
          cdf <- ecdf(i.per)
          
          # int_des <- data[r, "int_des"]
          # gbd_des <- data[r, "gbd_des"]  
          # # Plot the CDF
          # par(mfrow = c(nrows, ncols), mar = c(4, 4, 2, 1))  # Set the layout and margins
          # plot(cdf, col = "blue",
          #      xlab = "Percent of Income OOP", ylab = "Cumulative Probability",
          #      main = paste("CDF of OOP/income", gbd_des, "using", int_des))
          # 
          # # Add gridlines
          # grid()
          # 
          # define the sigmoid function
          t <- c/2  # Threshold/2
          a <- 0.01  # Parameter a
          
          sigmoid <- function(x) {
            1 / (1 + exp(-(x - t) / a))
          }
          
          # Generate x values for the sigmoid function
          x_sigmoid <- seq(min(i.per), max(i.per), length.out = 1000)
          
          # Plot the sigmoid function
          #lines(x_sigmoid, sigmoid(x_sigmoid), col = "red")
          
          # Calculate area between the two plots
          f <- function(x) {abs(sigmoid(x)*(1 - cdf(x))) }
          che <- integrate(f, 0, max(i.per) ,subdivisions = 5000)$value
          #normalize <- integrate(function(x) abs(sigmoid(x)), 0, max(i.per))$value
          normalize <- 1
          
          che.index[index] <- che/normalize
          
        }
        
        ## Store the che.10 and che.25 values in separate vectors
        che.10_value <- che.index[1]*n_target/total.draws_truncated
        che.25_value <- che.index[2]*n_target/total.draws_truncated
        
        ## Append the values to the temp data frame
        temp <- rbind(temp, data.frame(gbd_n = data$gbd_n[r], int_n = data$int_1_n[r], che.10 = che.10_value, che.25 = che.25_value, seed = n))
        
        
        ##Progress bar
        setTxtProgressBar(pb, r+ n*dim(data)[1])
        
        hh.incomes$prob <-NULL
      }  # closes row loop
      
      if (n==1){
        
        # Close the PDF device and save the plots
        #dev.off()
        
        # Close the PNG device and save the plot
        #dev.off()
      }
      
    } # closes the seed loop
    
    close(pb) # Close the connection
    
   
    ## Average the different runs for che.10 and che.25
    averaged_temp <- temp %>%
      group_by(gbd_n, int_n) %>%
      summarize(che.10 = mean(che.10),
                che.25 = mean(che.25))
    
    ### Assign the averaged values back to the original data frame
    #temp_merged <- merge(data, averaged_temp, by = c("gbd_n"), all.x = TRUE)
    
    data$che.10<-averaged_temp$che.10
    data$che.25<-averaged_temp$che.25
    
    
    
    
    
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
  } # closes pop/burden uncertainty loop 
} # closes int eff uncertainty loop
  
## save file with all vars
  write.table(save,paste0("./03_output_data/03_LINKED_all_vars_index.csv"),sep=",",row.names=F)

###### aggregate the results for each file ##########
ticker2 <- 0
  
for(e in c("MEAN","LB","UB")) {
  for(s in c("MEAN","LB","UB")) {
    ticker2 <- ticker2 + 1
    
    ## keep just the relevant data for a specific scenario
    data <- save[save$int_eff_uncert==e & save$pop_burden_uncert==s,]
    
    ## rename int_1_n to int_n
    names(data)[names(data)=="int_1_n"] <- "int_n"
    
    ## collapse the dataset so that each intervention is one row
    ## for maternal -- want just overall cost of SBA, BEmOC, CEmOC with added deaths etc. from 3.1-3.3 and 4.1-4.2
    mat <- data[data$int_n %in% c(3.1,3.2,3.3,4.1,4.2),]
    mat$int_n[mat$int_n %in% c(3.1,3.2,3.3)] <- 3
    mat$int_n[mat$int_n %in% c(4.1,4.2)] <- 4
    i.3.4 <- aggregate(cbind(delta.deaths,delta.deaths.npv,
                             che.10,che.25) ~ int_n, FUN = sum, data=mat)
    #print(i.3.4)
    
    ## only one intervention -- isolate to merge back in -- make cost = 0
    i.3.4$total_cost <- 0
    i.3.4$int_des <- "Basic emergency obstetric care"
    
    ## save master data for interventions 3 and 4
    i.mat <- i.3.4[i.3.4$int_n %in% c(3,4),c("int_n","int_des","delta.deaths","delta.deaths.npv",
                                             "total_cost",
                                             "che.10",
                                             "che.25")]
    #print(i.mat)
    ## for population targets -- just want overall cost (don't double count) + additive delta.deaths + che across disease categories
    i.population <- aggregate(cbind(delta.deaths,delta.deaths.npv,
                                    che.10,
                                    che.25) ~ int_n + int_des, 
                              FUN = sum ,data[data$int_target=="population",], na.rm=T)
    #print(i.population)
    i.pop.cost <- unique(data[data$int_target=="population",c("int_n","total_cost")]) # bring in total cost so it's counted once
    #print(i.pop.cost)
    i.population <- merge(i.population,i.pop.cost,by=c("int_n"),all.x=T)
    #print(i.population)
    ## put files together
    data <- rbind(i.mat,i.population)
    #print(data)
    data <- data[order(data$int_n),]
    #print(data)
    # check to make sure all the interventions are accounted for
    check == data$int_n
    
    ## add variables to track uncertainty scenario
    data$int_eff_uncert <- e
    data$pop_burden_uncert <- s
    
    ## save file with all vars
    if(ticker2==1) {
      save2 <- data
    } else {
      save2 <- rbind(save2,data)
    }

  } # closes the aggregation loop
} # closes the loop on pop/burden uncertainty loop
  
## save file with deaths and che estimates
  write.table(save2,paste0("./03_output_data/03_LINKED_deaths_che_index.csv"),sep=",",row.names=F)

  
  


 