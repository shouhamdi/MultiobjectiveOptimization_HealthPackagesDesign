## Author:  Kate Lofgren (klofgren@g.harvard.edu)
## Date:    1/30/2019
## Purpose: Prep the main input file

## set up
  rm(list=ls())
  date <- Sys.Date()

## libraries
  library(foreign)
  library(gdata)

## set seed
  set.seed(02139)

## set directory
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  source("./02_code/00_universal_parameters.R")
  
###### Pull File #########
  print("USING FILE:")
  print(file)
  
## GBD processed
  gbd <- read.csv("./01_input_data/01_processed_GBD-2017.csv",header=T,stringsAsFactors=F)
  
## UNPOP
  pop <- read.csv("./01_input_data/01_processed_UNPOP.csv",header=T,stringsAsFactors=F)
  
## Intervention Main Info
  data <- read.xls(file,sheet="Interventions - Basics")
  data$X <- NULL
  names(data) <- c("WHO","int_n","int_des","int_des_short","int_target")
  
################################################################ 
#### Intervention Efficacy                                  ####
################################################################ 
  eff <- read.xls(file,sheet="Interventions - Efficacy",stringsAsFactors=F)
  eff <- eff[,c("Intervention..","GBD.Disease.Category..",
                "Final","Effect.Target","LB.Final","UB.Final")]
  names(eff) <- c("int_n","gbd_n","int_eff","int_eff_target","int_eff_LB","int_eff_UB")
  
  ## merge with main data file -- want the dimensions to remain at eff level
  before <- dim(eff)
  data <- merge(data,eff,by=c("int_n"),all.y=T)
  before[1] == dim(data)[1] # TRUE
  
################################################################ 
#### POPULATION & BURDEN                                    ####
################################################################ 
  temp <- read.xls(file,sheet="Target Pop",stringsAsFactors=F)
  temp$X <- temp$X.1 <- temp$X.2 <- temp$X.3 <- NULL
  names(temp) <- c("int_n","int_des","gbd_n",
                   "target_type","target_pop",
                   "target_age","target_sex")
  temp <- temp[,c("int_n","target_type","target_age","target_sex")]
  temp <- unique(temp)
  
  ## modify variable names to work with pop merge
  temp$target_sex[temp$target_sex=="All"] <- "both"
  temp$target_sex[temp$target_sex=="females"] <- "female"
  temp$target_sex[temp$target_sex=="males"] <- "male"
  
  ## merge in target age and sex
  dim.before <- dim(data)
  data <- merge(data,temp,by=c("int_n"),all.x=T)
  dim.before[1] == dim(data)[1] # TRUE
  
  ## merge the population #'s 
  dim.before <- dim(data)
  data <- merge(data,pop,by.x=c("target_age","target_sex"),by.y=c("age","sex"),all.x=T)
  dim.before[1] == dim(data)[1] # TRUE

################################################################ 
#### BASELINE COVERAGE                                      ####
################################################################  
  cov <- read.xls(file,sheet="Interventions - Coverage",stringsAsFactors=F)
  names(cov) <- c("who","int_n","int_des","who_cov","baseline_cov")
  cov <- cov[,2:5]
  cov <- cov[,c("int_n","baseline_cov")]
  
  ## merge with the GBD data
  before <- dim(data)
  data <- merge(data,cov,by=c("int_n"))
  before[1]==dim(data)[1] # TRUE
  
################################################################ 
#### Unit Cost -- for the Government                        ####
################################################################ 
  u.cost <- read.xls(file,sheet="Interventions - Unit Cost",stringsAsFactors=F)
  u.cost <- u.cost[,c("Intervention..","Intervention.Unit.Cost..2016.USD.")]
  names(u.cost) <- c("int_n","unit.cost")
  
  ## merge with the GBD data
  before <- dim(data)
  data <- merge(data,u.cost,by=c("int_n"),all.x=T)
  #before[1]==dim(data)[1] -- won't be true b/c unit costs just has 3/4 not sub interventions
  
  # add in unit cost of 3/4 subsections
  data$unit.cost[data$int_n %in% c(3.1,3.2,3.3)] <- data$unit.cost[data$int_n==3]
  data$unit.cost[data$int_n %in% c(4.1,4.2)] <- data$unit.cost[data$int_n==4]
  
################################################################ 
#### OOP  Cost                                              ####
################################################################ 
  oop.cost <- read.xls(file,sheet="Interventions - OOP Costs",stringsAsFactors=F)
  oop.cost <- oop.cost[,c("Intervention..","OOP.HH.expenditure..")]
  names(oop.cost) <- c("int_n","oop.cost")
  
  ## merge with the GBD data
  before <- dim(data)
  data <- merge(data,oop.cost,by=c("int_n"),all.x=T)
  before[1]==dim(data)[1] 
  
  # add in unit cost of 3/4 subsections
  data$oop.cost[data$int_n %in% c(3.1,3.2,3.3)] <- data$oop.cost[data$int_n==3]
  data$oop.cost[data$int_n %in% c(4.1,4.2)] <- data$oop.cost[data$int_n==4]
  
################################################################ 
#### GBD Amenable  & Disease Burden group                   ####
################################################################ 
  am <- read.xls(file,sheet="Disease Burden",stringsAsFactors=F)
  am <- am[,2:9]
  names(am) <- c("int_n","int_des","gbd_n","gbd_id","gbd_des","burden_age","time_lag","burden_amenable")

  ## merge with main file
  before <- dim(data)
  data <- merge(data,am,by=c("int_n","int_des","gbd_n"),all.x=T)
  before[1]==dim(data)[1] 
  
################################################################ 
#### GBD Deaths Data                                        ####
################################################################ 
  # standardize names for merge
  names(gbd)[3:4] <- c("gbd_id","burden_age")
  gbd$burden_age[gbd$burden_age=="Neonatal"] <- "0-28 days"
  gbd$burden_age[gbd$burden_age=="Under 5"] <- "Under-5"
  gbd$burden_age[gbd$burden_age=="15-49 years"] <- "15-49"
  gbd$burden_age[gbd$burden_age=="15-69 years"] <- "15-69"
  
  ## merge in deaths data
  before <- dim(data)
  data <- merge(data,gbd[gbd$measure_name=="Deaths",c("gbd_id","burden_age","val","upper","lower")],
                by=c("gbd_id","burden_age"),all.x=T)
  names(data)[names(data) == "val"] <- "deaths"
  names(data)[names(data) == "upper"] <- "deaths_UB"
  names(data)[names(data) == "lower"] <- "deaths_LB"
  before[1]==dim(data)[1] 
  
  ## merge in incidence data
  before <- dim(data)
  data <- merge(data,gbd[gbd$measure_name=="Incidence",c("gbd_id","burden_age","val","upper","lower")],
                by=c("gbd_id","burden_age"),all.x=T)
  names(data)[names(data) == "val"] <- "incidence"
  names(data)[names(data) == "upper"] <- "incidence_UB"
  names(data)[names(data) == "lower"] <- "incidence_LB"
  before[1]==dim(data)[1] 
  
  ## merge in prevalence data
  before <- dim(data)
  data <- merge(data,gbd[gbd$measure_name=="Prevalence",c("gbd_id","burden_age","val","upper","lower")],
                by=c("gbd_id","burden_age"),all.x=T)
  names(data)[names(data) == "val"] <- "prevalence"
  names(data)[names(data) == "upper"] <- "prevalence_UB"
  names(data)[names(data) == "lower"] <- "prevalence_LB"
  before[1]==dim(data)[1] 
  
  # generate CFR variable(s)
  data$CFR <- data$deaths/data$incidence
  data$CFR_LB <- data$deaths_LB/data$incidence_LB
  data$CFR_UB <- data$deaths_UB/data$incidence_UB
  
  ## for neonates incidence = 0, CFR is INF, change to NA, don't need
  data$CFR[data$incidence==0] <- NA
  data$CFR_LB[data$incidence==0] <- NA
  data$CFR_UB[data$incidence==0] <- NA
  
################################################################ 
#### Screen and Treat Interventions                         ####
################################################################ 
## although there are 5 screen, then treat interventions only 3 assume pop screening with conditionally expensive treatment
## others assume more targeted screening of positive pop
## only need to deal with pop targets, with conditional screen

  ## first ID the screen, then treat interventions
  data$screen <- 0
  data$screen[data$int_des_short %in% 
                c("DVI screen and treatment","Hypertension screen and control","Diabetes screen and control")] <- 1
  
  ## bring in the percent breakdown of screen vs. treat
  p.screen <- read.xls(file,sheet="Interventions - Screening",stringsAsFactors=F)
  p.screen <- p.screen[,1:3]
  names(p.screen) <- c("int_n","int_des","p.treat")
  # drop the interventions with p.treat == 1, those we don't need to treat specially
  p.screen <- p.screen[p.screen$p.treat!=1,]
  ## merge in with main dataset
  before <- dim(data)
  data <- merge(data,p.screen,
                by=c("int_n","int_des"),all.x=T)
  before[1]==dim(data)[1] # TRUE
  
  ## bring in the unit costs for screen pop vs. treat pop
  p.uc <- read.xls(file,sheet="Interventions - UC Screen",stringsAsFactors=F)
  p.uc <- p.uc[,c("Intervention..","Intervention","Final.Screen","Final.Treat"),]
  names(p.uc) <- c("int_n","int_des","uc.screen","uc.treat")
  ## merge in with main dataset
  before <- dim(data)
  data <- merge(data,p.uc,
                by=c("int_n","int_des"),all.x=T)
  before[1]==dim(data)[1] # TRUE
  
#########################################################################
#### STANDARDIZE & SAVE                                              ####
#########################################################################
  data <- data[order(data$int_n),]
  
  ## create a target_n variable that is specific to the int_target value
  data$target_n[data$int_target=="population"] <- data$population[data$int_target=="population"]
  data$target_n[data$int_target=="incidence"] <- data$incidence[data$int_target=="incidence"]
  data$target_n[data$int_target=="prevalence"] <- data$prevalence[data$int_target=="prevalence"]
  
  data$target_n_LB[data$int_target=="population"] <- data$population[data$int_target=="population"] # same
  data$target_n_LB[data$int_target=="incidence"] <- data$incidence_LB[data$int_target=="incidence"]
  data$target_n_LB[data$int_target=="prevalence"] <- data$prevalence_LB[data$int_target=="prevalence"]
  
  data$target_n_UB[data$int_target=="population"] <- data$population[data$int_target=="population"] # same
  data$target_n_UB[data$int_target=="incidence"] <- data$incidence_UB[data$int_target=="incidence"]
  data$target_n_UB[data$int_target=="prevalence"] <- data$prevalence_UB[data$int_target=="prevalence"]
  
  ## order
  data <- data[,c("int_n","int_des","int_target","target_n","target_n_LB","target_n_UB",
                  "int_eff_target","target_age","target_sex",
                  "population","baseline_cov","int_eff",
                  "int_eff_LB","int_eff_UB",
                  "gbd_n","burden_age","burden_amenable","deaths",
                  "unit.cost","oop.cost","deaths_LB","deaths_UB",
                  "incidence","incidence_LB","incidence_UB",
                  "prevalence","prevalence_LB","prevalence_UB","CFR","CFR_LB","CFR_UB","time_lag",
                  "screen","p.treat","uc.screen","uc.treat")]

  ## write out master file
  write.csv(data,"./01_input_data/02_final_input_parameters.csv",row.names=F)

  
  