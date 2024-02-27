## Author:  Kate Lofgren (klofgren@g.harvard.edu)
## Date:    1/30/2019
## Purpose: Prep burden of disease files

## set up
  rm(list=ls())
  date <- Sys.Date()

## libraries
  library(foreign)

## set seed
  set.seed(02139)

## set directory
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  
## bring in all GBD data
  all.data <- read.csv("./01_input_data/00_raw_files/IHME-GBD_2017_MASTER.csv",header=T,stringsAsFactors=F)
  
####################
#### Maternal ######
####################
  data <- all.data
  
  ## all maternal conditions for 15-49 years
  data <- data[data$age_name=="15-49 years",] 
  
  # keep 4 causes of death relevant for our study
  data <- data[data$cause_name %in% c("Maternal disorders",
                                      "Maternal hemorrhage",
                                      "Maternal hypertensive disorders",
                                      "Maternal sepsis and other maternal infections",
                                      "Maternal obstructed labor and uterine rupture"),]
  
  # keep just the numbers, not percents or rates
  data <- data[data$metric_name == "Number",]
  
  # keep just females
  data <- data[data$sex_name=="Female",]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  data$age_name = "15-49"
  
  ## keep as a master file while the other target disease types are processed
  master <- data
  data <- NULL
  
####################
#### Neonatal ######
####################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Both",]
  
  # keep Neonatal and birth
  data <- data[data$age_name %in% c("Birth","Early Neonatal","Late Neonatal"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  ## keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Neonatal disorders",
                                      "Neonatal preterm birth",
                                      "Neonatal encephalopathy due to birth asphyxia and trauma",
                                      "Neonatal sepsis and other neonatal infections"),]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  # sum numbers across agegroups
  data$neonatal <- 0
  data$neonatal[data$age_name %in% c("Early Neonatal","Late Neonatal")] <- 1
  val <- aggregate(val ~ measure_name + cause_name + cause_id + neonatal, FUN=sum, data=data)
  lower <- aggregate(lower ~ measure_name + cause_name + cause_id + neonatal, FUN=sum, data=data)
  upper <- aggregate(upper ~ measure_name + cause_name + cause_id + neonatal, FUN=sum, data=data)
  data <- merge(val,lower,by=c("measure_name","cause_name","cause_id","neonatal"))
  data <- merge(data,upper,by=c("measure_name","cause_name","cause_id","neonatal"))
  data$age_name[data$neonatal==1] = "Neonatal"
  data$age_name[data$neonatal==0] = "Births"
  data$neonatal <- NULL
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL
  
##########################################
#### Childhood                      ######
##########################################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Both",]
  
  # keep under-5
  data <- data[data$age_name %in% c("Under 5"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  ## keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Diarrheal diseases",
                                      "Lower respiratory infections",
                                      "Pneumococcal meningitis",
                                      "Diphtheria",
                                      "H influenzae type B meningitis",
                                      "Acute hepatitis B",
                                      "Measles",
                                      "Meningococcal meningitis"),]
  
  # limit to the important variables
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL
  
##########################
#### TB & HIV       ######
##########################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Both",]
  
  # keep ages 15-69
  data <- data[data$age_name %in% c("15-49 years","50-69 years"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  ## keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Drug-susceptible tuberculosis",
                                      "HIV/AIDS"),]
  
  # sum numbers across agegroups
  val <- aggregate(val ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  lower <- aggregate(lower ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  upper <- aggregate(upper ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  data <- merge(val,lower,by=c("measure_name","cause_name","cause_id"))
  data <- merge(data,upper,by=c("measure_name","cause_name","cause_id"))
  data$age_name = "15-69"
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL

###########################################
#### Cardiovascular Disease        ########
###########################################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Both",]
  
  # keep adult ages over 30-69
  data <- data[data$age_name %in% c("50-69 years"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  ## keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Ischemic stroke",
                                      "Ischemic heart disease",
                                      "Hypertensive heart disease"),]
  
  # sum numbers across agegroups
  val <- aggregate(val ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  lower <- aggregate(lower ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  upper <- aggregate(upper ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  data <- merge(val,lower,by=c("measure_name","cause_name","cause_id"))
  data <- merge(data,upper,by=c("measure_name","cause_name","cause_id"))
  data$age_name = "50-69"
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL
  
###########################################
#### Diabetes                      ########
###########################################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Both",]
  
  # keep adult ages 30-69
  data <- data[data$age_name %in% c("50-69 years"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  ## keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Diabetes mellitus type 2"),]
  
  # sum numbers across agegroups
  val <- aggregate(val ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  lower <- aggregate(lower ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  upper <- aggregate(upper ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  data <- merge(val,lower,by=c("measure_name","cause_name","cause_id"))
  data <- merge(data,upper,by=c("measure_name","cause_name","cause_id"))
  data$age_name = "50-69"
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL
  
##########################################
#### Surgery Amenable Injuries    ########
##########################################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Both",]
  
  # keep 0-69
  data <- data[data$age_name %in% c("Under 5",
                                    "5-14 years",
                                    "15-49 years",
                                    "50-69 years"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  ## keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Transport injuries",
                                      "Falls",
                                      "Fire, heat, and hot substances",
                                      "Exposure to mechanical forces",
                                      "Foreign body",
                                      "Self-harm and interpersonal violence"),]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  # sum numbers across agegroups
  val <- aggregate(val ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  lower <- aggregate(lower ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  upper <- aggregate(upper ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  data <- merge(val,lower,by=c("measure_name","cause_name","cause_id"))
  data <- merge(data,upper,by=c("measure_name","cause_name","cause_id"))
  data$age_name = "0-69"
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL
  
##########################################
#### Cervical Cancer              ########
##########################################
  data <- all.data
  
  # keep both sexes
  data <- data[data$sex_name=="Female",]
  
  # keep adult ages over 20-69
  data <- data[data$age_name %in% c("20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                    "45 to 49","50-69 years"),]
  
  # keep number
  data <- data[data$metric_name == "Number",]
  
  # keep causes of death relevent to our study
  data <- data[data$cause_name %in% c("Cervical cancer"),]
  
  # limit to the important variable
  data <- data[,c("measure_name","cause_name","cause_id","age_name","val","upper","lower")]
  
  # sum numbers across agegroups
  val <- aggregate(val ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  lower <- aggregate(lower ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  upper <- aggregate(upper ~ measure_name + cause_name + cause_id, FUN=sum, data=data)
  data <- merge(val,lower,by=c("measure_name","cause_name","cause_id"))
  data <- merge(data,upper,by=c("measure_name","cause_name","cause_id"))
  data$age_name = "20-69"
  
  ## keep as a master file while the other target disease types are processed
  master <- rbind(master,data)
  data <- NULL
  
###############################
## write out master file ######
###############################
  write.csv(master,"./01_input_data/01_processed_GBD-2017.csv",row.names=F)

  
  
  
  
  