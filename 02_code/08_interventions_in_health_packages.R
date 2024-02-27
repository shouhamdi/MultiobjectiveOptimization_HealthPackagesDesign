## Date:    31/07/2023
## Purpose: determine the set of interventions used in each pareto efficient health package



## set up
rm(list=ls())
date <- Sys.Date()

## libraries
library(foreign)
library(dplyr)
library(lpSolve)
library(reshape2)
library(viridis)
library(ggplot2)
library(plotly)

## set seed
set.seed(02139)


## set directory, bring in universal parameters
setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
source("./02_code/00_universal_parameters.R")

##select the parameters 
epsilon.deaths<-10
epsilon.che<-200
epsilon.equity<-50
epsilon.b<-5


## bring in intervention data
data <- read.table(paste("./03_output_data/optimization_runs/interventions/05_optimized_interventions_full_frontier_with0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_",epsilon.b,".csv"), sep=",", header=T)
data <- data[,c("int_n","budget","k","k_bis","fund")]
data <- data[data$fund!=0,] 

package <- read.table(paste("./03_output_data/optimization_runs/package_without_0/05_optimized_package_counts_full_frontier_without0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_",epsilon.b,".csv"), sep=",", header=T)
package <- package[,c("budget","k","k_bis","package.deaths","package.che.10","package.E")]


health_package <- merge(package,data,by=c("budget","k","k_bis"),all.x=T)



package_counts <- package[,c("budget","package.deaths","package.che.10","package.E")]
package_counts <- unique(package_counts)
package_counts$p_counts <- 1
package_counts <-package_counts %>%
  group_by(budget) %>%
  summarize(p_counts = sum(p_counts))


data_counts <- data
data_counts$d_counts <- 1
data_counts <-data_counts %>%
  group_by(budget,k,k_bis) %>%
  summarize(d_counts = sum(d_counts))


health_package <- merge(health_package,package_counts,by="budget",all.x=T)
health_package <- merge(health_package,data_counts,by=c("budget","k","k_bis"),all.x=T)

health_package$packages_per_budget <- health_package$p_counts
health_package$interventions_per_package <- health_package$d_counts
health_package$d_counts<- NULL
health_package$p_counts<- NULL

health_package$k <- NULL
health_package$k_bis <- NULL
health_package$fund <- NULL
health_package <-unique(health_package)




###Same analysis for other budgets 

## bring in intervention data
data_bis <- read.table(paste("./03_output_data/optimization_runs/interventions/05_optimized_interventions_full_frontier_with0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_200to700M.csv"), sep=",", header=T)
data_bis <- data_bis[,c("int_n","budget","k","k_bis","fund")]
data_bis <- data_bis[data_bis$fund!=0,] 

package_bis <- read.table(paste("./03_output_data/optimization_runs/package_without_0/05_optimized_package_counts_full_frontier_without0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_200to700M.csv"), sep=",", header=T)
package_bis <- package_bis[,c("budget","k","k_bis","package.deaths","package.che.10","package.E")]


health_package_bis <- merge(package_bis,data_bis,by=c("budget","k","k_bis"),all.x=T)



package_counts_bis <- package_bis[,c("budget","package.deaths","package.che.10","package.E")]
package_counts_bis <- unique(package_counts_bis)
package_counts_bis$p_counts <- 1
package_counts_bis <-package_counts_bis %>%
  group_by(budget) %>%
  summarize(p_counts = sum(p_counts))


data_counts_bis <- data_bis
data_counts_bis$d_counts <- 1
data_counts_bis <-data_counts_bis %>%
  group_by(budget,k,k_bis) %>%
  summarize(d_counts = sum(d_counts))


health_package_bis <- merge(health_package_bis,package_counts_bis,by="budget",all.x=T)
health_package_bis <- merge(health_package_bis,data_counts_bis,by=c("budget","k","k_bis"),all.x=T)

health_package_bis$packages_per_budget <- health_package_bis$p_counts
health_package_bis$interventions_per_package <- health_package_bis$d_counts
health_package_bis$d_counts<- NULL
health_package_bis$p_counts<- NULL

health_package_bis$k <- NULL
health_package_bis$k_bis <- NULL
health_package_bis$fund <- NULL
health_package_bis <-unique(health_package_bis)


############################
#######SAVE FILES###########
############################

##Save into one file
health_package_bis <-rbind(health_package,health_package_bis)
write.table(health_package_bis,paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/details_packages_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_10to700M.csv"),sep=",",row.names=F)
write.table(health_package_bis,paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/details_packages_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_10to100M.csv"),sep=",",row.names=F)

##save file with only number of packages per budget 
save <- health_package[, c("budget", "packages_per_budget")]
save <- unique(save)
save <- save[order(save$budget),]
write.table(save, paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/packages_per_budget_eCHE_", epsilon.che, "_eE_", epsilon.equity, "_budget_10to700M.csv"), sep = ",", row.names = FALSE)

##Plot the number of packages according to the budget 
colors <- viridis(10)
png("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/number_of_packages_according_to_the_budget.png")
plot(save$budget, save$packages_per_budget, type = "l", col = colors[5], 
     xlab = "Budget", ylab = "Number of packages",
     main = "Number of packages according to the budget")

# Add points
points(save$budget, save$packages_per_budget, col = colors[5], pch = 16)

# Add gridlines
grid()
dev.off()


## save a file with the single objectives and distances between them
single <- data.frame()
budget <- health_package$budget
budget <- unique(budget)

for (b in budget) {
  temp <- health_package[health_package$budget == b, ]
  d.max <- max(temp$package.deaths) 
  che.max <- max(temp$package.che.10)
  E.max <- max(temp$package.E)
  temp <- temp[temp$package.deaths >= d.max | temp$package.che.10 >= che.max | temp$package.E >= E.max, ]
  temp$MAX_Health <- NA
  temp$MAX_FRP <- NA
  temp$MAX_Equity <- NA
  temp$MAX_Health[temp$package.deaths >= d.max] <-1
  temp$MAX_FRP[temp$package.che.10 >= che.max] <-1
  temp$MAX_Equity[temp$package.E >= E.max] <-1
  
  
  # Extract the coordinates of the three points: MAX Population health, MAX FRP and MAX Equity
  x_h <- unique(temp$package.deaths[temp$package.deaths >= d.max])
  y_h <- unique(temp$package.che.10[temp$package.deaths >= d.max])
  z_h <- unique(temp$package.E[temp$package.deaths >= d.max])
  
  x_c <- unique(temp$package.deaths[temp$package.che.10 >= che.max])
  y_c<- unique(temp$package.che.10[temp$package.che.10 >= che.max])
  z_c<- unique(temp$package.E[temp$package.che.10 >= che.max])
  
  x_e <- unique(temp$package.deaths[temp$package.E >= E.max])
  y_e<- unique(temp$package.che.10[temp$package.E >= E.max])
  z_e<- unique(temp$package.E[temp$package.E >= E.max])
  
  ## Calculate the Euclidean distance
  #Population Health - CHE
  h_c <- sqrt(((x_c - x_h)/d.max)^2 + ((y_c - y_h)/che.max)^2 + ((z_c - z_h)/E.max)^2)
  print(h_c)
  #Population Health - Equity
  h_e <- sqrt(((x_e - x_h)/d.max)^2 + ((y_e - y_h)/che.max)^2 + ((z_e - z_h)/E.max)^2)
  #CHE - Equity
  c_e <- sqrt(((x_e - x_c)/d.max)^2 + ((y_e - y_c)/che.max)^2 + ((z_e - z_c)/E.max)^2)
  
  temp$distance_max_Health_CHE <- h_c
  temp$distance_max_Health_Equity <- h_e
  temp$distance_max_CHE_Equity <- c_e

  single <- rbind(single, temp)
  single <-unique(single)
}

write.table(single,paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/details_packages_MAX_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_10to100M.csv"),sep=",",row.names=F)



##Plot the mean number of of interventions per packages according to the budget 

  int_per_pack <- health_package[,c("budget","package.deaths","package.che.10","package.E","interventions_per_package")]
  int_per_pack <- unique(int_per_pack)
  int_per_pack <- int_per_pack[,c("budget","interventions_per_package")]
  int_per_pack <- int_per_pack %>%
    group_by(budget) %>%
    summarize(interventions_per_package = mean(interventions_per_package))
  
  write.table(int_per_pack, paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/mean_int_per_pack_eCHE_", epsilon.che, "_eE_", epsilon.equity, "_budget_10to100M.csv"), sep = ",", row.names = FALSE)
  
  ##Plot the number of packages according to the budget 
  colors <- viridis(10)
  png("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/mean_int_per_packages_according_to_the_budget.png")
  plot(int_per_pack$budget, int_per_pack$interventions_per_package, type = "l", col = colors[5], 
       xlab = "Budget", ylab = "Mean number of interventions per package",
       main = "Mean number of interventions per package according to the budget")
  
  # Add points
  points(int_per_pack$budget, int_per_pack$interventions_per_package, col = colors[5], pch = 16)
  
  # Add gridlines
  grid()
  dev.off()
  

##Plot the distance between extrema according to the budget 
  
  distances <- single[,c("budget", "distance_max_Health_CHE","distance_max_Health_Equity", "distance_max_CHE_Equity")]
  distances <- distances[order(distances$budget),]
  ##Plot the distances according to the budget 
  colors <- viridis(10)
    
    ##Health_CHE
    png("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/Health_CHE_according_to_the_budget.png")
    plot(distances$budget, distances$distance_max_Health_CHE, type = "l", col = colors[6], 
         xlab = "Budget", ylab = "Distance_max_Health_CHE",
         main = "distance_max_Health_CHE according to the budget")
    # Add points
    points(distances$budget, distances$distance_max_Health_CHE, col = colors[6], pch = 16)
    # Add gridlines
    grid()
    dev.off()
    
    ##Health_Equity
    png("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/Health_Equity_according_to_the_budget.png")
    plot(distances$budget, distances$distance_max_Health_Equity, type = "l", col = colors[7], 
         xlab = "Budget", ylab = "Distance_max_Health_Equity",
         main = "distance_max_Health_Equity according to the budget")
    # Add points
    points(distances$budget, distances$distance_max_Health_Equity, col = colors[7], pch = 16)
    # Add gridlines
    grid()
    dev.off()

    ##CHE_Equity
    png("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/CHE_Equity_according_to_the_budget.png")
    plot(distances$budget, distances$distance_max_CHE_Equity, type = "l", col = colors[8], 
         xlab = "Budget", ylab = "Distance_max_CHE_Equity",
         main = "distance_max_CHE_Equity according to the budget")
    # Add points
    points(distances$budget, distances$distance_max_CHE_Equity, col = colors[8], pch = 16)
    # Add gridlines
    grid()
    dev.off()

  ###Plot extrema points on the same 3D Figure
    extrema <- single[,c("budget","package.deaths","package.che.10","package.E","MAX_Health","MAX_FRP","MAX_Equity")]
    extrema <- unique(extrema)
    colors <- viridis(10)
    colors2 <- plasma(10)
    
    # start plot
    png("./04_figures/All_Extrema_3D.png", height=8, width=11)
    
    # plot code
    par(mar=c(6.5,6.5,2.5,1), mfrow=c(1,1))
    plot_ly(data=extrema) %>%
      add_trace(x=extrema$package.deaths[extrema$MAX_Health==1], 
                y=extrema$package.che.10[extrema$MAX_Health==1], 
                z=extrema$package.E[extrema$MAX_Health==1], 
                type="scatter3d",
                mode = 'markers',
                marker = list(size = 4, color = colors2[1]),
                name = "MAX Health") %>%
      add_trace(x=extrema$package.deaths[extrema$MAX_FRP==1], 
                y=extrema$package.che.10[extrema$MAX_FRP==1], 
                z=extrema$package.E[extrema$MAX_FRP==1], 
                type="scatter3d",
                mode = 'markers',
                marker = list(size = 4, color = colors2[5]),
                name = "MAX FRP") %>%
      add_trace(x=extrema$package.deaths[extrema$MAX_Equity==1], 
                y=extrema$package.che.10[extrema$MAX_Equity==1], 
                z=extrema$package.E[extrema$MAX_Equity==1], 
                type="scatter3d",
                mode = 'markers',
                marker = list(size = 4, color = colors2[10]),
                name = "MAX Equity") %>%
      add_text(data=extrema, x=extrema$package.deaths[extrema$MAX_Health==1]+500,
               y=extrema$package.che.10[extrema$MAX_Health==1]+0.01,
               z=extrema$package.E[extrema$MAX_Health==1]+0.015,
               text=paste(extrema$budget), 
               textfont = list(family = "serif"), showlegend = TRUE, xanchor = "right", color = colors2[1], name = "MAX Health Budget") %>%
      add_text(data=extrema, x=extrema$package.deaths[extrema$MAX_FRP==1]+500,
               y=extrema$package.che.10[extrema$MAX_FRP==1]+0.01,
               z=extrema$package.E[extrema$MAX_FRP==1]+0.015,
               text=paste(extrema$budget),
               textfont = list(family = "serif"), showlegend = TRUE, xanchor = "right", color = colors2[5], name = "MAX FRP Budget") %>%
      add_text(data=extrema, x=extrema$package.deaths[extrema$MAX_Equity==1]+500,
               y=extrema$package.che.10[extrema$MAX_Equity==1]+0.01,
               z=extrema$package.E[extrema$MAX_Equity==1]+0.015,
               text=paste(extrema$budget), 
               textfont = list(family = "serif"), showlegend = TRUE, xanchor = "right",color = colors2[10], name = "MAX Equity Budget") %>%
      layout(scene = list(xaxis = list(title = "Deaths Averted"),
                          yaxis = list(title = "CHE Averted"),
                          zaxis = list(title = "Equity")),
             title = "Single Optimal solutions for eCHE 200 and eE 50",
             titlefont = list(size = 12, color = "black"),
             showlegend = TRUE)  # Set showlegend to TRUE to display the legends
    
    # stop plot
    dev.off()
    
    
    
####List the interventions in the extrema packages 
    list_int <- single[,c("budget","int_n", "MAX_Health","MAX_FRP","MAX_Equity")]
    list_int <- list_int[order(list_int$budget,list_int$int_n),]
    grouped_data <- list_int %>%
      group_by(budget, MAX_Health, MAX_FRP, MAX_Equity) %>%
      summarize(int_n_list = toString(as.character(int_n))) %>%
      mutate(no_int_n_list = toString(setdiff(1:20, as.numeric(strsplit(int_n_list, ", ")[[1]]))))
    write.table(grouped_data, paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/list_int_MAX_eCHE_", epsilon.che, "_eE_", epsilon.equity, "_budget_10to100M.csv"), sep = ",", row.names = FALSE)
    

####List the interventions in all the packages 
    list_int_all <- health_package[,c("budget","package.deaths","package.che.10","package.E","int_n")]
    list_int_all <- list_int_all[order(list_int_all$budget,list_int_all$int_n),]
    grouped_data_all <- list_int_all %>%
      group_by(budget, package.deaths,package.che.10,package.E) %>%
      summarize(int_n_list = toString(as.character(int_n))) %>%
      mutate(no_int_n_list = toString(setdiff(1:20, as.numeric(strsplit(int_n_list, ", ")[[1]]))))
    write.table(grouped_data_all, paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/list_int_all_eCHE_", epsilon.che, "_eE_", epsilon.equity, "_budget_10to100M.csv"), sep = ",", row.names = FALSE)
    
    
    
#### Plot the interventions used in extrema packages according to budget
    
    list_int_plot <- single[,c("budget","int_n", "MAX_Health","MAX_FRP","MAX_Equity")]
    list_int_plot <- list_int_plot[order(list_int_plot$budget),]
    ## Plot the list_int_plot according to the budget 
    colors <- viridis(10)
    # Create the plot
    png("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/list_int_MAX_according_to_the_budget.png")
    plot(list_int_plot$budget-2000000, list_int_plot$int_n[list_int_plot$MAX_Health==1], col = colors[1], 
         xlab = "Budget", ylab = "int_n",
         main = "Interventions in single objectives packages according to the budget",
         xlim = c(0,110000000))
    # Add MAX FRP points
    points(list_int_plot$budget, list_int_plot$int_n[list_int_plot$MAX_FRP==1], col = colors[5], pch = 16)
    # Add MAX Equity points
    points(list_int_plot$budget+2000000, list_int_plot$int_n[list_int_plot$MAX_Equity==1], col = colors[10], pch = 16)
    # Add gridlines
    grid()
    # Add legend
    legend("topright", legend = c("MAX_Health", "MAX_FRP", "MAX_Equity"), col = c(colors[1], colors[5], colors[10]), pch = c(1, 16, 16))
    dev.off()
    
  

    
## calculate the total cost of financing all the interventions
all_data <- read.table("./03_output_data/04_combined_decision_vars.csv", sep=",", header=T)
total_cost <- sum(all_data$total_cost)
print(total_cost)
    
## Must-do and do not do treatments
for (b in budget) {
  png(paste("./03_output_data/optimization_runs/08_interventions_in_health_packages_outputs/Histogram_of_used_interventions_with_budget_", b, ".png"),
      width = 800,      # Adjust the width of the PNG file
      height = 600)     # Adjust the height of the PNG file
  temp <- health_package[health_package$budget == b, ]
  int <- temp$int_n
  hist <- hist(int,
               breaks = 20,
               main = "",        # Set the default title to an empty string
               xlab = "",        # Suppress the x-axis label
               ylab = "",        # Suppress the y-axis label
               col = "blue",     # Bar color
               border = "black", # Border color
               axes = FALSE      # Suppress default title and axes
               )
  plot(hist)
  title(main = paste("Histogram of used interventions with budget ", b/1000000,"M"), 
        xlab = "Int_n", 
        ylab = "Frequency", 
        col = colors2[1])
  dev.off()
}




