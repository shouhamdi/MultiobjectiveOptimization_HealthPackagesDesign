
## Date:    24/07/2023
## Purpose: Graph full feasability space for one budget scenario

## set up
  rm(list=ls())
  date <- Sys.Date()

## libraries
  library(foreign)
  library(dplyr)
  library(lpSolve)
  library(reshape2)
  library(viridis)
  library(plotly)


## set seed
  set.seed(02139)

## set directory, bring in universal parameters
  setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
  source("./02_code/00_universal_parameters.R")
  
  ## bring in the data
  epsilon.che<-200
  epsilon.equity<-50
  epsilon.b<-5
  pc <- read.table(paste("./03_output_data/optimization_runs/package_with_0/05_optimized_package_counts_full_frontier_with0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_ 5 .csv"),sep=",",header=T,stringsAsFactors=F)
  
  pc2 <-pc[pc$budget==55000000,]
  pc2 <-  pc2[ pc2$package.deaths!=0 |  pc2$package.che.10!=0,]
  pc2 <-  pc2[,c("objective","package.deaths","package.che.10","constraint","package.E")]
  pc2 <- unique( pc2) 
  
  
  
  # for(b in seq(200000000, 700000000,100000000)){
  ## keep budget for first graph
  b<-77500000
  pc <- pc[pc$budget==b,] 
   
  
## get rid of the zeros -- these constraints (k) where too large
  pc <- pc[pc$package.deaths!=0 | pc$package.che.10!=0,]
  pc <- pc[,c("objective","package.deaths","package.che.10","constraint","package.E")]
  pc <- unique(pc)  
  
## save a file with the single objectives for plot
  d.max <- max(pc$package.deaths) 
  che.max <- max(pc$package.che.10)
  E.max <- max(pc$package.E)
  single <- pc[(pc$package.deaths>=d.max | pc$package.che.10 >=che.max | pc$package.E >=E.max),]
  
  ## save a file with the single objectives for plot
  d.max_2 <- max( pc2$package.deaths) 
  che.max_2 <- max( pc2$package.che.10)
  E.max_2 <- max( pc2$package.E)
  single_2 <-  pc2[( pc2$package.deaths>=d.max |  pc2$package.che.10 >=che.max |  pc2$package.E >=E.max),]
  
  
## create a cohesive set of points to plot for the pareto frontier that combine deaths and CHE info
  # front <- pc[pc$constraint=="<=",]
  # front <- front[order(front$package.deaths),]
  # front <- front[c("package.deaths","package.che.10","package.E")]
  # front <- unique(front)    
  

###################################################################################
####### plot the feasable space and the pareto frontier / optimal solutions #######
###################################################################################
  
# create a color palette for the plot
  colors <- viridis(10)
  colors2 <- plasma(10)
#   
# start plot
#pdf("./04_figures/FIGURE_3D.pdf",height=8,width=11)

  # plot code
  par(mar=c(6.5,6.5,2.5,1),mfrow=c(1,1))

  class(pc$package.E[pc$objective=="delta.deaths"])


 plot_ly(data=pc) %>%
   add_trace(x=pc$package.deaths[pc$objective=="delta.deaths"], 
         y=pc$package.che.10[pc$objective=="delta.deaths"], 
         z=pc$package.E[pc$objective=="delta.deaths"], 
         type="scatter3d", mode='markers',  marker = list(size = 4.5)) %>%
   add_trace(data=pc, x=pc$package.deaths[pc$objective=="delta.deaths"], 
                       y=pc$package.che.10[pc$objective=="delta.deaths"], 
                       z=pc$package.E[pc$objective=="delta.deaths"], type="mesh3d", opacity = 0.5,
             intensity = ~pc$package.E[pc$objective=="delta.deaths"]
                      )%>%
   add_trace(data=single, 
             x = single$package.deaths[single$objective == "delta.deaths"],
             y = single$package.che.10[single$objective == "delta.deaths"],
             z = single$package.E[single$objective == "delta.deaths"],
             type = "scatter3d",
             mode = 'markers',
             marker = list(size = 7.5, color = colors2[7])) %>%
   add_trace(
     x = c(single$package.deaths[single$package.deaths >= d.max],
           single$package.deaths[single$package.che.10 >= che.max]),
     y = c(single$package.che.10[single$package.deaths >= d.max],
           single$package.che.10[single$package.che.10 >= che.max]),
     z = c(single$package.E[single$package.deaths >= d.max],
           single$package.E[single$package.che.10 >= che.max]),
     type = "scatter3d",
     mode = "lines",
     line = list(color = "red", width = 2)) %>%
   add_trace(
     x = c(single$package.deaths[single$package.deaths >= d.max],
           single$package.deaths[single$package.E >= E.max]),
     y = c(single$package.che.10[single$package.deaths >= d.max],
           single$package.che.10[single$package.E >= E.max]),
     z = c(single$package.E[single$package.deaths >= d.max],
           single$package.E[single$package.E >= E.max]),
     type = "scatter3d",
     mode = "lines",
     line = list(color = "red", width = 2)) %>%
   add_trace(
     x = c(single$package.deaths[single$package.che.10 >= che.max],
           single$package.deaths[single$package.E >= E.max]),
     y = c(single$package.che.10[single$package.che.10 >= che.max] ,
           single$package.che.10[single$package.E >= E.max]),
     z = c(single$package.E[single$package.che.10 >= che.max],
           single$package.E[single$package.E >= E.max]),
     type = "scatter3d",
     mode = "lines",
     line = list(color = "red", width = 2)) %>%
   add_text(data=single, x=single$package.deaths[single$package.deaths>=d.max]+500,
            y=single$package.che.10[single$package.deaths>=d.max]+0.01,
            z=single$package.E[single$package.deaths>=d.max]+0.015,
            text="MAX Population Health", 
            textfont = list(family = "serif"), showlegend = FALSE, xanchor = "right") %>%
   add_text(data=single, x=single$package.deaths[single$package.che.10>=che.max]+500,
            y=single$package.che.10[single$package.che.10>=che.max]+0.01,
            z=single$package.E[single$package.che.10>=che.max]+0.015,
            text="MAX FRP", 
            textfont = list(family = "serif"), showlegend = FALSE, xanchor = "right") %>%
   add_text(data=single, x=single$package.deaths[single$package.E>=E.max]+500,
            y=single$package.che.10[single$package.E>=E.max]+0.01,
            z=single$package.E[single$package.E>=E.max]+0.015,
            text="MAX Equity", 
            textfont = list(family = "serif"), showlegend = FALSE, xanchor = "right") %>%
   layout(scene = list(xaxis = list(title = "Deaths Averted"),
                      yaxis = list(title = "CHE Averted"),
                      zaxis = list(title = "Equity")
                         ),
          title = paste("Optimal solutions for eCHE ",epsilon.che," and eE ",epsilon.equity," with budget ",b/1000000,"M"),
          titlefont = list(size = 12, color = "black"))
 
 
 
 
 
 #############################################################
 ### show that the Pareto set of an increased budget dominates
 #############################################################
 
 plot_ly(data=pc) %>%
   add_trace(data=pc, x=pc$package.deaths[pc$objective=="delta.deaths"], 
             y=pc$package.che.10[pc$objective=="delta.deaths"], 
             z=pc$package.E[pc$objective=="delta.deaths"], type="mesh3d", opacity = 0.2,
             intensity = ~pc$package.E[pc$objective=="delta.deaths"]
   )%>%
   add_trace(data=pc2, x= pc2$package.deaths[ pc2$objective=="delta.deaths"], 
             y= pc2$package.che.10[ pc2$objective=="delta.deaths"], 
             z= pc2$package.E[ pc2$objective=="delta.deaths"], type="mesh3d", opacity = 0.8,
             intensity = ~ pc2$package.E[ pc2$objective=="delta.deaths"]
   )%>%
   add_trace(data=single, 
             x = single$package.deaths[single$objective == "delta.deaths"],
             y = single$package.che.10[single$objective == "delta.deaths"],
             z = single$package.E[single$objective == "delta.deaths"],
             type = "scatter3d",
             mode = 'markers',
             marker = list(size = 7.5, color = colors2[7])) %>%
   add_trace(data=single_2, 
             x = single_2$package.deaths[ single_2$objective == "delta.deaths"],
             y =  single_2$package.che.10[ single_2$objective == "delta.deaths"],
             z =  single_2$package.E[ single_2$objective == "delta.deaths"],
             type = "scatter3d",
             mode = 'markers',
             marker = list(size = 7.5, color = colors2[7])) %>%
   add_text(data=single, x=single$package.deaths[single$package.deaths>=d.max]+500,
            y=single$package.che.10[single$package.deaths>=d.max]+0.01,
            z=single$package.E[single$package.deaths>=d.max]+0.015,
            text="MAX Population Health", 
            textfont = list(family = "serif"), showlegend = FALSE, xanchor = "right") %>%
   add_text(data=single, x=single$package.deaths[single$package.che.10>=che.max]+500,
            y=single$package.che.10[single$package.che.10>=che.max]+0.01,
            z=single$package.E[single$package.che.10>=che.max]+0.015,
            text="MAX FRP", 
            textfont = list(family = "serif"), showlegend = FALSE, xanchor = "right") %>%
   add_text(data=single, x=single$package.deaths[single$package.E>=E.max]+500,
            y=single$package.che.10[single$package.E>=E.max]+0.01,
            z=single$package.E[single$package.E>=E.max]+0.015,
            text="MAX Equity", 
            textfont = list(family = "serif"), showlegend = FALSE, xanchor = "right") %>%
   layout(scene = list(xaxis = list(title = "Deaths Averted"),
                       yaxis = list(title = "CHE Averted"),
                       zaxis = list(title = "Equity")
   ),
   title = paste("Optimal solutions for eCHE ",epsilon.che," and eE ",epsilon.equity," with budget 55M and 77.5M"),
   titlefont = list(size = 12, color = "black"))
 
 
 
 
# ######Try to parametrize the plot into a surface 
#  x <- pc$package.deaths[pc$objective == "delta.deaths"]
#  y <- pc$package.che.10[pc$objective == "delta.deaths"]
#  z <- pc$package.E[pc$objective == "delta.deaths"]
#  model <- lm(z ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + y + I(y^2) + I(y^3) + I(y^4) + I(y^5), data = pc)
#  
#  # Define the number of samples for x and y
#  x_sample <- 100
#  y_sample <- 100
#  
#  # Create sequences of x and y values
#  x_model <- seq(0, 5903, length.out = x_sample)
#  y_model <- seq(0, 0.15, length.out = y_sample)
#  
#  # Initialize an empty matrix to store the predictions
#  prediction_matrix <- matrix(NA, nrow = y_sample, ncol = x_sample)
#  
#  # Loop through each combination of x and y values
#  for (i in 1:y_sample) {
#    for (j in 1:x_sample) {
#      new_data <- data.frame(x = x_model[j], y = y_model[i])
#      prediction_matrix[j, i] <- predict(model, new_data)
#    }
#  }
#  
# # z_model <- predict(model, newdata = new_data)
# # plot_ly() %>% 
# #   add_trace(x=x_model, 
# #             y=y_model, 
# #             z=z_model, 
# #             type="scatter3d", mode='markers',  marker = list(size = 4.5)) %>%
# #   add_trace(x=x_model, 
# #             y=y_model, 
# #             z=z_model, type="mesh3d", opacity = 0.5,
# #             intensity = ~z_model
# #   )
#  
#  
#  
# 
# plot_ly(z=prediction_matrix, type="surface")
 

#dev.off()



