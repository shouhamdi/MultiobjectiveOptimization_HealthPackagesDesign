###########################
#######TRADE-OFFS##########


## Date:    01/08/2023
## Purpose: find the trade offs from the slopes of the limit hyperplans in order to consider one point as the optimal 3D objective package

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
library(pracma)
library(geometry)


# create a color palette for the plot
colors <- viridis(10)
colors2 <- plasma(10)

## set seed
set.seed(02139)

## set directory, bring in universal parameters
setwd("~/Documents/Sophia/Stage_Harvard/Health_packages/Code/")
source("./02_code/00_universal_parameters.R")

## bring in the data with the correct parameters
epsilon.che<-200
epsilon.equity<-50
epsilon.b<-5
pc <- read.table(paste("./03_output_data/optimization_runs/package_with_0/05_optimized_package_counts_full_frontier_with0_eCHE_",epsilon.che,"_eE_",epsilon.equity,"_budget_ 5 .csv"),sep=",",header=T,stringsAsFactors=F)
b<-55000000
pc <- pc[pc$budget==b,] 


## get rid of the zeros -- these constraints (k) where too large
pc <- pc[pc$package.deaths!=0 | pc$package.che.10!=0,]
pc <- pc[,c("package.deaths","package.che.10","package.E")]
pc <- unique(pc)  


# Compute the convex hull
pc_matrix <- as.matrix(pc)
convex_hull <- convhulln(pc_matrix) #returns the indices in each row describing the vertices of 2-dimensional triangles

# extract the points forming the convex hull
pc_h <- c(convex_hull)
pc_h <-unique(pc_h)
points_in_convex_hull <- pc[pc_h,]


## select single objectives packages
d.max <- max(pc$package.deaths) 
che.max <- max(pc$package.che.10)
E.max <- max(pc$package.E)
single <- pc[(pc$package.deaths>=d.max | pc$package.che.10 >=che.max | pc$package.E >=E.max),]

##retrieve the triangles that contain a MAX
#find the indices of the MAX
i_d.max <- which(pc[["package.deaths"]] == d.max)
i_che.max <- which(pc[["package.che.10"]] == che.max)
i_E.max <- which(pc[["package.E"]] == E.max)

#store the triangles which vertices is one of the MAX
convex_hull <- as.data.frame(convex_hull)
tr_d.max <-convex_hull[convex_hull$V1 == i_d.max | convex_hull$V2 == i_d.max | convex_hull$V3 == i_d.max, ]
tr_che.max <-convex_hull[convex_hull$V1 == i_che.max | convex_hull$V2 == i_che.max | convex_hull$V3 == i_che.max, ]
tr_E.max <-convex_hull[convex_hull$V1 == i_E.max | convex_hull$V2 == i_E.max | convex_hull$V3 == i_E.max, ]




##plot the convexhull

plot_ly(data=pc) %>%
  add_trace(x=pc$package.deaths, 
            y=pc$package.che.10, 
            z=pc$package.E, 
            type="scatter3d", mode='markers',  marker = list(size = 4.5)) %>%
  add_trace(data=pc, x=pc$package.deaths, 
            y=pc$package.che.10, 
            z=pc$package.E, type="mesh3d", opacity = 0.5,
            intensity = ~pc$package.E)%>%
  add_trace(data=single, 
            x = single$package.deaths,
            y = single$package.che.10,
            z = single$package.E,
            type = "scatter3d",
            mode = 'markers',
            marker = list(size = 10, color = colors2[7])) %>%
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
  add_trace(data=points_in_convex_hull, 
            x = points_in_convex_hull$package.deaths,
            y = points_in_convex_hull$package.che.10,
            z = points_in_convex_hull$package.E,
            type = "scatter3d",
            mode = 'markers',
            marker = list(size = 6, color = colors2[1])) %>%
  add_trace(x=surface_pts_d.max[,1], 
            y=surface_pts_d.max[,2], 
            z=surface_pts_d.max[,3], type="mesh3d", opacity = 0.25)%>%
  add_trace(x=surface_pts_che.max[,1], 
            y=surface_pts_che.max[,2], 
            z=surface_pts_che.max[,3], type="mesh3d", opacity = 0.25)%>%
  add_trace(x=surface_pts_E.max[,1], 
            y=surface_pts_E.max[,2], 
            z=surface_pts_E.max[,3], type="mesh3d", opacity = 0.25)%>%
  # add_surface(z=surface_matrix, type="surface") %>%
  layout(scene = list(xaxis = list(title = "Deaths Averted"),
                      yaxis = list(title = "CHE Averted"),
                      zaxis = list(title = "Equity")
  ),
  title = paste("Optimal solutions for eCHE ",epsilon.che," and eE ",epsilon.equity," with budget 55M with hyperplan"),
  titlefont = list(size = 12, color = "black"))



print( planes_d.max[3,])
