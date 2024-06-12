library(cumSeg)
library(reticulate)
library(readr)
library(ggplot2)
library(VGAM)
library(gridExtra)
library(reticulate)


Data <- read_csv("csv.csv", col_types = cols(measure_medial_axis = col_number(), 
                                             talweg_elevation_min = col_number(), 
                                             idx_confinement = col_number(), water_channel_width = col_number(), 
                                             active_channel_width = col_number(), 
                                             natural_corridor_width = col_number(), 
                                             connected_corridor_width = col_number(), 
                                             valley_bottom_width = col_number(), talweg_slope = col_number(), 
                                             floodplain_slope = col_number(), water_channel = col_number(), 
                                             gravel_bars = col_number(), natural_open = col_number(), 
                                             forest = col_number(), grassland = col_number(), 
                                             crops = col_number(), diffuse_urban = col_number(), 
                                             dense_urban = col_number(), infrastructures = col_number(), 
                                             active_channel = col_number(), riparian_corridor = col_number(), 
                                             semi_natural = col_number(), reversible = col_number(), 
                                             disconnected = col_number(), built_environment = col_number(), 
                                             water_channel_pc = col_number(), gravel_bars_pc = col_number(), 
                                             natural_open_pc = col_number(), forest_pc = col_number(), 
                                             grassland_pc = col_number(), crops_pc = col_number(), 
                                             diffuse_urban_pc = col_number(), dense_urban_pc = col_number(), 
                                             infrastructures_pc = col_number(), active_channel_pc = col_number(), 
                                             riparian_corridor_pc = col_number(), 
                                             semi_natural_pc = col_number(), reversible_pc = col_number(), 
                                             disconnected_pc = col_number(), built_environment_pc = col_number(), 
                                             sum_area = col_number()))
Data <- arrange(Data,measure)
Data <- Data[Data$toponyme == "le Drac", ]

while (anyNA(Data[1,])) {
  Data <- Data[-1,]
}

while (anyNA(Data[nrow(Data),])) {
  Data <- Data[-nrow(Data),]
}
Data <- Data %>% mutate_if(is.numeric, na.approx)
while (anyNA(Data)) {
  Data <- na.approx(Data)
}





python_script <- sprintf('
from ggs import *
import numpy as np
import pandas as pd

Data = pd.read_csv("data/new_data.csv")
Data = np.array(Data.T)
print(Data)

bps, objectives = GGS(Data,Kmax = %s,lamb = %s)
plotVals = list(range(len(objectives)))
df = pd.DataFrame({\'Number_of_Breakpoints\': plotVals, \'Objective\': objectives})

' , input$Kmax, input$lamb)
  
  tttt <- py_run_string(python_script)
  cps = tttt$bps
  seg = cps[length(cps)]
  seg = seg[[1]]
  seg[1] <- 1
  cpt_points <- seg
  
}


return(list(seg = seg, cpt_points = cpt_points))