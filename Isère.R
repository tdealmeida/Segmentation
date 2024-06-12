# Fonction permettant de comparer les algo sur des données réelles et avec log


library(dplyr)
library(changepoint)
# devtools::install_github("lvaudor/hubr")
library(hubr)
library(Rbeast)
library(strucchange)
library(ecp)
library(cpm)
library(RPostgreSQL)
library(sf)
library(purrr)
library(tidyr)
library(cumSeg)
library(reticulate)
library(readr)
library(ggplot2)
library(VGAM)
library(gridExtra)


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

run_change_point_analysis <- function(values,name) {
  
  var <- var(values)
  N <- length(values)
  ID <- name
  
  temps_debut_pelt <- Sys.time()
  cpt_result_pelt <- cpt.mean(values, method = "PELT", minseglen = 4, penalty = "Manual", pen.value = var(values) * log(length(values)))
  temps_fin_pelt <- Sys.time()
  time_pelt <- temps_fin_pelt - temps_debut_pelt
  pen_list_pelt <- cpt_result_pelt@pen.value
  cpt_pelt <- cpt_result_pelt@cpts
  cpt_pelt <- cpt_pelt[-length(cpt_pelt)]
  groups <- split(values, findInterval(seq_along(values), cpt_pelt))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_pelt <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_pelt <- mean(group_lengths)
  
  temps_debut_BinSeg <- Sys.time()
  cpt_result_BinSeg <- cpt.mean(values, method = "BinSeg", Q= 100, penalty = "Manual", pen.value = var(values) * log(length(values)))
  temps_fin_BinSeg <- Sys.time()
  time_BinSeg <- temps_fin_BinSeg - temps_debut_BinSeg
  pen_list_BinSeg <- cpt_result_BinSeg@pen.value
  cpt_BinSeg <- cpt_result_BinSeg@cpts
  cpt_BinSeg <- cpt_BinSeg[-length(cpt_BinSeg)]
  groups <- split(values, findInterval(seq_along(values), cpt_BinSeg))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_BinSeg <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_BinSeg <- mean(group_lengths)
  
  temps_debut_SegNeigh <- Sys.time()
  cpt_result_SegNeigh <- cpt.mean(values, method = "SegNeigh", Q= 100,penalty = "Manual", pen.value = var(values) * log(length(values)))
  temps_fin_SegNeigh <- Sys.time()
  time_SegNeigh <- temps_fin_SegNeigh - temps_debut_SegNeigh
  pen_list_SegNeigh <- cpt_result_SegNeigh@pen.value
  cpt_SegNeigh <- cpt_result_SegNeigh@cpts
  cpt_SegNeigh <- cpt_SegNeigh[-length(cpt_SegNeigh)]
  groups <- split(values, findInterval(seq_along(values), cpt_SegNeigh))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_SegNeigh <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_SegNeigh <- mean(group_lengths)
  
  temps_debut_hubert <- Sys.time()
  cpt_result_hubert <- Hubert_segmentation(values,alpha = 0.05)
  temps_fin_hubert <- Sys.time()
  time_hubert <- temps_fin_hubert - temps_debut_hubert
  alpha_hubert <- 0.05
  cpt_hubert <- cpt_result_hubert$locations
  cpt_hubert <- cpt_hubert[-length(cpt_hubert)]
  cpt_hubert <- cpt_hubert[-1]
  groups <- split(values, findInterval(seq_along(values), cpt_hubert))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_hubert <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_hubert <- mean(group_lengths)
  
  temps_debut_e_div <- Sys.time()
  cpt_result_e_div <- e.divisive(matrix(values), sig.lvl = 0.05 , R = 199, k = NULL, min.size = 5, alpha = 1)
  temps_fin_e_div <- Sys.time()
  time_e_div <- temps_fin_e_div - temps_debut_e_div
  cpt_e_div <- cpt_result_e_div$estimates
  cpt_e_div <- cpt_e_div[-length(cpt_e_div)]
  cpt_e_div <- cpt_e_div[-1]
  groups <- split(values, findInterval(seq_along(values), cpt_e_div))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_e_div <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_e_div <- mean(group_lengths)
  
  # temps_debut_e_agglo <- Sys.time()
  # cpt_result_e_agglo <- e.agglo(matrix(values),member=1:length(values), alpha=1, penalty=function(cps){0})
  # temps_fin_e_agglo <- Sys.time()
  # time_e_agglo <- temps_fin_e_agglo - temps_debut_e_agglo
  # cpt_e_agglo <- cpt_result_e_agglo$estimates
  # cpt_e_agglo <- cpt_e_agglo[-length(cpt_e_agglo)]
  # cpt_e_agglo <- cpt_e_agglo[-1]
  # groups <- split(values, findInterval(seq_along(values), cpt_e_agglo))
  # sd_groups <- map_dbl(groups, ~ sd(.x))
  # effect_size_e_agglo <- mean(sd_groups,na.rm = TRUE)
  # group_lengths <- map_dbl(groups, ~ length(.x))
  # segment_mean_e_agglo <- mean(group_lengths)
  
  # temps_debut_breakpoints <- Sys.time()
  # cpt_result_breakpoints <- breakpoints(values~1,h=20)
  # temps_fin_breakpoints <- Sys.time()
  # time_breakpoints <- temps_fin_breakpoints - temps_debut_breakpoints
  # cpt_breakpoints <- cpt_result_breakpoints$breakpoints
  # groups <- split(values, findInterval(seq_along(values), cpt_breakpoints))
  # sd_groups_breakpoints <- map_dbl(groups, ~ sd(.x))
  # effect_size_breakpoints <- mean(sd_groups,na.rm = TRUE)
  # group_lengths <- map_dbl(groups, ~ length(.x))
  # segment_mean_breakpoints <- mean(group_lengths)
  
  temps_debut_cpm <- Sys.time()
  cpt_result <- processStream(values,cpmType = "Student",ARL0 = ceiling(0.5 *length(x1)/1000)*1000, startup = 20)
  temps_fin_cpm <- Sys.time()
  time_cpm <- temps_fin_cpm - temps_debut_cpm
  cpt_cpm <- cpt_result$changePoints
  groups <- split(values, findInterval(seq_along(values), cpt_cpm))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_cpm <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_cpm <- mean(group_lengths)
  
  temps_debut_beast <- Sys.time()
  cpt_result_beast <- beast(values,season = "none",tseg.min	= 5,tcp.minmax = c(0,100))
  temps_fin_beast <- Sys.time()
  time_beast <- temps_fin_beast - temps_debut_beast
  ncp_mode_beast <- cpt_result_beast$trend$ncp_mode
  cp_beast <- cpt_result_beast$trend$cp
  cpt_beast <- cp_beast[1:ncp_mode_beast]
  cpt_beast <- sort(cpt_beast)
  groups <- split(values, findInterval(seq_along(values), cpt_beast))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_beast <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_beast <- mean(group_lengths)
  
  temps_debut_jumpoint <- Sys.time()
  cpt_result_jumpoint <- jumpoints(values, output = "2")
  temps_fin_jumpoint <- Sys.time()
  time_jumpoint <- temps_fin_jumpoint - temps_debut_jumpoint
  cpt_jumpoint <- cpt_result_jumpoint$psi
  groups <- split(values, findInterval(seq_along(values), cpt_jumpoint))
  sd_groups <- map_dbl(groups, ~ sd(.x))
  effect_size_jumpoint <- mean(sd_groups,na.rm = TRUE)
  group_lengths <- map_dbl(groups, ~ length(.x))
  segment_mean_jumpoint <- mean(group_lengths)  
  
  #   temps_debut <- Sys.time()
  #   write.csv(values, "C:/Users/tdealm01/Desktop/doctorat_TDA/avancements_non_manuscrits/fichier R/fonction/data.csv", row.names = FALSE)
  #   python_code <- sprintf('
  # import numpy as np
  # import ruptures as rpt
  # import pandas as pd
  # 
  # 
  # data = pd.read_csv("C:/Users/tdealm01/Desktop/doctorat_TDA/avancements_non_manuscrits/fichier R/fonction/data.csv")
  # data = np.array(data)
  # pen = np.log(len(data))*(np.log(np.var(data))*5)
  # algo = rpt.BottomUp(model="l1").fit(data)
  # result = algo.predict(pen=pen)
  #   ')
  #   cpt_result <- py_run_string(python_code) # sans var donc pour des log : np.log(len(data))*np.log(len(data))
  #   temps_fin <- Sys.time()
  #   time_bottomup <- temps_fin - temps_debut
  #   cpt_bottomup <-  cpt_result$result 
  #   groups <- split(values, findInterval(seq_along(values), cpt_bottomup))
  #   sd_groups <- map_dbl(groups, ~ sd(.x))
  #   effect_size_bottomup <- mean(sd_groups,na.rm = TRUE)
  #   group_lengths <- map_dbl(groups, ~ length(.x))
  #   segment_mean_bottomup <- mean(group_lengths)
  
  
  cp_pelt <- data.frame(cp_values = cpt_pelt, method = "PELT")
  cp_BinSeg <- data.frame(cp_values = cpt_BinSeg, method = "BinSeg")
  cp_SegNeigh <- data.frame(cp_values = cpt_SegNeigh, method = "SegNeigh")
  cp_hubert <- data.frame(cp_values = cpt_hubert, method = "hubert")
  cp_cpm <- data.frame(cp_values = cpt_cpm, method = "cpm")
  cp_beast <- data.frame(cp_values = cpt_beast, method = "beast")
  cp_jumpoint <- data.frame(cp_values = cpt_jumpoint, method = "jumpoint")
  cp_e_div <- data.frame(cp_values = cpt_e_div, method = "e_divisive")
  
  cp_result <- rbind(cp_e_div, cp_pelt, cp_cpm, cp_beast, cp_jumpoint, cp_hubert
                     ,cp_BinSeg, cp_SegNeigh
  )
  
  results <- data.frame(
    ID = ID,
    N = N,
    var = var,
    time_pelt=as.numeric(time_pelt, units = "secs"),
    cpt_pelt = length(cpt_pelt),
    pen_list_pelt = pen_list_pelt,
    segment_mean_pelt = segment_mean_pelt, 
    effect_size_pelt = effect_size_pelt,
    time_BinSeg=as.numeric(time_BinSeg, units = "secs"),
    cpt_BinSeg = length(cpt_BinSeg),
    pen_list_BinSeg = pen_list_BinSeg,
    segment_mean_BinSeg = segment_mean_BinSeg,
    effect_size_BinSeg = effect_size_BinSeg,
    time_SegNeigh=as.numeric(time_SegNeigh, units = "secs"),
    cpt_SegNeigh = length(cpt_SegNeigh),
    pen_list_SegNeigh = pen_list_SegNeigh,
    segment_mean_SegNeigh = segment_mean_SegNeigh,
    effect_size_SegNeigh = effect_size_SegNeigh,
    time_hubert = as.numeric(time_hubert, units = "secs"),
    alpha_hubert = alpha_hubert,
    cpt_hubert = length(cpt_hubert),
    segment_mean_hubert = segment_mean_hubert,
    effect_size_hubert = effect_size_hubert,
    time_e_div = as.numeric(time_e_div, units = "secs"),
    cpt_e_div = length(cpt_e_div),
    segment_mean_e_div =segment_mean_e_div,
    effect_size_e_div = effect_size_e_div,
    # time_e_agglo = as.numeric(time_e_agglo, units = "secs"),
    # cpt_e_agglo = length(cpt_e_agglo),
    # segment_mean_e_agglo = segment_mean_e_agglo,
    # effect_size_e_agglo = effect_size_e_agglo,
    # time_breakpoints = as.numeric(time_breakpoints, units = "secs"),
    # cpt_breakpoints = length(cpt_breakpoints),
    # segment_mean_breakpoints = segment_mean_breakpoints,
    # effect_size_breakpoints = effect_size_breakpoints,
    time_cpm = as.numeric(time_cpm, units = "secs"),
    cpt_cpm = length(cpt_cpm),
    segment_mean_cpm = segment_mean_cpm,
    effect_size_cpm = effect_size_cpm,
    time_beast = as.numeric(time_beast, units = "secs"),
    cpt_beast = length(cpt_beast),
    segment_mean_beast = segment_mean_beast,
    effect_size_beast = effect_size_beast,
    time_jumpoint = as.numeric(time_jumpoint, units = "secs"),
    cpt_jumpoint = length(cpt_jumpoint),
    segment_mean_jumpoint = segment_mean_jumpoint,
    effect_size_jumpoint = effect_size_jumpoint
    # time_bottomup = as.numeric(time_bottomup, units = "secs"),
    # cpt_bottomup = length(cpt_bottomup),
    # segment_mean_bottomup = segment_mean_bottomup,
    # effect_size_bottomup = effect_size_bottomup
  )
  
  return(list(cp_result = cp_result, results = results))
}

values <- Data$active_channel_width
name = "1 - active_channel_width"
resultats_reel_AC <- run_change_point_analysis(values,name)
lengths <- aggregate(resultats_reel_AC$cp_result[,1] ~ method, data = resultats_reel_AC$cp_result, FUN = length)
names(lengths)[2] <- name
resultats_reel_AC$cp_result$ID <- name
results_AC <- resultats_reel_AC$results
cp_result_AC <- resultats_reel_AC$cp_result

values <- log(Data$active_channel_width+1)
name = "2 - log_active_channel_width"
resultats_reel_log_AC <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_log_AC$cp_result[,1] ~ method, data = resultats_reel_log_AC$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[3] <- name
resultats_reel_log_AC$cp_result$ID <- name
results_AC <- rbind(results_AC,resultats_reel_log_AC$results)
cp_result_AC <- rbind(cp_result_AC,resultats_reel_log_AC$cp_result)

values <- Data$valley_bottom_width
name = "3 - valley_bottom_width"
resultats_reel_VB <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_VB$cp_result[,1] ~ method, data = resultats_reel_VB$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[4] <- name
resultats_reel_VB$cp_result$ID <- name
results_VB <- resultats_reel_VB$results
cp_result_VB <- resultats_reel_VB$cp_result

values <- log(Data$valley_bottom_width+1)
name = "4 - log_valley_bottom_width"
resultats_reel_log_VB <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_log_VB$cp_result[,1] ~ method, data = resultats_reel_log_VB$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[5] <- name
resultats_reel_log_VB$cp_result$ID <- name
results_VB <- rbind(results_VB,resultats_reel_log_VB$results)
cp_result_VB <- rbind(cp_result_VB,resultats_reel_log_VB$cp_result)


results_top <- rbind(results_AC,results_VB)
cp_result_top <- rbind(cp_result_AC,cp_result_VB)

#

values <- Data$natural_corridor_width
name = "5 - natural_corridor_width"
resultats_reel_NC <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_NC$cp_result[,1] ~ method, data = resultats_reel_NC$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[6] <- name
resultats_reel_NC$cp_result$ID <- name
results_total <- rbind(results_top,resultats_reel_NC$results)
cp_result_total <- rbind(cp_result_top,resultats_reel_NC$cp_result)

values <- log(Data$natural_corridor_width+1)
name = "6 - log_natural_corridor_width"
resultats_reel_log_NC <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_log_NC$cp_result[,1] ~ method, data = resultats_reel_log_NC$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[7] <- name
resultats_reel_log_NC$cp_result$ID <- name
results_total <- rbind(results_total,resultats_reel_log_NC$results)
cp_result_total <- rbind(cp_result_total,resultats_reel_log_NC$cp_result)

values <- Data$talweg_slope
name = "7 - talweg_slope"
resultats_reel_slope <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_slope$cp_result[,1] ~ method, data = resultats_reel_slope$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[8] <- name
resultats_reel_slope$cp_result$ID <- name
results_total <- rbind(results_total,resultats_reel_slope$results)
cp_result_total <- rbind(cp_result_total,resultats_reel_slope$cp_result)

values <- log(Data$talweg_slope+1)
name = "8 - log_talweg_slope"
resultats_reel_log_slope <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_log_slope$cp_result[,1] ~ method, data = resultats_reel_log_slope$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[9] <- name
resultats_reel_log_slope$cp_result$ID <- name
results_total <- rbind(results_total,resultats_reel_log_slope$results)
cp_result_total <- rbind(cp_result_total,resultats_reel_log_slope$cp_result)

values <- Data$idx_confinement
name = "9 - idx_confinement"
resultats_reel_IC <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_IC$cp_result[,1] ~ method, data = resultats_reel_IC$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[10] <- name
resultats_reel_IC$cp_result$ID <- name
results_total <- rbind(results_total,resultats_reel_IC$results)
cp_result_total <- rbind(cp_result_total,resultats_reel_IC$cp_result)

values <- log(Data$idx_confinement+1)
name = "10 - log_idx_confinement"
resultats_reel_log_IC <- run_change_point_analysis(values,name)
tt <- aggregate(resultats_reel_log_IC$cp_result[,1] ~ method, data = resultats_reel_log_IC$cp_result, FUN = length)
lengths <- cbind(lengths,tt[,2])
names(lengths)[11] <- name
resultats_reel_log_IC$cp_result$ID <- name
results_total <- rbind(results_total,resultats_reel_log_IC$results)
cp_result_total <- rbind(cp_result_total,resultats_reel_log_IC$cp_result)






# # Pour tester la distribution des données si pvalue test de shapiro 
# # < 0.05 alors pas loi normale
# options(scipen = 999)
# hist(values)
# qqnorm(values, datax=TRUE)
# qqline(values,datax=TRUE)
# shapiro.test(values)
# 
# hist(VB)
# qqnorm(VB, datax=TRUE)
# qqline(VB,datax=TRUE)
# shapiro.test(VB)
# 
# hist(NC)
# qqnorm(NC, datax=TRUE)
# qqline(NC,datax=TRUE)
# shapiro.test(NC)
# 
# hist(Slope)
# qqnorm(Slope, datax=TRUE)
# qqline(Slope,datax=TRUE)
# shapiro.test(Slope)
# 
# hist(IC)
# qqnorm(IC, datax=TRUE)
# qqline(IC,datax=TRUE)
# shapiro.test(IC)
# 
# hist(WC)
# qqnorm(WC, datax=TRUE)
# qqline(WC,datax=TRUE)
# shapiro.test(WC)
# 
# # test avec log
# hist(log_values)
# qqnorm(log_values, datax=TRUE)
# qqline(log_values,datax=TRUE)
# shapiro.test(log_values)
