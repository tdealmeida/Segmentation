pelt <- results_total %>%
  select(N,var,cpt_pelt,time_pelt,segment_mean_pelt,effect_size_pelt,ID)
pelt$method <- "PELT"
colnames(pelt)[3] <- "cpt"
colnames(pelt)[4] <- "time"
colnames(pelt)[5] <- "segment_mean"
colnames(pelt)[6] <- "effect_size_algo"
SegNeigh <- results_total %>%
  select(N,var,cpt_SegNeigh,time_SegNeigh,segment_mean_SegNeigh,effect_size_SegNeigh,ID)
SegNeigh$method <- "SegNeigh"
colnames(SegNeigh)[3] <- "cpt"
colnames(SegNeigh)[4] <- "time"
colnames(SegNeigh)[5] <- "segment_mean"
colnames(SegNeigh)[6] <- "effect_size_algo"
BinSeg <- results_total %>%
  select(N,var,cpt_BinSeg,time_BinSeg,segment_mean_BinSeg,effect_size_BinSeg,ID)
BinSeg$method <- "BinSeg"
colnames(BinSeg)[3] <- "cpt"
colnames(BinSeg)[4] <- "time"
colnames(BinSeg)[5] <- "segment_mean"
colnames(BinSeg)[6] <- "effect_size_algo"
hubert <- results_total %>%
  select(N,var,cpt_hubert,time_hubert,segment_mean_hubert,effect_size_hubert,ID)
hubert$method <- "hubert"
colnames(hubert)[3] <- "cpt"
colnames(hubert)[4] <- "time"
colnames(hubert)[5] <- "segment_mean"
colnames(hubert)[6] <- "effect_size_algo"
e_div <- results_total %>%
  select(N,var,cpt_e_div,time_e_div,segment_mean_e_div,effect_size_e_div,ID)
e_div$method <- "e_div"
colnames(e_div)[3] <- "cpt"
colnames(e_div)[4] <- "time"
colnames(e_div)[5] <- "segment_mean"
colnames(e_div)[6] <- "effect_size_algo"
cpm <- results_total %>%
  select(N,var,cpt_cpm,time_cpm,segment_mean_cpm,effect_size_cpm,ID)
cpm$method <- "cpm"
colnames(cpm)[3] <- "cpt"
colnames(cpm)[4] <- "time"
colnames(cpm)[5] <- "segment_mean"
colnames(cpm)[6] <- "effect_size_algo"
beast <- results_total %>%
  select(N,var,cpt_beast,time_beast,segment_mean_beast,effect_size_beast,ID)
beast$method <- "beast"
colnames(beast)[3] <- "cpt"
colnames(beast)[4] <- "time"
colnames(beast)[5] <- "segment_mean"
colnames(beast)[6] <- "effect_size_algo"
jumpoint <- results_total %>%
  select(N,var,cpt_jumpoint,time_jumpoint,segment_mean_jumpoint,effect_size_jumpoint,ID)
jumpoint$method <- "jumpoint"
colnames(jumpoint)[3] <- "cpt"
colnames(jumpoint)[4] <- "time"
colnames(jumpoint)[5] <- "segment_mean"
colnames(jumpoint)[6] <- "effect_size_algo"

algo <- rbind(pelt, SegNeigh, BinSeg, hubert, 
              e_div, cpm, beast, jumpoint)
algo <- algo %>% 
  mutate(segment_mean = sapply(segment_mean, function(x) as.numeric(x[[1]])))
algo <- algo %>% 
  mutate(time = sapply(time, function(x) as.numeric(x[[1]])))

#

separated_data <- split(resultats_reel_AC$cp_result, resultats_reel_AC$cp_result$method)

#

values_AC <- data.frame(values = Data$active_channel_width)
values_AC$x <- 1:680
values_AC$ID <- "1 - active_channel_width"
values_log_AC <- data.frame(values = log(Data$active_channel_width+1))
values_log_AC$x <- 1:680
values_log_AC$ID <- "2 - log_active_channel_width"
values_VB <- data.frame(values = Data$valley_bottom_width)
values_VB$x <- 1:680
values_VB$ID <- "3 - valley_bottom_width"
values_log_VB <- data.frame(values = log(Data$valley_bottom_width+1))
values_log_VB$x <- 1:680
values_log_VB$ID <- "4 - log_valley_bottom_width"
new_values_top <- rbind(values_AC,values_log_AC,values_VB,values_log_VB)
new_values_AC <- rbind(values_AC,values_log_AC)
new_values_VB <- rbind(values_VB,values_log_VB)

#
#
#

#graphique pour montrer segmentation facet de données réelles pour 1 série univariée
ggplot(data = values_AC, aes(x = x, y = values)) +
  geom_line() +
  geom_vline(data = resultats_reel_AC$cp_result, aes(xintercept = cp_values), color = "red",linetype="dashed") +
  facet_wrap(~method,ncol = 2) +
  theme_bw() +
  labs( x = "Distance", y = "Active channel width")

#idem mais avec un facet différents afin d'obtenir des segments
grid_arrange_1 <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data$beast$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "Beast",x = NULL, y = NULL), 
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data$cpm$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "CPM",x = NULL, y = NULL),
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data$jumpoint$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "Jumpoint",x = NULL,y=NULL),
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data$e_divisive$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "E.div",x = NULL, y = NULL),
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data$PELT$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "PELT",x = NULL, y = NULL),
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data$hubert$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "Hubert",x = NULL, y = NULL),
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data$BinSeg$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 labs(title = "BinSeg", 
                      x = "Distance",y=NULL),
               
               ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data$segneigh$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 labs(title = "SegNeigh", 
                      x = "Distance",y=NULL),
               
               ncol = 2)
}
grid_arrange_1()

#
#
#

# exemple avec pelt
new_test_pelt <- cp_result_top[cp_result_top$method == "PELT",]

ggplot(new_values_top) +
  geom_line(data = new_values_top, aes(x = x, y = values)) +
  geom_vline(data = new_test_pelt, aes(xintercept = cp_values), color = "red",linetype="dashed") +
  facet_wrap(~ID,scales = "free_y",nrow = 2) +
  theme_bw() +
  labs(title = "PELT", x = "Distance",y = NULL)

# grid arrange avec plusieurs algo en ligne 
new_test_SegNeigh <- cp_result_top[cp_result_top$method == "SegNeigh",]
new_test_BinSeg <- cp_result_top[cp_result_top$method == "BinSeg",]
new_test_hubert <- cp_result_top[cp_result_top$method == "hubert",]
new_test_cpm <- cp_result_top[cp_result_top$method == "cpm",]
new_test_e_divisive <- cp_result_top[cp_result_top$method == "e_divisive",]
new_test_jumpoint <- cp_result_top[cp_result_top$method == "jumpoint",]
new_test_beast <- cp_result_top[cp_result_top$method == "beast",]

grid_arrange <- function(){
  grid.arrange(
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_pelt, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      theme_bw() +
      labs(y = "PELT", x = NULL)+
      theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()),  
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_BinSeg, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      theme_bw() +
      labs(y = "BinSeg", x = NULL)+
      theme(axis.text.x = element_blank(), 
            strip.text = element_blank(),axis.ticks.x = element_blank()),
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_SegNeigh, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      labs(y = "SegNeigh", x = NULL)+
      theme_bw() +
      theme(axis.text.x = element_blank(),  
            strip.text = element_blank(),axis.ticks.x = element_blank()),
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_hubert, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      labs(y = "Hubert", x = NULL)+
      theme_bw() +
      theme(axis.text.x = element_blank(),  
            strip.text = element_blank(),axis.ticks.x = element_blank()),
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_cpm, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      labs(y = "CPM", x = NULL)+
      theme_bw() +
      theme(axis.text.x = element_blank(),  
            strip.text = element_blank(),axis.ticks.x = element_blank()),
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_e_divisive, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      labs(y = "E_div", x = NULL)+
      theme_bw() +
      theme(axis.text.x = element_blank() 
            ,strip.text = element_blank(),axis.ticks.x = element_blank()),
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_jumpoint, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1) +
      labs(y = "Jumpoint", x = NULL)+
      theme_bw() +
      theme(axis.text.x = element_blank()  
            ,strip.text = element_blank(),axis.ticks.x = element_blank()),
    ggplot(new_values_top) +
      geom_line(data = new_values_top, aes(x = x, y = values)) +
      geom_vline(data = new_test_beast, aes(xintercept = cp_values), color = "red",linetype="dashed") +
      facet_wrap(~ID,scales = "free_y",nrow = 1,axis.labels = "margins") +
      theme_bw() +
      labs(y = "Beast", x = NULL)+
      theme(strip.text = element_blank()),
    ncol = 1
    
  )
  
}
grid_arrange()

# identique mais avec grid
separated_data_pelt <- split(new_test_pelt, new_test_pelt$ID)
separated_data_BinSeg <- split(new_test_BinSeg, new_test_BinSeg$ID)
separated_data_SegNeigh <- split(new_test_SegNeigh, new_test_SegNeigh$ID)
separated_data_hubert <- split(new_test_hubert, new_test_hubert$ID)
separated_data_cpm <- split(new_test_cpm, new_test_cpm$ID)
separated_data_e_divisive <- split(new_test_e_divisive, new_test_e_divisive$ID)
separated_data_jumpoint <- split(new_test_jumpoint, new_test_jumpoint$ID)
separated_data_beast <- split(new_test_beast, new_test_beast$ID)

grid_pelt <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_pelt$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "active channel width",x = NULL, y = "PELT"), 
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_pelt$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "log active channel width",x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_pelt$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "Valley bottom width",x = NULL,y=NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_pelt$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(title = "log Valley bottom width",x = NULL, y = NULL),
               
               ncol = 4)
  
}
grid_BinSeg <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_BinSeg$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = "BinSeg"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_BinSeg$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_BinSeg$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_BinSeg$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}
grid_SegNeigh <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_SegNeigh$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = "SegNeigh"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_SegNeigh$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_SegNeigh$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_SegNeigh$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}
grid_hubert <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_hubert$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = "Hubert"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_hubert$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_hubert$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_hubert$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}
grid_e_divisive <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_e_divisive$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = "E.div"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_e_divisive$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_e_divisive$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_e_divisive$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}
grid_jumpoint <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_jumpoint$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = "Jumpoint"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_jumpoint$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_jumpoint$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_jumpoint$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}
grid_cpm <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_cpm$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = "CPM"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_cpm$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_cpm$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_cpm$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}
grid_beast <- function(){
  grid.arrange(ggplot(data = values_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_beast$active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 labs(x = NULL, y = "BEAST"),
               
               ggplot(data = values_log_AC, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_beast$log_active_channel_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values, c(1,separated_data_beast$valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 labs(x = NULL, y = NULL),
               
               ggplot(data = values_log_VB, aes(x = x, y = values)) +
                 geom_line() +
                 geom_line(aes(y = model_signal(values,c(1, separated_data_beast$log_valley_bottom_width$cp_values,length(values)+1))), 
                           color = "red", size = 0.7) +
                 theme_bw() +
                 labs(x = NULL, y = NULL),
               
               ncol = 4)
}

grid.arrange(grid_pelt(),grid_BinSeg(),grid_SegNeigh(),grid_hubert(), 
             grid_e_divisive(), grid_jumpoint(), grid_cpm(), grid_beast(), 
             
             ncol = 1)

#

# Nombre de rupture avec lengths log ou non 
rupture_lenghts <- function(){
  lengths_top_1 <- lengths %>% 
    select(method, "1 - active_channel_width","2 - log_active_channel_width"
    ) %>% 
    pivot_longer(cols = -method, names_to = "measurement", values_to = "value")
  lengths_top_2 <- lengths %>% 
    select(method,
           "3 - valley_bottom_width","4 - log_valley_bottom_width") %>% 
    pivot_longer(cols = -method, names_to = "measurement", values_to = "value")
  
  grid.arrange(
    ggplot(lengths_top_1, aes(x = measurement, y = value, fill = measurement)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = value),vjust = -0.5, color = "black", size = 3)+
      facet_wrap(~method,ncol = 8, strip.position = "bottom") +  
      labs(
        x = NULL,
        y = "Nombre de rupture",
        fill = "Variable") +
      theme_minimal()+
      theme(axis.text.x = element_blank()),
    
    ggplot(lengths_top_2, aes(x = measurement, y = value, fill = measurement)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = value),vjust = -0.5, color = "black", size = 3)+
      facet_wrap(~method,ncol = 8, strip.position = "bottom") +  
      labs(
        x = NULL,
        y = "Nombre de rupture",
        fill = "Variable") +
      theme_minimal()+
      theme(axis.text.x = element_blank()),
    
    ncol = 1)
  
}
rupture_lenghts()

#
#
#

# point pour time avec l'ensemble de ceux des algo 
ggplot(algo, aes(x = as.factor(ID), y = time)) +
  geom_point(aes(color = method), size = 3, position = position_dodge(width = 0.5)) +
  labs(x = "Variables", y = "Temps de calcul en seconde") +
  theme_bw()

#
#
#

# Point pour segment_mean avec l'ensemble de ceux des algo
ggplot(algo, aes(x = as.factor(ID), y = segment_mean)) +
  geom_point(aes(color = method), size = 3, position = position_dodge(width = 0.5)) +
  labs(x = "Algo", y = "Taille moyenne de segment") +
  theme_bw()

#
#
#

# Point pour effect_size avec l'ensemble de ceux des algo
ggplot(algo, aes(x = as.factor(ID), y = effect_size_algo)) +
  geom_point(aes(color = method), size = 3, position = position_dodge(width = 0.5)) +
  labs(x = "Algo", y = "Taille de l'effet") +
  theme_bw()







# cpt en fonction de la variance 
ggplot(algo, aes(x = var, y = cpt)) +
  geom_point(aes(color = method), size = 3, position = position_dodge(width = 0.5)) +
  labs(x = "Algo", y = "Taille de l'effet") +
  theme_bw()





