# Experiment1
# photosynthesis and respiration data preparation

library(tidyverse)
library(lubridate)
library(ggpubr)
library(openxlsx)
library(dplyr)

test_Data <- read_csv2("thesisdata.csv")

# change text columns to factor
test_Data <- test_Data %>% 
  mutate_at(c("run", "jar_nr", "position", "cond_organism", "fragment_ID"),factor)  

######set time as time and date as date
test_Data$time_start_L <- hms(test_Data$time_start_L)
test_Data$time_stop_L <- hms(test_Data$time_stop_L)
test_Data$time_start_D <- hms(test_Data$time_start_D)
test_Data$time_stop_D <- hms(test_Data$time_stop_D)

test_Data$date <- dmy(test_Data$date)

# create column for incubation duration (light and dark) and oxygen difference
test_Data$time_L_min <- time_length(test_Data$time_stop_L- test_Data$time_start_L, unit = "minutes")
test_Data$time_D_min <- time_length(test_Data$time_stop_D- test_Data$time_start_D, unit = "minutes")

test_Data$oxy_L_diff <- test_Data$oxygen_stop_L-test_Data$oxygen_start_L
test_Data$oxy_D_diff <- test_Data$oxygen_start_D-test_Data$oxygen_stop_D

#####change O2 from mg/L to μg/L
test_Data$oxy_L_diff <- test_Data$oxy_L_diff*1000
test_Data$oxy_D_diff <- test_Data$oxy_D_diff*1000

######calculate volume per jar
Vol <- read_csv2("Volume.csv")

###delete unneeded columns
Vol_new<-Vol[,c("jar_nr", "weight_empty_g", "weight_full_g")]
rm(Vol)

####calculate each jars volumne (L)
Vol_new$vol_jar_L <- (Vol_new$weight_full_g-Vol_new$weight_empty_g)/1000
Vol <- Vol_new[,c("jar_nr","vol_jar_L")]
rm(Vol_new)

#####join Volume to test_Data
Data_join_complete <- merge(x = test_Data, y = Vol, by = "jar_nr")
rm(Vol, test_Data)

###############################################################################
# calculate O² production and consumption per time and Volume
Data_join_complete$base_O2produced_ug_min <- Data_join_complete$oxy_L_diff* Data_join_complete$vol_jar_L/
  Data_join_complete$time_L_min
Data_join_complete$base_O2produced_ug_h <- Data_join_complete$base_O2produced_ug_min *60

Data_join_complete$base_O2respiration_ug_min <- Data_join_complete$oxy_D_diff* Data_join_complete$vol_jar_L/
  Data_join_complete$time_D_min
Data_join_complete$base_O2respiration_ug_h <- Data_join_complete$base_O2respiration_ug_min *60

# separate control
# Treat of-species and within species controls separately
Control <- Data_join_complete %>% filter(species=="con")
Control_within <- Control %>% subset(incubation == "within-species")
Control_off <-Control %>% subset(incubation == "off-species")
rm(Control)

# separate Data_join_complete into off-species and within species
Data_off <- Data_join_complete %>% filter(incubation=="off-species")
Data_within <- Data_join_complete %>% subset(incubation == "within-species")
rm(Data_join_complete)

# Mean of two controls per day 
Control_merged_02produced <- Control_off %>% group_by(run) %>% summarise(mean(base_O2produced_ug_h))
Control_merged_respiration <- Control_off %>% group_by(run) %>% summarise(mean(base_O2respiration_ug_h))

# Merge tables
Data_con<- left_join(Data_off, Control_merged_02produced, by= "run")
Data_com <- left_join(Data_con, Control_merged_respiration, by= "run")

rm(Control_merged_02produced, Control_merged_respiration, Control_off, Data_con, Data_off)

# 1 Control per day in species self conditioned control -> only one control per species and day
# -> no mean value
# base_O2produced_mg_h und base_O2respiration_mg_h duplizieren und entsprechend umbenennen
n = 1
Control_within1 = cbind(Control_within, replicate(n,Control_within$base_O2produced_ug_h))
n = 1
Control_within2 = cbind(Control_within1, replicate(n,Control_within1$base_O2respiration_ug_h)) 

# careful with the numbers -> need to be the last to columns in the table
names(Control_within2)[43] <- "mean(base_O2produced_ug_h)"
names(Control_within2)[44] <- "mean(base_O2respiration_ug_h)"

rm(n, Control_within, Control_within1)

#split Control_within2 run 7 & 8 to join by cond_organism
Control_with7 <- Control_within2 %>% filter(run == "7")
Control_with8 <- Control_within2 %>% filter (run =="8")

rm(Control_within2)

#delete unnecessary information
Control_within7 <- Control_with7[,c("cond_organism","mean(base_O2produced_ug_h)", "mean(base_O2respiration_ug_h)")]
Control_within8 <- Control_with8[,c("cond_organism","mean(base_O2produced_ug_h)", "mean(base_O2respiration_ug_h)")]
rm(Control_with7, Control_with8)

#split Data_within in run 7 & 8 to join by cond_organism
Data_within7 <- Data_within %>% filter(run =="7")
Data_within8 <- Data_within %>% filter (run =="8")

# Merge tables 
Data_contr7 <- left_join(Data_within7, Control_within7, by = "cond_organism")
Data_contr8 <- left_join(Data_within8, Control_within8, by = "cond_organism")


####fill in mean(base_O2produced_mg_h) & mean(base_O2respiration_mg_h) for run 7 & 8 by cond_organism & run
Data_comp <- rbind(Data_contr7, Data_contr8, Data_com)

rm(Data_com, Data_contr7, Data_contr8, Data_within, Data_within7, Data_within8, Control_within7, Control_within8)

Data_comp$net_photo_ug_h <- Data_comp$base_O2produced_ug_h-Data_comp$`mean(base_O2produced_ug_h)`
Data_comp$respiration_ug_h <- Data_comp$base_O2respiration_ug_h-Data_comp$`mean(base_O2respiration_ug_h)`
Data_comp$gross_photo_ug_h <- Data_comp$net_photo_ug_h+Data_comp$respiration_ug_h

####plotten von net_photo_mg_h
frag_surface <- read.csv2("fragment.csv")

frag_surface$surface_cm2 <- frag_surface$surface_mm2/ 100

frag_sur <- frag_surface [,c("fragment_ID", "surface_cm2")]

Data_all <- merge(x = Data_comp, y = frag_sur, by ="fragment_ID")

rm(Data_comp)

####normalise photosynthesis and respiration with surface
Data_all$net_photo_ug_h_cm2 <- Data_all$net_photo_ug_h / Data_all$surface_cm2
Data_all$respiration_ug_h_cm2 <- Data_all$respiration_ug_h / Data_all$surface_cm2
Data_all$gross_photo_ug_h_cm2 <- Data_all$net_photo_ug_h_cm2 + Data_all$respiration_ug_h_cm2

# Delete controls
Data_all <- Data_all %>%  subset(species != "con")

# Delete unnecessary objects
rm(frag_surface, frag_sur)

######save table
write.csv(Data_all, file = "data_prep.csv")
