#Experiment2 data preparation
library(tidyverse)
library(lubridate)
library(dplyr)
library(oce)

# Preparation of photosynthesis and respiration
# read in experiment data
test_Data <- read_csv("Exp2_oxy_raw_data.csv")

test_Data <- test_Data %>% 
  mutate_at(c("day", "position", "Category", "fragment_ID"),factor)  

test_Data$time_start_L <- hms(test_Data$time_start_L)
test_Data$time_end_L <- hms(test_Data$time_end_L)
test_Data$time_start_D <- hms(test_Data$time_start_D)
test_Data$time_end_D <- hms(test_Data$time_end_D)

test_Data$date <- ymd(test_Data$date)

test_Data$time_L_min <- time_length(test_Data$time_end_L- test_Data$time_start_L, unit = "minutes")
test_Data$time_D_min <- time_length(test_Data$time_end_D- test_Data$time_start_D, unit = "minutes")

test_Data$oxy_L_diff <- test_Data$O2_end_L-test_Data$O2_start_L
test_Data$oxy_D_diff <- test_Data$O2_start_D-test_Data$O2_end_D

#change O2 from mg/L to ug/L
test_Data$oxy_L_diff <- test_Data$oxy_L_diff*1000
test_Data$oxy_D_diff <- test_Data$oxy_D_diff*1000

#calculate volume per jar
Vol <- read_csv("Jar_volume.csv")
Vol$vol_jar_L <- (Vol$Full-Vol$Empty)/1000
Vol_new <- Vol[,c("position","vol_jar_L")]
rm(Vol)

#join Volume to test_Data
Data_join_complete <- merge(x = test_Data, y = Vol_new, by = "position")
rm(Vol_new, test_Data)

#O² production and consumption per time and Volume
Data_join_complete$base_O2produced_ug_min <- Data_join_complete$oxy_L_diff* Data_join_complete$vol_jar_L/
  Data_join_complete$time_L_min
Data_join_complete$base_O2produced_ug_h <- Data_join_complete$base_O2produced_ug_min *60

Data_join_complete$base_O2respiration_ug_min <- Data_join_complete$oxy_D_diff* Data_join_complete$vol_jar_L/
  Data_join_complete$time_D_min
Data_join_complete$base_O2respiration_ug_h <- Data_join_complete$base_O2respiration_ug_min *60

# separate controls
Control <- Data_join_complete %>% filter(fragment_ID=="Contr")

# Mean of two controls per day 
Control_merged_02produced <- Control %>% group_by(day) %>% summarise(mean(base_O2produced_ug_h))
Control_merged_respiration <- Control %>% group_by(day) %>% summarise(mean(base_O2respiration_ug_h))

# Merge tables
Data_con <- left_join(Data_join_complete, Control_merged_02produced, by= "day")
Data_com <- left_join(Data_con, Control_merged_respiration, by= "day")

rm(Control_merged_02produced, Control_merged_respiration, Data_join_complete, Data_con)

#Corrected oxygen production Sample = Oxygen production Sample - Oxygen production Control
Data_com$net_photo_ug_h <- Data_com$base_O2produced_ug_h-Data_com$`mean(base_O2produced_ug_h)`
Data_com$respiration_ug_h <- Data_com$base_O2respiration_ug_h-Data_com$`mean(base_O2respiration_ug_h)`
Data_com$gross_photo_ug_h <- Data_com$net_photo_ug_h+Data_com$respiration_ug_h

###include coral surface for normalization
frag_surface <- read_csv2("Surface.csv")
frag_surface$surface_cm2 <- frag_surface$Surface/ 100
frag_surface$volume_cm3 <- frag_surface$Volume/ 1000
frag_sur <- frag_surface [,c("fragment_ID", "surface_cm2")]
Data_all <- merge(x = Data_com, y = frag_sur, by ="fragment_ID")
frag_vol <- frag_surface [,c("fragment_ID", "volume_cm3")]
Data_all <- merge(x = Data_all, y = frag_vol, by ="fragment_ID")
rm(Data_com, frag_sur, frag_vol, frag_surface)

####normalise photosynthesis and respiration with surface
Data_all$net_photo_ug_h_cm2 <- Data_all$net_photo_ug_h / Data_all$surface_cm2
Data_all$respiration_ug_h_cm2 <- Data_all$respiration_ug_h / Data_all$surface_cm2
Data_all$gross_photo_ug_h_cm2 <- Data_all$net_photo_ug_h_cm2 + Data_all$respiration_ug_h_cm2

# Delete controls
Data_all <- Data_all %>%  subset(fragment_ID != "Contr")

rm(Control)

######save table
write.csv(Data_all, file = "data_prep_exp2.csv")

rm(Data_all)

#_____________________________________
#calculation of total alcalinity
#_____________________________________
listof <- list.files(path="Alkalinity", recursive=T, pattern="*.csv", full.names = T) %>%
  str_subset(pattern=".*/CSV/.*") 

# drop all pathways which are not the titration output (e.g. results calculated by titrator)
listoff <- listof %>%
  str_subset(., "Alkalinity_Gran_0_1N(?=_)")


# Import all files in a single tibble
read_flnm <- function(x) {
  read_delim(x, delim = ";",
             locale = readr::locale(encoding = "latin1")) %>% 
    mutate(Filename = x)}

Flnm <- listoff %>% map_df(~read_flnm(.)) 

# delete unnecessary part of the filename in each row
Flnm$Filename <- str_sub(Flnm$Filename, -42)

# Format in a more convenient way
#error in rename
Flnm_clean <- Flnm %>%
  # replace "-" 'cause R sees it as an operator ...
  mutate(Filename = str_replace(Filename, "-", "_")) %>% 
  #remove .csv
  mutate(Filename = gsub(".csv", "", Filename)) %>% 
  #remove []
  mutate(Filename = gsub("\\[1]", "", Filename)) %>% 
  #delete empty space
  mutate(Filename = gsub(" ", "", Filename)) %>% 
  #change Temperature name
  rename(Temp = "°C")                                            

rm(listof, listoff, read_flnm, Flnm)



## Select (and then read) only summary file based on (its) name length 
listof <- list.files(path="Alkalinity", recursive=T, pattern="*.csv", full.names = T) %>%
  str_subset(pattern=".*/CSV/.*") 

# drop all pathways which are not the summary
listoff <- listof %>%
  str_subset(., "Alkalinity_Gran_0_1N\\.") #names that have Alkalinity_Gran_0_1N


# Import all files in a single tibble
read_flnm <- function(x) {
  read_delim(x, delim = ";",
             locale = readr::locale(encoding = "latin1"),
             col_types = list(`EP [ml]` = col_character(), # map_df gives this warning Can't combine `EP [ml]` <double> and `EP [ml]` <character>. Therefore changed them here to characters
                              `EP [pH]` = col_character()))%>% 
    mutate(Filename = x)
  
}

summary <- listoff %>% map_df(~read_flnm(.))  


# Delim should be ";" (but once was "," ... )
# (with this specif.s no need to change the locale for characters as it skipts the painful row)
#Summary_0 <- read_delim(summaryfile, delim = Sep, col_names = FALSE, skip = 1) 

rm(listof, listoff, read_flnm)  



## Create tibble "Summary" with only the stuff that we need, in the right format

# Function to correct nchar of datetime parts (e.g. "1" becomes "01" -> keep same nr of char)
addzero <- function(vrbl) {
  ifelse((nchar(vrbl) == 1), paste("0", vrbl, sep = ""), vrbl)
}


# From datetime to filename format (to match with Filename in Flnm_all) ...
Summary <- summary %>%
  select('Method name', 'Sample ID', 'End time') %>%  
  rename(Methodname = 'Method name',
         Sample_ID = 'Sample ID',
         Timetitr_end = 'End time')%>%
  mutate(
    Timetitr_end = lubridate::as_datetime(lubridate::mdy_hms(Timetitr_end)),
    Day = lubridate::day(Timetitr_end),
    Month = lubridate::month(Timetitr_end),
    Year = lubridate::year(Timetitr_end),
    Hour = lubridate::hour(Timetitr_end),
    Mins = lubridate::minute(Timetitr_end),
    Sec = lubridate::second(Timetitr_end),
    date = lubridate::date(Timetitr_end)) %>%
  mutate_at(c("Day", "Month", "Hour", "Mins", "Sec"), addzero) %>%
  mutate(Year = as.character(Year),
         Year = str_sub(Year, start = 3, end = 4),
         Methodname = stringr::str_replace(Methodname, "0.1N", "0_1N")) %>%
  unite("Filename", c(Methodname, Day, Month, Year, Hour, Mins, Sec)) %>%
  
  select(c(date, Filename, Sample_ID))

rm(summary)

# get Info summary file of all titrations

SampleIDWS <- read_csv("Data/Titrator_raw_data.csv")
SampleIDWS$date <-  lubridate::ymd(SampleIDWS$date)

# - Part 1: check for typos & missing samples ---------------------------------------------------------------------
# Perform anti-join with SampleIDWS vs Summary & then reverse them (i.e. Summary vs Sample IDWS)
# & check if all samples in weights tibble are in summary & vice-versa 
# perform it with date column in both to be able to trace back the mismatch 

a <- SampleIDWS %>%
  select(date, Sample_ID) %>%
  anti_join(Summary %>% #all the rows in x (SampleIDWs) without a match in y (Summary)
              select(date, Sample_ID))

rm(a)

#The code in previous version of the script for performing data checks won't work 
# if you have multiple titrations where the same samples were measured
#Potential Alternative to that warning code section 
Nr_matchingSIDs <- length(intersect(SampleIDWS$Sample_ID, Summary$Sample_ID)) 
Nr_SIDs <- length(unique(SampleIDWS$Sample_ID))

ifelse(Nr_matchingSIDs - Nr_SIDs == 0, "All good, can continue",
       stop("*** Sample IDs don't match ***\n -> cross-check the 2 tibbles to find the mismatch", call. = F ))

#This will tell you if you have all your samples (for which you imported the sample weight) 
# also in the titration file. But if you forgot to transfer a sample & it's not in the
# weights' tibble (but you have its titration), it will go unnoticed in this warning.



# - Part 2: assemble ---------------------------------------------------------------------

# Create a new tibble (Titr_Flnm_SID_Ws) with all your data
Titr_Flnm_SIDWS <- Flnm_clean %>%
  # Join the csv data (which has the actual titration) with Sample_ID by Filename 
  left_join(Summary, by = ("Filename")) %>%
  
  # Now also join it with the sample IDs & weight (Ws) data by date & sample ID
  # make sure sample IDs are in the same format
  left_join(SampleIDWS, by = c("Sample_ID", "date")) 

# Convert the tibble into a list of tibbles, where each tibble contains a single
# titration & the name of the tibble in the list is its Filename

# Function to split the tibble by group (Filename)
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}

# Create list of tibbles
Titr_Flnm_SIDWS_List <- Titr_Flnm_SIDWS %>% 
  named_group_split(Filename)


# Titrant (HCl) normality always NHCl <- 0.1
# Pressure was constant at 1013 hPa, needed in dbar for formula. Therefore 10.13253 dbar (keep 4 decimals for formula)

TA_Gran <- function(., N = 0.1, Ws, pH, Vt, Sal, Temp) { 
  
  options(digits = 5)
  # swd = seawater density
  swd <- oce::swRho(salinity = Sal, temperature = Temp, pressure = 10.186)/1000 # g/cm3 * 1000 = kg/m3
  # Vs = volume of the sample
  Vs <- Ws * swd   # weight * density                                     
  # p <- data.frame(pH = pH, Vt = Vt (Vt = ml)
  p <- tibble(pH = pH, Vt = Vt)
  z <- p
  iii <- which((3.5 <= p$pH) & (p$pH <= 4.5))
  z <- p[iii,]
  Vtt <- z$Vt
  z <- z$pH   
  F1 <- (Vs + Vtt) * 10^-z
  f <- lm(Vtt ~ F1)
  Ve <- coef(f)[1]
  TA <- Ve * 1000 * N / Vs
  return(TA)
  attributes(TA) <- NULL
  attr(TA, "unit") <- "mmol/L"
  attr(TA, "name") <- "Total Alkalinity from Gran approximation"
  
}


# Calculate TA ---------------------------------------------------------------------- 

# create a vector with the names of the list-tibble to iterate over
list_names <- names(Titr_Flnm_SIDWS_List)

# calculate TA & add it to each tibble as another column
tab <- map_dfr(.x = list_names,
               .f = ~ {
                 # error_frame <<- .x  # if function fails and you want to know where the error occured use this
                 Titr_Flnm_SIDWS_List %>%
                   pluck(.x) %>%
                   mutate(TA_mmolL = TA_Gran(pH = pH, Vt = ml, Ws = Sample_volume[1], 
                                             Temp = mean(Temp), 
                                             S = Sal[1])) %>%
                   select(-c(ml:Temp, Sample_volume)) %>%
                   distinct()
               })


# save 
write.csv(tab, "Data/TA_Results.csv")



#_______________________________________________________
# calculate symbiont cell density
#_______________________________________________________
Dinos <- read_csv2("Symbionts.csv")

# delete outliers that occured during lab work
Dinos <- Dinos %>% 
  filter(fragment_ID != "Amu_C1_M") %>%
  filter(fragment_ID != "Pru_A2_M") %>%
  filter(fragment_ID != "Pru_B2_M") %>%
  filter(fragment_ID != "Pve_A2_P") %>%
  filter(fragment_ID != "Pve_B1_M")

# date as date
Dinos$Date <- dmy(Dinos$Date)

# calculate mean per fragment and 5 squares
Dinos$mean_cells <- (Dinos$count_1+Dinos$count_2+Dinos$count_3+Dinos$count_4+Dinos$count_5+Dinos$count_6)/6

###include coral surface for normalization
frag_surface <- read_csv2("Surface.csv")
frag_surface$surface_cm2 <- frag_surface$Surface/ 100
frag_sur <- frag_surface [,c("fragment_ID", "surface_cm2")]
Dinos <- merge(x = Dinos, y = frag_sur, by ="fragment_ID")
rm(frag_sur, frag_surface)

# calculate cells per ml
Dinos$cells_ml <- Dinos$mean_cells*50000

# cells/cm²
Dinos$cells_cm2 <- (Dinos$cells_ml*Dinos$resuspention_ml)/Dinos$surface_cm2

# save 
write.csv(Dinos, "Symbionts_prep.csv")

