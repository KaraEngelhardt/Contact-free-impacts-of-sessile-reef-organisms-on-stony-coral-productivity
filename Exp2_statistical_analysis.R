#Experiment 2 statistical analysis
library(tidyverse)
library(rstatix)
library(reshape2)
library(car)
library(coin)

# read in oxygen data calculated in Exp2_data_preparation
oxy_calc <- read_csv("data_prep_exp2.csv")

oxy_calc <- oxy_calc %>% 
  filter(fragment_ID != "Spi_A2_P") %>%
  filter(fragment_ID != "Spi_A2_M")

oxy_calc <- oxy_calc %>% 
  mutate_at(c("Species", "Category"),factor)  

oxy_calc <- oxy_calc %>% select(-c(...1))

#Analysis of Net photosynthesis
Netto_M <- oxy_calc %>% filter(Category == "M") %>% 
  rename(net_photo_ug_h_cm2_M = net_photo_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, net_photo_ug_h_cm2_M, Species)

Netto_P <- oxy_calc %>% filter(Category == "P") %>% 
  rename(net_photo_ug_h_cm2_P = net_photo_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, net_photo_ug_h_cm2_P)

Netto <- full_join(Netto_M, Netto_P, by = c("Colony"))

#check if data for normal distributed
Dif_netto <- Netto$net_photo_ug_h_cm2_M - Netto$net_photo_ug_h_cm2_P

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
oxy_calc %>% 
  group_by(Species) %>% 
  rstatix::pairwise_t_test(net_photo_ug_h_cm2 ~ Category, detailed = TRUE, paired = TRUE,
                           p.adjust.method = "bonferroni")
# significant: P. rus **, P. verrucosa **
# not significant: A. muricata, M. digitata, S. pistillata

# Analysis of Respiration
Respi_M <- oxy_calc %>% filter(Category == "M") %>% 
  rename(respiration_ug_h_cm2_M = respiration_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, respiration_ug_h_cm2_M, Species)

Respi_P <- oxy_calc %>% filter(Category == "P") %>% 
  rename(respiration_ug_h_cm2_P = respiration_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, respiration_ug_h_cm2_P)

Respi <- full_join(Respi_M, Respi_P, by = c("Colony"))

Dif_respi <- Respi$respiration_ug_h_cm2_M - Respi$respiration_ug_h_cm2_P

shapiro.test(Dif_respi)
# check graphical
hist(Dif_respi)
# no normal distribution

# wilcoxon signed-rank test
oxy_calc %>% 
  group_by(Species) %>% 
  rstatix::pairwise_wilcox_test(respiration_ug_h_cm2 ~ Category, detailed = TRUE, paired = TRUE,
                                p.adjust.method = "bonferroni")
# no significance for all species

# Analysis of Gross photo
Gross_M <- oxy_calc %>% filter(Category == "M") %>% 
  rename(gross_photo_ug_h_cm2_M = gross_photo_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, gross_photo_ug_h_cm2_M, Species)

Gross_P <- oxy_calc %>% filter(Category == "P") %>% 
  rename(gross_photo_ug_h_cm2_P = gross_photo_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, gross_photo_ug_h_cm2_P)

Gross <- full_join(Gross_M, Gross_P, by = c("Colony"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_M - Gross$gross_photo_ug_h_cm2_P

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
oxy_calc %>% 
  group_by(Species) %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ Category, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant: P. verrucosa*
# not significant: A. muricata, M. digitata, P. rus, S. pistillata

rm(list = ls())


#_________________________________
# Analysis of calcification
#_________________________________
Data <- read_csv(file = "Calcification.csv") %>% 
  mutate(Category = as.factor(Category)) %>% 
  mutate(Species = as.factor(Species))

Data <- Data %>% mutate(Colony = str_sub(fragment_ID, start = -4, end=-4))

# as we excluded one Pve_A2_P, Pve_D1_M, Mdi_C1_P and one Spi_A1_P, we also excluded one colony fragment each from the monoculture for the paired t test
Data_sub <- Data %>%  filter(fragment_ID != "Pve_A2_M" & fragment_ID != "Pve_D1_P" & fragment_ID != "Spi_A1_M" & fragment_ID != "Mdi_C1_M")

#Prepare Dataset
Stat_Data_M <- Data_sub %>% filter(Category == "M") %>% 
  rename(Calc_umol_cm2_h_M = Calc_umol_cm2_h) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-4)) %>% 
  select(Colony, Calc_umol_cm2_h_M, Species)

Stat_Data_P <- Data_sub %>% filter(Category == "P") %>% 
  rename(Calc_umol_cm2_h_P = Calc_umol_cm2_h) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-4)) %>% 
  select(Colony, Calc_umol_cm2_h_P)

Stat_Data <- full_join(Stat_Data_M, Stat_Data_P, by = c("Colony"))

#sample size is < 30, thus check if data for normal distributed
# for a paired t est the difference of the groups need to meet the normality requirement
Dif <-  Stat_Data$Calc_umol_cm2_h_M - Stat_Data$Calc_umol_cm2_h_P

shapiro.test(Dif)
# check graphical
hist(Dif)
# normal distribution

# paired t-test
Data_sub %>%  group_by(Species) %>%  
  rstatix::pairwise_t_test(
    Calc_umol_cm2_h ~ Category,
    paired = TRUE,
    detailed = TRUE)
# significant: A. muricata *, P. rus *
# not significant: M. digitata, P. verrucosa, S. pistillata

rm(Data, Data_sub, Stat_Data, Stat_Data_M, Stat_Data_P, Dif)

#__________________________________________________
# Symbiont cell density analysis
#__________________________________________________
# read in data from Exp2_data_preparation
Dinos <- read_csv("Symbionts_prep.csv")

# separate into incubated species
Pru <- Dinos %>% filter(Species == "Pru")
Pve <- Dinos %>% filter(Species == "Pve")
Spi <- Dinos %>% filter(Species == "Spi")
Amu <- Dinos %>% filter(Species == "Amu")
Mdi <- Dinos %>% filter(Species == "Mdi")


# unequal samples per group -> unpaired tests

# P. rus
Pru_m <- Pru %>% filter(Category == "M")
Pru_p <- Pru %>% filter(Category == "P")

# Pru mono
shapiro.test(Pru_m$cells_cm2)
shapiro.test(Pru_p$cells_cm2)
# not normally distributed

#add log transformation
Pru_m$cells_log <- log(Pru_m$cells_cm2)
Pru_p$cells_log <- log(Pru_p$cells_cm2)
 
shapiro.test(Pru_m$cells_log)
shapiro.test(Pru_p$cells_log)
# not normal distributed

# add sqrt transformation
Pru_m$cells_sqrt <- sqrt(Pru_m$cells_cm2)
Pru_p$cells_sqrt <- sqrt(Pru_p$cells_cm2)
 
shapiro.test(Pru_m$cells_sqrt)
shapiro.test(Pru_p$cells_sqrt)
# not normally distributed
# data not normally distributed

# wilcoxon rank-sum test normal data
Pru %>%
  rstatix::wilcox_test(cells_cm2 ~ Category, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant
rm(Pru, Pru_m, Pru_p)


# P. verrucosa
Pve_m <- Pve %>% filter(Category == "M")
Pve_p <- Pve %>% filter(Category == "P")

# Pve Netto mono
shapiro.test(Pve_m$cells_cm2)
shapiro.test(Pve_p$cells_cm2)
# normal distribution

#add log transformation
Pve_m$cells_log <- log(Pve_m$cells_cm2)
Pve_p$cells_log <- log(Pve_p$cells_cm2)
shapiro.test(Pve_m$cells_log)
shapiro.test(Pve_p$cells_log)
# normal distributed

# add log and sqrt transformation for Pve
Pve$cells_log <- log(Pve$cells_cm2)
Pve$cells_sqrt <- sqrt(Pve$cells_cm2)

# test for equality of variance separated for self & external
leveneTest(cells_cm2 ~ Category, data = Pve)
leveneTest(cells_log ~ Category, data = Pve)
leveneTest(cells_sqrt ~ Category, data = Pve)
# no equality of variance

# wilcoxon rank-sum test
Pve %>%
  rstatix::wilcox_test(cells_cm2 ~ Category, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant
rm (Pve, Pve_m, Pve_p)


# A. muricata
Amu_m <- Amu %>% filter(Category == "M")
Amu_p <- Amu %>% filter(Category == "P")


shapiro.test(Amu_m$cells_cm2)
shapiro.test(Amu_p$cells_cm2)
# normal distribution

# levene test
leveneTest(cells_cm2 ~ Category, data = Amu)
# not equal

#add log transformation
Amu_m$cells_log <- log(Amu_m$cells_cm2)
Amu_p$cells_log <- log(Amu_p$cells_cm2)
shapiro.test(Amu_m$cells_log)
shapiro.test(Amu_p$cells_log)
# normal distribution

# add log to Amu
Amu$cells_log <- log(Amu$cells_cm2)
# levene test
leveneTest(cells_log ~ Category, data = Amu)
# not equal

# add sqrt transformation
Amu_m$cells_sqrt <- sqrt(Amu_m$cells_cm2)
Amu_p$cells_sqrt <- sqrt(Amu_p$cells_cm2)
shapiro.test(Amu_m$cells_sqrt)
shapiro.test(Amu_p$cells_sqrt)
# normal distribution

# add sqrt to Amu
Amu$cells_sqrt <- sqrt(Amu$cells_cm2)
# levene test
leveneTest(cells_sqrt ~ Category, data = Amu)
# not equal

# wilcoxon rank-sum test normal data
Amu %>%
  rstatix::wilcox_test(cells_cm2 ~ Category, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# significant*
rm(Amu, Amu_m, Amu_p)


# M. digitata
Mdi_m <- Mdi %>% filter(Category == "M")
Mdi_p <- Mdi %>% filter(Category == "P")

# Mdi
shapiro.test(Mdi_m$cells_cm2)
shapiro.test(Mdi_p$cells_cm2)
# normal distribution

# levene test
leveneTest(cells_cm2 ~ Category, data = Mdi)
# not equal

# add sqrt to Mdi
Mdi$cells_sqrt <- sqrt(Mdi$cells_cm2)
# levene test
leveneTest(cells_sqrt ~ Category, data = Mdi)
# not equal

# add log to Mdi
Mdi$cells_log <- log(Mdi$cells_cm2)
# levene test
leveneTest(cells_log ~ Category, data = Mdi)
# not equal

# wilcoxon rank-sum test normal data
Mdi %>%
  rstatix::wilcox_test(cells_cm2 ~ Category, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant
rm(Mdi, Mdi_m, Mdi_p)


# S. pistillata
Spi_m <- Spi %>% filter(Category == "M")
Spi_p <- Spi %>% filter(Category == "P")

shapiro.test(Spi_m$cells_cm2)
shapiro.test(Spi_p$cells_cm2)
# normal distribution

# levene test
leveneTest(cells_cm2 ~ Category, data = Spi)
# not equal

# add sqrt to Spi
Spi$cells_sqrt <- sqrt(Spi$cells_cm2)
# levene test
leveneTest(cells_sqrt ~ Category, data = Spi)
# not equal

# add log to Spi
Spi$cells_log <- log(Spi$cells_cm2)
# levene test
leveneTest(cells_log ~ Category, data = Spi)
# not equal

# wilcoxon rank-sum test normal data
Spi %>%
  rstatix::wilcox_test(cells_cm2 ~ Category, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant
rm(Spi, Spi_m, Spi_p, Dinos)

