#Experiment 2 statistical analysis
library(tidyverse)
library(rstatix)

# read in oxygen data calculated in Exp2_data_preparation
oxy_calc <- read_csv("data_prep_exp2.csv")

oxy_calc <- oxy_calc %>% 
  filter(fragment_ID != "Spi_A2_P") %>%
  filter(fragment_ID != "Spi_A2_M")

oxy_calc <- oxy_calc %>% 
  mutate_at(c("Species", "Category"),factor)  

oxy_calc <- oxy_calc %>% select(-c(X1))

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

#sample size is < 30, thus check if data for normal distributed
# for a paired t est the difference of the groups need to meet the normality requirement
Dif_netto <- Netto$net_photo_ug_h_cm2_M - Netto$net_photo_ug_h_cm2_P

# the p-value 0.9255 is greater than the significance level 0.05 implying that the distribution of the differences (d) are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)

# In an unpaired t-test, the variance between groups is assumed to be equal. 
# In a paired t-test, the variance is not assumed to be equal. Thus no levene test necessary

oxy_calc %>% 
  group_by(Species) %>% 
  rstatix::pairwise_t_test(net_photo_ug_h_cm2 ~ Category, detailed = TRUE, paired = TRUE,
                           p.adjust.method = "bonferroni")


# respiration
#Prepare Dataset
Respi_M <- oxy_calc %>% filter(Category == "M") %>% 
  rename(respiration_ug_h_cm2_M = respiration_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, respiration_ug_h_cm2_M, Species)

Respi_P <- oxy_calc %>% filter(Category == "P") %>% 
  rename(respiration_ug_h_cm2_P = respiration_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, respiration_ug_h_cm2_P)

Respi <- full_join(Respi_M, Respi_P, by = c("Colony"))

#sample size is < 30, thus check if data for normal distributed
# for a paired t est the difference of the groups need to meet the normality requirement
Dif_respi <- Respi$respiration_ug_h_cm2_M - Respi$respiration_ug_h_cm2_P

# the p-value 0.9255 is greater than the significance level 0.05 implying that the 
#distribution of the differences (d) are not significantly different from normal distribution. 
#In other words, we can not assume the normality.
shapiro.test(Dif_respi)
# check graphical
hist(Dif_respi)

# Wilcox Test
oxy_calc %>% 
  group_by(Species) %>% 
  rstatix::pairwise_wilcox_test(respiration_ug_h_cm2 ~ Category, detailed = TRUE, paired = TRUE,
                                p.adjust.method = "bonferroni")


# gross photo
#Prepare Dataset
Gross_M <- oxy_calc %>% filter(Category == "M") %>% 
  rename(gross_photo_ug_h_cm2_M = gross_photo_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, gross_photo_ug_h_cm2_M, Species)

Gross_P <- oxy_calc %>% filter(Category == "P") %>% 
  rename(gross_photo_ug_h_cm2_P = gross_photo_ug_h_cm2) %>% 
  mutate(Colony = stringr::str_sub(fragment_ID, end=-3)) %>% 
  select(Colony, gross_photo_ug_h_cm2_P)

Gross <- full_join(Gross_M, Gross_P, by = c("Colony"))

#sample size is < 30, thus check if data for normal distributed
# for a paired t est the difference of the groups need to meet the normality requirement
Dif_gross <- Gross$gross_photo_ug_h_cm2_M - Gross$gross_photo_ug_h_cm2_P

# the p-value 0.9255 is greater than the significance level 0.05 implying that the distribution of the differences (d) are not significantly different from normal distribution. In other words, we can assume the normality.
shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)

# In an unpaired t-test, the variance between groups is assumed to be equal. In a paired t-test, the variance is not assumed to be equal. Thus no levene test necessary

oxy_calc %>% 
  group_by(Species) %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ Category, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")

