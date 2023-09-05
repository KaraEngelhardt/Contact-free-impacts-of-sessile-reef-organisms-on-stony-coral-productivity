# statistical analysis
library(tidyverse)
library(rstatix)
library(car)

# overview analysis (shown in Figure 2) 
#________________________________________

# read in data_prep produced in Exp1_data_preparation
calc <- read_csv("data_prep.csv")
calc_all <- calc [,c(2:52)]
rm(calc)

calc_all <- mutate_if(calc_all, 
                      is.character, 
                      str_replace_all, pattern = "within-species", replacement = "self-conditioned")


calc_all <- mutate_if(calc_all, 
                      is.character, 
                      str_replace_all, pattern = "off-species", replacement = "external-conditioned")

# separate into incubated species
Pru <- calc_all %>% filter(species == "Pru")
Pve <- calc_all %>% filter(species == "Pve")
Spi <- calc_all %>% filter(species == "Spi")

#________________________________________
# Porites rus
Pru_self <- Pru %>% filter(incubation == "self-conditioned")
Pru_external <- Pru %>% filter(incubation == "external-conditioned")

# Pru Netto self
shapiro.test(Pru_self$net_photo_ug_h_cm2)
#Pru Netto external
shapiro.test(Pru_external$net_photo_ug_h_cm2)
# not normally distributed

# add transformation
Pru_self$net_log <- log(Pru_self$net_photo_ug_h_cm2)
Pru_external$net_log <- log(Pru_external$net_photo_ug_h_cm2)

# test normality with transformation
shapiro.test(Pru_self$net_log)
shapiro.test(Pru_external$net_log)
# normal distributed

# add log transformation net to table Pru
Pru$net_log <- log(Pru$net_photo_ug_h_cm2)
Pru$net_sqrt <- sqrt(Pru$net_photo_ug_h_cm2)

# test for equality of variance
leveneTest(net_log ~ incubation, data = Pru)
# not equal in variance

# wilcoxon rank-sum test normal data
Pru %>%
  rstatix::wilcox_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant

################
# Pru Gross self
shapiro.test(Pru_self$gross_photo_ug_h_cm2)

# Pru Gross external
shapiro.test(Pru_external$gross_photo_ug_h_cm2)
# not normally distributed

# add transformation
Pru_self$gross_log <- log(Pru_self$gross_photo_ug_h_cm2)
Pru_external$gross_log <- log(Pru_external$gross_photo_ug_h_cm2)

# test normality with transformation Pru_netto
shapiro.test(Pru_self$gross_log)
shapiro.test(Pru_external$gross_log)
# normal distributed

# add log transformation gross to table Pru
Pru$gross_log <- log(Pru$gross_photo_ug_h_cm2)
Pru$gross_sqrt <- sqrt(Pru$gross_photo_ug_h_cm2)

# test for equality of variance separated for self & external
leveneTest(gross_log ~ incubation, data = Pru)
leveneTest(gross_sqrt ~ incubation, data = Pru)
# no equality of variance

# wilcoxon rank-sum test normal data
Pru %>%
  rstatix::wilcox_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")

# significant*

###############
# Pru Resp self
shapiro.test(Pru_self$respiration_ug_h_cm2)

#Pru Resp external
shapiro.test(Pru_external$respiration_ug_h_cm2)
# normal distributed

# test for equality of variance
leveneTest(respiration_ug_h_cm2 ~ incubation, data = Pru)
# equality of variance

# unpaired t-test normal data
Pru %>%
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                  p.adjust.method = "bonferroni")

# significant*
#remove Porites rus data
rm(Pru, Pru_external, Pru_self)

#___________________________________
# Pocillopora verrucosa
Pve_self <- Pve %>% filter(incubation == "self-conditioned")
Pve_external <- Pve %>% filter(incubation == "external-conditioned")

# Pve Netto self
shapiro.test(Pve_self$net_photo_ug_h_cm2)
#Pve Netto external
shapiro.test(Pve_external$net_photo_ug_h_cm2)
# normal distributed

# add log transformation net to table Pve
Pve$net_log <- log(Pve$net_photo_ug_h_cm2)
Pve$net_sqrt <- sqrt(Pve$net_photo_ug_h_cm2)

# test for equality of variance
leveneTest(net_photo_ug_h_cm2 ~ incubation, data = Pve)
# no equality of variance with normal data or transformation

# wilcox rank-sum test normal data
Pve %>%
  rstatix::wilcox_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# significant *

################
# Pve Gross self
shapiro.test(Pve_self$gross_photo_ug_h_cm2)
#Pve Gross external
shapiro.test(Pve_external$gross_photo_ug_h_cm2)
# normal distributed

# add log transformation gross to table Pve (to try equality of variance with the tranformations)
Pve$gross_log <- log(Pve$gross_photo_ug_h_cm2)
Pve$gross_sqrt <- sqrt(Pve$gross_photo_ug_h_cm2)

# test for equality of variance
leveneTest(gross_photo_ug_h_cm2 ~ incubation, data = Pve)
# no equality of variance with normal data or transformation

# wilcoxon rank-sum test normal data
Pve %>%
  rstatix::wilcox_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# significant*

###############
# Pve Resp self
shapiro.test(Pve_self$respiration_ug_h_cm2)
#Pve Resp external
shapiro.test(Pve_external$respiration_ug_h_cm2)
# not normally distributed

#add transformation log
Pve_self$resp_log <- log(Pve_self$respiration_ug_h_cm2)
Pve_external$resp_log <- log(Pve_external$respiration_ug_h_cm2)

#test normality with transformation Pve_netto
shapiro.test(Pve_self$resp_log)
shapiro.test(Pve_external$resp_log)
# normal distributed

#add transformation sqrt
Pve_self$resp_sqrt <- sqrt(Pve_self$respiration_ug_h_cm2)
Pve_external$resp_sqrt <- sqrt(Pve_external$respiration_ug_h_cm2)

# add sqrt transformation resp to table Pve
Pve$resp_sqrt <- sqrt(Pve$respiration_ug_h_cm2)

# test for equality of variance
leveneTest(resp_sqrt ~ incubation, data = Pve)
# no equality of variance with normal data or transformation

# wilcoxon rank-sum test normal data
Pve %>%
  rstatix::wilcox_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant

#remove P. verrucosa data
rm(Pve, Pve_external, Pve_self)

#__________________________________
# Stylophora pistillata
Spi_self <- Spi %>% filter(incubation == "self-conditioned")
Spi_external <- Spi %>% filter(incubation == "external-conditioned")

# Spi Netto self
shapiro.test(Spi_self$net_photo_ug_h_cm2)
# Spi Netto external
shapiro.test(Spi_external$net_photo_ug_h_cm2)
# not normally distributed

# add transformation
Spi_self$net_log <- log(Spi_self$net_photo_ug_h_cm2)
Spi_external$net_log <- log(Spi_external$net_photo_ug_h_cm2)

# test normality with transformation Spi_netto
shapiro.test(Spi_self$net_log)
shapiro.test(Spi_external$net_log)
# normal distributed

# add log & sqrt transformation net to table Spi
Spi$net_log <- log(Spi$net_photo_ug_h_cm2)
Spi$net_sqrt <- sqrt(Spi$net_photo_ug_h_cm2)

# test for equality of variance separated for self & external
leveneTest(net_log ~ incubation, data = Spi)
# no equality of variance for normal data or transformation

# wilcoxon rank-sum test normal data
Spi %>%
  rstatix::wilcox_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant

################
# Spi Gross self
shapiro.test(Spi_self$gross_photo_ug_h_cm2)
# Spi Gross external
shapiro.test(Spi_external$gross_photo_ug_h_cm2)
# not normally distributed

# add transformation
Spi_self$gross_log <- log(Spi_self$gross_photo_ug_h_cm2)
Spi_external$gross_log <- log(Spi_external$gross_photo_ug_h_cm2)

# test normality with transformation
shapiro.test(Spi_self$gross_log)
shapiro.test(Spi_external$gross_log)
# not normal distributed

# add transformation
Spi_self$gross_sqrt <- sqrt(Spi_self$gross_photo_ug_h_cm2)
Spi_external$gross_sqrt <- sqrt(Spi_external$gross_photo_ug_h_cm2)

# test normality with transformation Spi_gross
shapiro.test(Spi_self$gross_sqrt)
shapiro.test(Spi_external$gross_sqrt)
# normal distributed

# add sqrt transformation gross to table
Spi$gross_sqrt <- sqrt(Spi$gross_photo_ug_h_cm2)

# test for equality of variance separated for self & external
leveneTest(gross_sqrt ~ incubation, data = Spi)
# not equality of variance

Spi %>%
  rstatix::wilcox_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                       p.adjust.method = "bonferroni")
# not significant

###############
# Spi Resp self
shapiro.test(Spi_self$respiration_ug_h_cm2)
#Spi Resp external
shapiro.test(Spi_external$respiration_ug_h_cm2)
# normal distributed

#add transformation
Spi_self$resp_log <- log(Spi_self$respiration_ug_h_cm2)
Spi_external$resp_log <- log(Spi_external$respiration_ug_h_cm2)

# test for equality of variance separated for self & external
leveneTest(respiration_ug_h_cm2 ~ incubation, data = Spi)
# equality of variance

# unpaired t-test normal data
Spi %>%
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = FALSE,
                  p.adjust.method = "bonferroni")
# not significant

#remove S. pistillata data
rm(Spi, Spi_external, Spi_self, calc_all)


#########################################
###______________________________________
###Analysis of each conditioning organism separately (shown in Figure 3)
###______________________________________
#########################################

calc <- read_csv("data_prep.csv")
calc_all <- calc [,c(2:52)]
rm(calc)

calc_all <- mutate_if(calc_all, 
                      is.character, 
                      str_replace_all, pattern = "within-species", replacement = "self-conditioned")


calc_all <- mutate_if(calc_all, 
                      is.character, 
                      str_replace_all, pattern = "off-species", replacement = "external-conditioned")


###separate in different tables to look at incubations and incubated species separately
Pru <- calc_all %>% filter(species == "Pru")
Pve <- calc_all %>% filter(species == "Pve")
Spi <- calc_all %>% filter(species == "Spi")

##Pru
#####
Pru_self<- Pru %>% filter(incubation == "self-conditioned")
Pru_external <- Pru %>% filter(incubation == "external-conditioned")

Pru_e_amu <- Pru_external %>% filter(cond_organism == "Amu")
Pru_e_csp <- Pru_external %>% filter(cond_organism == "Csp")
Pru_e_hcn <- Pru_external %>% filter(cond_organism == "Hcn")
Pru_e_hsp <- Pru_external %>% filter(cond_organism == "Hsp")
Pru_e_mdi <- Pru_external %>% filter(cond_organism == "Mdi")
Pru_e_ssp <- Pru_external %>% filter(cond_organism == "Ssp")
Pru_e_xsp <- Pru_external %>% filter(cond_organism == "Xsp")

Pru_amu <- rbind(Pru_self, Pru_e_amu)
Pru_csp <- rbind(Pru_self, Pru_e_csp)
Pru_hcn <- rbind(Pru_self, Pru_e_hcn)
Pru_hsp <- rbind(Pru_self, Pru_e_hsp)
Pru_mdi <- rbind(Pru_self, Pru_e_mdi)
Pru_ssp <- rbind(Pru_self, Pru_e_ssp)
Pru_xsp <- rbind(Pru_self, Pru_e_xsp)

rm(Pru_self, Pru_external, Pru_e_csp, Pru_e_amu, Pru_e_hcn, Pru_e_hsp, Pru_e_mdi, Pru_e_ssp, Pru_e_xsp)

# Acropora muricata netto
Netto_PSamu <- Pru_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEamu <- Pru_amu %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSamu, Netto_PEamu, by = c("fragment_ID"))

# for a paired test the difference of the groups need to meet the normality requirement
Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Pru_amu %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant**
rm(Netto, Netto_PEamu, Netto_PSamu, Dif_netto)


# Acropora muricata gross
Gross_PSamu <- Pru_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEamu <- Pru_amu %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PSamu, Gross_PEamu, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distributed

# paired t-test
Pru_amu %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant**
rm(Gross, Gross_PEamu, Gross_PSamu, Dif_gross)

# Acropora muricata respiration
Resp_PSamu <- Pru_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEamu <- Pru_amu %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSamu, Resp_PEamu, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
hist(Dif_respiration)
# normal distribution

# paired t-test
Pru_amu %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **

rm(Resp, Resp_PEamu, Resp_PSamu, Dif_respiration, Pru_amu)


###############
#######Pru and Csp
# Csp Netto
Netto_PScsp <- Pru_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEcsp <- Pru_csp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PScsp, Netto_PEcsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
hist(Dif_netto)
# normal distributed

# paired t-test
Pru_csp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Netto, Netto_PEcsp, Netto_PScsp, Dif_netto)


### Caulerpa sp. gross
Gross_PScsp <- Pru_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEcsp <- Pru_csp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEcsp, Gross_PScsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distributed

# paired t-test
Pru_csp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Gross, Gross_PEcsp, Gross_PScsp, Dif_gross)


# Caulerpa sp. respiration
Resp_PScsp <- Pru_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEcsp <- Pru_csp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PScsp, Resp_PEcsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributed

# paired t-test
Pru_csp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Resp, Resp_PEcsp, Resp_PScsp, Dif_respiration, Pru_csp)

### Pru and Haliclona cnidata
### Hcn Netto
Netto_PShcn <- Pru_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEhcn <- Pru_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PShcn, Netto_PEhcn, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Pru_hcn %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEhcn, Netto_PShcn, Dif_netto)

# Haliclona cnidata gross
Gross_PShcn <- Pru_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEhcn <- Pru_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEhcn, Gross_PShcn, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Pru_hcn %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Gross, Gross_PEhcn, Gross_PShcn, Dif_gross)

# Haliclona cnidata respiration
Resp_PShcn <- Pru_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEhcn <- Pru_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PShcn, Resp_PEhcn, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Pru_hcn %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Resp, Resp_PEhcn, Resp_PShcn, Dif_respiration, Pru_hcn)

###Pru and Halimeda sp.
###Halimeda sp. Netto
Netto_PShsp <- Pru_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEhsp <- Pru_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PShsp, Netto_PEhsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pru_hsp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Netto, Netto_PEhsp, Netto_PShsp, Dif_netto)

# Halimeda sp. Gross
Gross_PShsp <- Pru_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEhsp <- Pru_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEhsp, Gross_PShsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Pru_hsp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Gross, Gross_PEhsp, Gross_PShsp, Dif_gross)

#
# Halimeda sp. Respiration
Resp_PShsp <- Pru_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEhsp <- Pru_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PShsp, Resp_PEhsp, by = c("fragment_ID"))

Resp$Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Resp$Dif_respiration)
# check graphical
hist(Resp$Dif_respiration)
# no normal distribution

# add tranformation
Resp$resp_E_log <- log(Resp$respiration_ug_h_cm2_E)
Resp$resp_S_log <- log(Resp$respiration_ug_h_cm2_S)

Resp$Dif_respiration_log <- Resp$resp_S_log - Resp$resp_E_log

shapiro.test(Resp$Dif_respiration_log)
# check graphical
hist(Resp$Dif_respiration_log)
# normal distribution with log transformation

# add log tranformation to Pru_hsp
Pru_hsp$resp_log <- log(Pru_hsp$respiration_ug_h_cm2)

# paired t-test with log transformation
Pru_hsp %>% 
  rstatix::t_test(resp_log ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant ***
rm(Resp, Resp_PEhsp, Resp_PShsp, Dif_respiration, Pru_hsp)

#### Pru and Montipora digitata
### M. digitata Netto
Netto_PSmdi <- Pru_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEmdi <- Pru_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSmdi, Netto_PEmdi, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pru_mdi %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Netto, Netto_PEmdi, Netto_PSmdi, Dif_netto)


### M. digitata Gross
Gross_PSmdi <- Pru_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEmdi <- Pru_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEmdi, Gross_PSmdi, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distributed

# paired t-test
Pru_mdi %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant**
rm(Gross, Gross_PEmdi, Gross_PSmdi, Dif_gross)


# M digitata Respiration
Resp_PSmdi <- Pru_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEmdi <- Pru_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSmdi, Resp_PEmdi, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributed

# paired t-test
Pru_mdi %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Resp, Resp_PEmdi, Resp_PSmdi, Dif_respiration, Pru_mdi)


### Pru and Sinularia sp.
### Sinularia sp. Netto
Netto_PSssp <- Pru_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEssp <- Pru_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSssp, Netto_PEssp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pru_ssp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Netto, Netto_PEssp, Netto_PSssp, Dif_netto)

### Sinularia sp. Gross
Gross_PSssp <- Pru_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEssp <- Pru_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEssp, Gross_PSssp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Pru_ssp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Gross, Gross_PEssp, Gross_PSssp, Dif_gross)

# Sinularia sp. Respiration
Resp_PSssp <- Pru_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEssp <- Pru_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSssp, Resp_PEssp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributed

# paired t-test
Pru_ssp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant ***
rm(Resp, Resp_PEssp, Resp_PSssp, Dif_respiration, Pru_ssp)

### Pru and Xenia sp.
### Xenia sp. Netto
Netto_PSxsp <- Pru_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PExsp <- Pru_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSxsp, Netto_PExsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pru_xsp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PExsp, Netto_PSxsp, Dif_netto)

### Xsp Gross
Gross_PSxsp <- Pru_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PExsp <- Pru_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PExsp, Gross_PSxsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Pru_xsp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PExsp, Gross_PSxsp, Dif_gross)

# Xenia sp. Respiration
Resp_PSxsp <- Pru_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PExsp <- Pru_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSxsp, Resp_PExsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributed

# paired t-test
Pru_xsp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Resp, Resp_PExsp, Resp_PSxsp, Dif_respiration, Pru_xsp, Pru)

#____________________________________
##Pocillopora verrucosa and each conditioning organism
#____________________________________
Pve_self<- Pve %>% filter(incubation == "self-conditioned")
Pve_external <- Pve %>% filter(incubation == "external-conditioned")

Pve_e_amu <- Pve_external %>% filter(cond_organism == "Amu")
Pve_e_csp <- Pve_external %>% filter(cond_organism == "Csp")
Pve_e_hcn <- Pve_external %>% filter(cond_organism == "Hcn")
Pve_e_hsp <- Pve_external %>% filter(cond_organism == "Hsp")
Pve_e_mdi <- Pve_external %>% filter(cond_organism == "Mdi")
Pve_e_ssp <- Pve_external %>% filter(cond_organism == "Ssp")
Pve_e_xsp <- Pve_external %>% filter(cond_organism == "Xsp")

Pve_amu <- rbind(Pve_self, Pve_e_amu)
Pve_csp <- rbind(Pve_self, Pve_e_csp)
Pve_hcn <- rbind(Pve_self, Pve_e_hcn)
Pve_hsp <- rbind(Pve_self, Pve_e_hsp)
Pve_mdi <- rbind(Pve_self, Pve_e_mdi)
Pve_ssp <- rbind(Pve_self, Pve_e_ssp)
Pve_xsp <- rbind(Pve_self, Pve_e_xsp)

rm(Pve_self, Pve_external, Pve_e_csp, Pve_e_amu, Pve_e_hcn, Pve_e_hsp, Pve_e_mdi, Pve_e_ssp, Pve_e_xsp)

### Acropora muricata netto
Netto_PSamu <- Pve_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEamu <- Pve_amu %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSamu, Netto_PEamu, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pve_amu %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEamu, Netto_PSamu, Dif_netto)

### A. muricata gross
Gross_PSamu <- Pve_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEamu <- Pve_amu %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PSamu, Gross_PEamu, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distributed

# paired t-test
Pve_amu %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEamu, Gross_PSamu, Dif_gross)

# A. muricata Respiration
Resp_PSamu <- Pve_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEamu <- Pve_amu %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSamu, Resp_PEamu, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributed

# paired t-test
Pve_amu %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant 
rm(Resp, Resp_PEamu, Resp_PSamu, Dif_respiration, Pve_amu)

### Pve and Caulerpa sp.
### Caulerpa sp. Netto
Netto_PScsp <- Pve_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEcsp <- Pve_csp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PScsp, Netto_PEcsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pve_csp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant ***
rm(Netto, Netto_PEcsp, Netto_PScsp, Dif_netto)

### Caulerpa sp. gross
Gross_PScsp <- Pve_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEcsp <- Pve_csp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEcsp, Gross_PScsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distributed

# paired t-test
Pve_csp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant ***
rm(Gross, Gross_PEcsp, Gross_PScsp, Dif_gross)

# Caulerpa sp. Respiration
Resp_PScsp <- Pve_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEcsp <- Pve_csp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PScsp, Resp_PEcsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributed

# paired t-test
Pve_csp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEcsp, Resp_PScsp, Dif_respiration, Pve_csp)

### P. verrucosa and Haliclona cnidata
### H. cnidata Netto
Netto_PShcn <- Pve_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEhcn <- Pve_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PShcn, Netto_PEhcn, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Pve_hcn %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant ***
rm(Netto, Netto_PEhcn, Netto_PShcn, Dif_netto)

### H. cnidata gross
Gross_PShcn <- Pve_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEhcn <- Pve_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEhcn, Gross_PShcn, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distributed

# paired t-test
Pve_hcn %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant ***
rm(Gross, Gross_PEhcn, Gross_PShcn, Dif_gross)

# H. cnidata Respiration
Resp_PShcn <- Pve_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEhcn <- Pve_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PShcn, Resp_PEhcn, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Pve_hcn %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEhcn, Resp_PShcn, Dif_respiration, Pve_hcn)

### P. verrucosa and Halimeda sp.
### Halimeda sp. Netto
Netto_PShsp <- Pve_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEhsp <- Pve_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PShsp, Netto_PEhsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E
# add tranformation
Netto$net_E_log <- log(Netto$net_photo_ug_h_cm2_E)
Netto$net_S_log <- log(Netto$net_photo_ug_h_cm2_S)

Netto$net_E_sqrt <- sqrt(Netto$net_photo_ug_h_cm2_E)
Netto$net_S_sqrt <- sqrt(Netto$net_photo_ug_h_cm2_S)

Dif_netto_log <- Netto$net_S_log - Netto$net_E_log

Dif_netto_sqrt <- Netto$net_S_sqrt - Netto$net_E_sqrt

shapiro.test(Dif_netto)
shapiro.test(Dif_netto_log)
shapiro.test(Dif_netto_sqrt)
# no normal distribution

# signed-rank wilcoxon test
Pve_hsp %>%
  rstatix::wilcox_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                       p.adjust.method = "bonferroni")
# not significant 
rm(Netto, Netto_PEhsp, Netto_PShsp, Dif_netto, Dif_netto_log, Dif_netto_sqrt)

### Hsp Gross
Gross_PShsp <- Pve_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEhsp <- Pve_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEhsp, Gross_PShsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

# add tranformation
Gross$gross_E_log <- log(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_log <- log(Gross$gross_photo_ug_h_cm2_S)

Gross$gross_E_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_S)

Dif_gross_log <- Gross$gross_S_log - Gross$gross_E_log
Dif_gross_sqrt <- Gross$gross_S_sqrt - Gross$gross_E_sqrt

shapiro.test(Dif_gross)
shapiro.test(Dif_gross_log)
shapiro.test(Dif_gross_sqrt)
# no normal distribution

# signed rank wilcoxon test
Pve_hsp %>%
  rstatix::wilcox_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                       p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEhsp, Gross_PShsp, Dif_gross, Dif_gross_log, Dif_gross_sqrt)

#Halimeda sp. respiration
Resp_PShsp <- Pve_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEhsp <- Pve_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PShsp, Resp_PEhsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Pve_hsp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant **
rm(Resp, Resp_PEhsp, Resp_PShsp, Dif_respiration, Pve_hsp)

### P. verrucosa and Montipora digitata
### M. digitata Netto
Netto_PSmdi <- Pve_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEmdi <- Pve_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSmdi, Netto_PEmdi, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distributed

# paired t-test
Pve_mdi %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Netto, Netto_PEmdi, Netto_PSmdi, Dif_netto)

### M. digitata Gross
Gross_PSmdi <- Pve_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEmdi <- Pve_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEmdi, Gross_PSmdi, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

# add tranformation
Gross$gross_E_log <- log(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_log <- log(Gross$gross_photo_ug_h_cm2_S)

Gross$gross_E_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_S)

Dif_gross_log <- Gross$gross_S_log - Gross$gross_E_log
Dif_gross_sqrt <- Gross$gross_S_sqrt - Gross$gross_E_sqrt

shapiro.test(Dif_gross)
shapiro.test(Dif_gross_log)
shapiro.test(Dif_gross_sqrt)
# no normal distribution

# signed rank wilcoxon test
Pve_mdi %>%
  rstatix::wilcox_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                       p.adjust.method = "bonferroni")
# significant **
rm(Gross, Gross_PEmdi, Gross_PSmdi, Dif_gross, Dif_gross_log, Dif_gross_sqrt)

# M. digitata Respiration
Resp_PSmdi <- Pve_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEmdi <- Pve_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSmdi, Resp_PEmdi, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Pve_mdi %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEmdi, Resp_PSmdi, Dif_respiration, Pve_mdi)


####### P. verrucosa and Sinularia sp.
### Sinularia sp. netto
Netto_PSssp <- Pve_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEssp <- Pve_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSssp, Netto_PEssp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Pve_ssp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Netto, Netto_PEssp, Netto_PSssp, Dif_netto)

### Sinularia sp. Gross
Gross_PSssp <- Pve_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEssp <- Pve_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEssp, Gross_PSssp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Pve_ssp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Gross, Gross_PEssp, Gross_PSssp, Dif_gross)

# Sinularia sp. Respiration
Resp_PSssp <- Pve_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEssp <- Pve_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSssp, Resp_PEssp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

# add tranformation
Resp$resp_E_log <- log(Resp$respiration_ug_h_cm2_E)
Resp$resp_S_log <- log(Resp$respiration_ug_h_cm2_S)

Resp$resp_E_sqrt <- sqrt(Resp$respiration_ug_h_cm2_E)
Resp$resp_S_sqrt <- sqrt(Resp$respiration_ug_h_cm2_S)

Dif_respiration_log <- Resp$resp_S_log - Resp$resp_E_log
Dif_respiration_sqrt <- Resp$resp_S_sqrt - Resp$resp_E_sqrt

shapiro.test(Dif_respiration)
shapiro.test(Dif_respiration_log)
shapiro.test(Dif_respiration_sqrt)
# no normal distribution

# signed rank wilcoxon test
Pve_ssp %>%
  rstatix::wilcox_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                       p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEssp, Resp_PSssp, Dif_respiration, Pve_ssp, Dif_respiration_log, Dif_respiration_sqrt)


####### P. verrucosa and Xenia sp.
### Xenia sp. Netto
Netto_PSxsp <- Pve_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PExsp <- Pve_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSxsp, Netto_PExsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Pve_xsp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Netto, Netto_PExsp, Netto_PSxsp, Dif_netto)

### Xenia sp. Gross
Gross_PSxsp <- Pve_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PExsp <- Pve_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PExsp, Gross_PSxsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Pve_xsp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Gross, Gross_PExsp, Gross_PSxsp, Dif_gross)

# Xenia sp. Respiration
Resp_PSxsp <- Pve_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PExsp <- Pve_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSxsp, Resp_PExsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Pve_xsp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PExsp, Resp_PSxsp, Dif_respiration, Pve_xsp, Pve)

##_________________________________
##Stylophora pistillata
##_________________________________
Spi_self<- Spi %>% filter(incubation == "self-conditioned")
Spi_external <- Spi %>% filter(incubation == "external-conditioned")

Spi_e_amu <- Spi_external %>% filter(cond_organism == "Amu")
Spi_e_csp <- Spi_external %>% filter(cond_organism == "Csp")
Spi_e_hcn <- Spi_external %>% filter(cond_organism == "Hcn")
Spi_e_hsp <- Spi_external %>% filter(cond_organism == "Hsp")
Spi_e_mdi <- Spi_external %>% filter(cond_organism == "Mdi")
Spi_e_ssp <- Spi_external %>% filter(cond_organism == "Ssp")
Spi_e_xsp <- Spi_external %>% filter(cond_organism == "Xsp")

Spi_amu <- rbind(Spi_self, Spi_e_amu)
Spi_csp <- rbind(Spi_self, Spi_e_csp)
Spi_hcn <- rbind(Spi_self, Spi_e_hcn)
Spi_hsp <- rbind(Spi_self, Spi_e_hsp)
Spi_mdi <- rbind(Spi_self, Spi_e_mdi)
Spi_ssp <- rbind(Spi_self, Spi_e_ssp)
Spi_xsp <- rbind(Spi_self, Spi_e_xsp)

rm(Spi_self, Spi_external, Spi_e_csp, Spi_e_amu, Spi_e_hcn, Spi_e_hsp, Spi_e_mdi, Spi_e_ssp, Spi_e_xsp)

### Acropora muricata netto
Netto_PSamu <- Spi_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEamu <- Spi_amu %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSamu, Netto_PEamu, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_amu %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEamu, Netto_PSamu, Dif_netto)

### A. muricata Gross
Gross_PSamu <- Spi_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEamu <- Spi_amu %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PSamu, Gross_PEamu, by = c("fragment_ID"))
Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E
# add tranformation
Gross$gross_E_log <- log(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_log <- log(Gross$gross_photo_ug_h_cm2_S)

Gross$gross_E_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_S)

Dif_gross_log <- Gross$gross_S_log - Gross$gross_E_log
Dif_gross_sqrt <- Gross$gross_S_sqrt - Gross$gross_E_sqrt

shapiro.test(Dif_gross)
shapiro.test(Dif_gross_log)
shapiro.test(Dif_gross_sqrt)
# log transformation normal distribution 

# add log tranformation to Spi_amu
Spi_amu$gross_log <- log(Spi_amu$gross_photo_ug_h_cm2)

# paired t-test with log transformation
Spi_amu %>% 
  rstatix::t_test(gross_log ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEamu, Gross_PSamu, Dif_gross, Dif_gross_log, Dif_gross_sqrt)

# A. muricata Respiration
Resp_PSamu <- Spi_amu %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEamu <- Spi_amu %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSamu, Resp_PEamu, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Spi_amu %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEamu, Resp_PSamu, Dif_respiration, Spi_amu)


### S. pistillata and Caulerpa sp.
### Caulerpa sp. netto
Netto_PScsp <- Spi_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEcsp <- Spi_csp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PScsp, Netto_PEcsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_csp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEcsp, Netto_PScsp, Dif_netto)

### Caulerpa sp. Gross
Gross_PScsp <- Spi_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEcsp <- Spi_csp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEcsp, Gross_PScsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal ditribution

# paired t-test
Spi_csp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEcsp, Gross_PScsp, Dif_gross)

# Caulerpa sp. Respiration
Resp_PScsp <- Spi_csp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEcsp <- Spi_csp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PScsp, Resp_PEcsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Spi_csp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEcsp, Resp_PScsp, Dif_respiration, Spi_csp)


####### S. pistillata and Haliclona cnidata
### H. cnidata Netto
Netto_PShcn <- Spi_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEhcn <- Spi_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PShcn, Netto_PEhcn, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_hcn %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEhcn, Netto_PShcn, Dif_netto)

### H. cnidata Gross
Gross_PShcn <- Spi_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEhcn <- Spi_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEhcn, Gross_PShcn, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Spi_hcn %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEhcn, Gross_PShcn, Dif_gross)

# H. cnidata Respiration
Resp_PShcn <- Spi_hcn %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEhcn <- Spi_hcn %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PShcn, Resp_PEhcn, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Spi_hcn %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEhcn, Resp_PShcn, Dif_respiration, Spi_hcn)


####### S. pistillata and Halimeda sp.
### Halimeda sp. Netto
Netto_PShsp <- Spi_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEhsp <- Spi_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PShsp, Netto_PEhsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_hsp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEhsp, Netto_PShsp, Dif_netto)

### Halimeda sp. Gross
Gross_PShsp <- Spi_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEhsp <- Spi_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEhsp, Gross_PShsp, by = c("fragment_ID"))
Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E
# add tranformation
Gross$gross_E_log <- log(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_log <- log(Gross$gross_photo_ug_h_cm2_S)

Gross$gross_E_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_E)
Gross$gross_S_sqrt <- sqrt(Gross$gross_photo_ug_h_cm2_S)

Dif_gross_log <- Gross$gross_S_log - Gross$gross_E_log
Dif_gross_sqrt <- Gross$gross_S_sqrt - Gross$gross_E_sqrt

shapiro.test(Dif_gross)
shapiro.test(Dif_gross_log)
shapiro.test(Dif_gross_sqrt)
# normal distribution with log transformation

# add log tranformation to Spi_hsp
Spi_hsp$gross_log <- log(Spi_hsp$gross_photo_ug_h_cm2)

# paired t-test with log transformation
Spi_hsp %>% 
  rstatix::t_test(gross_log ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEhsp, Gross_PShsp, Dif_gross, Dif_gross_log, Dif_gross_sqrt)


# Halimeda sp. Respiration
Resp_PShsp <- Spi_hsp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEhsp <- Spi_hsp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PShsp, Resp_PEhsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Spi_hsp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEhsp, Resp_PShsp, Dif_respiration, Spi_hsp)


### S. pistillata and Montipora digitata
### M. digitata Netto
Netto_PSmdi <- Spi_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEmdi <- Spi_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSmdi, Netto_PEmdi, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_mdi %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PEmdi, Netto_PSmdi, Dif_netto)

### M. digitata Gross
Gross_PSmdi <- Spi_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEmdi <- Spi_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEmdi, Gross_PSmdi, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Spi_mdi %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Gross, Gross_PEmdi, Gross_PSmdi, Dif_gross)

# M. digitata Respiration
Resp_PSmdi <- Spi_mdi %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEmdi <- Spi_mdi %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSmdi, Resp_PEmdi, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distributiom

# paired t-test
Spi_mdi %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEmdi, Resp_PSmdi, Dif_respiration, Spi_mdi)

### S. pistillata and Sinularia sp.
### Sinularia sp. Netto
Netto_PSssp <- Spi_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PEssp <- Spi_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSssp, Netto_PEssp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_ssp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Netto, Netto_PEssp, Netto_PSssp, Dif_netto)

### Sinularia sp. Gross
Gross_PSssp <- Spi_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PEssp <- Spi_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PEssp, Gross_PSssp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Spi_ssp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Gross, Gross_PEssp, Gross_PSssp, Dif_gross)

# Sinularia sp. Respiration
Resp_PSssp <- Spi_ssp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PEssp <- Spi_ssp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSssp, Resp_PEssp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Spi_ssp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PEssp, Resp_PSssp, Dif_respiration, Spi_ssp)

### S. pistillata and Xenia sp.
### Xenia sp. Netto
Netto_PSxsp <- Spi_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(net_photo_ug_h_cm2_S = net_photo_ug_h_cm2) %>% 
  select(fragment_ID, net_photo_ug_h_cm2_S)

Netto_PExsp <- Spi_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(net_photo_ug_h_cm2_E = net_photo_ug_h_cm2) %>%
  select(fragment_ID, net_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Netto <- full_join(Netto_PSxsp, Netto_PExsp, by = c("fragment_ID"))

Dif_netto <- Netto$net_photo_ug_h_cm2_S - Netto$net_photo_ug_h_cm2_E

shapiro.test(Dif_netto)
# check graphical
hist(Dif_netto)
# normal distribution

# paired t-test
Spi_xsp %>% 
  rstatix::t_test(net_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Netto, Netto_PExsp, Netto_PSxsp, Dif_netto)

### Xenia sp. Gross
Gross_PSxsp <- Spi_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(gross_photo_ug_h_cm2_S = gross_photo_ug_h_cm2) %>% 
  select(fragment_ID, gross_photo_ug_h_cm2_S)

Gross_PExsp <- Spi_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(gross_photo_ug_h_cm2_E = gross_photo_ug_h_cm2) %>%
  select(fragment_ID, gross_photo_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Gross <- full_join(Gross_PExsp, Gross_PSxsp, by = c("fragment_ID"))

Dif_gross <- Gross$gross_photo_ug_h_cm2_S - Gross$gross_photo_ug_h_cm2_E

shapiro.test(Dif_gross)
# check graphical
hist(Dif_gross)
# normal distribution

# paired t-test
Spi_xsp %>% 
  rstatix::t_test(gross_photo_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# significant *
rm(Gross, Gross_PExsp, Gross_PSxsp, Dif_gross)

# Xsp Respiration
Resp_PSxsp <- Spi_xsp %>% filter(incubation == "self-conditioned") %>% 
  rename(respiration_ug_h_cm2_S = respiration_ug_h_cm2) %>% 
  select(fragment_ID, respiration_ug_h_cm2_S)

Resp_PExsp <- Spi_xsp %>% filter(incubation == "external-conditioned") %>%
  rename(respiration_ug_h_cm2_E = respiration_ug_h_cm2) %>%
  select(fragment_ID, respiration_ug_h_cm2_E, cond_organism)

# merge together by fragment ID
Resp <- full_join(Resp_PSxsp, Resp_PExsp, by = c("fragment_ID"))

Dif_respiration <- Resp$respiration_ug_h_cm2_S - Resp$respiration_ug_h_cm2_E

shapiro.test(Dif_respiration)
# check graphical
hist(Dif_respiration)
# normal distribution

# paired t-test
Spi_xsp %>% 
  rstatix::t_test(respiration_ug_h_cm2 ~ incubation, detailed = TRUE, paired = TRUE,
                  p.adjust.method = "bonferroni")
# not significant
rm(Resp, Resp_PExsp, Resp_PSxsp, Dif_respiration, Spi_xsp, Spi)
