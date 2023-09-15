#Plots Experiment 1
library(tidyverse)
library(ggplot2)
#library(reshape)
library(rstatix)
#library(patchwork)
library(lubridate)
library(ggpubr)

# read in data_prep produced in Exp1_data_preparation
calc <- read_csv("data_prep.csv")
calc_all <- calc [,c(2:52)]
rm(calc)

calc_all <- mutate_if(calc_all, 
                      is.character, 
                      str_replace_all, pattern = "within-species", replacement = "self-conditioned")


calc_all <- mutate_if(calc_all, 
                      is.character, 
                      str_replace_all, pattern = "off-species", replacement = "heterospecific-conditioned")


#__________________________
# Figure 2
#__________________________
calc_long <- pivot_longer(calc_all, net_photo_ug_h_cm2:gross_photo_ug_h_cm2)

Pru <- calc_long %>% filter(species == "Pru")
Pve <- calc_long %>% filter(species == "Pve")
Spi <- calc_long %>% filter(species == "Spi")

#Porites rus
Pru$incubation<- factor(Pru$incubation, levels = c("self-conditioned", "heterospecific-conditioned"))
Pru$name<- factor(Pru$name, levels = c("net_photo_ug_h_cm2", "respiration_ug_h_cm2", "gross_photo_ug_h_cm2"))

pru <- ggplot(Pru, aes(x = name, y = value, fill = incubation)) +
  scale_color_manual(values=c("#000000")) +
  scale_fill_manual(values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "#3366FF", size = 0.5, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.35))+
  theme_classic() +
  scale_x_discrete (labels = c("net_photo_ug_h_cm2" = "Net photosynthesis",
                               "respiration_ug_h_cm2" = "Respiration",
                               "gross_photo_ug_h_cm2" = "Gross photosynthesis")) +
  ylim(0,20)+
  labs(y = expression(Oxygen~flux~(µg~O[2]~cm^{-2}~h^{-1}))) +
  annotate("text", x = 2, y = 19, size = 7, label = c("*"), color = "black") +
  annotate("text", x = 3, y = 19, size = 7, label = c("*"), color = "black") +
  ggtitle("Porites rus") +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.6), "cm"), 
        axis.title.y = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust= 1),
        axis.title.x = element_blank(),
        plot.title=element_text(face="italic", size=12)) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) + guides(color="none")

# Pocillopora verrucosa
Pve$incubation<- factor(Pve$incubation, levels = c("self-conditioned", "heterospecific-conditioned"))
Pve$name<- factor(Pve$name, levels = c("net_photo_ug_h_cm2", "respiration_ug_h_cm2", "gross_photo_ug_h_cm2"))

pve <- ggplot(Pve, aes(x = name, y = value, fill = incubation)) +
  scale_color_manual(values=c("#000000")) +
  scale_fill_manual(values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "#00CCFF", size = 0.5, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.35))+
  theme_classic() +
  scale_x_discrete (labels = c("net_photo_ug_h_cm2" = "Net photosynthesis",
                               "respiration_ug_h_cm2" = "Respiration",
                               "gross_photo_ug_h_cm2" = "Gross photosynthesis")) +
  ylim(0,40)+
  labs(y = expression(Oxygen~flux~(µg~O[2]~cm^{-2}~h^{-1}))) +
  annotate("text", x = 1, y = 38, size = 7, label = c("*"), color = "black") +
  annotate("text", x = 3, y = 38, size = 7, label = c("*"), color = "black") +
  ggtitle("Pocillopora verrucosa") +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.6), "cm"),
        plot.title = element_text(size = 12, face = "italic"),
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust= 1),
        axis.title.x = element_blank()) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) + guides(color="none")

# Stylophora pistillata
Spi$incubation<- factor(Spi$incubation, levels = c("self-conditioned", "heterospecific-conditioned"))
Spi$name<- factor(Spi$name, levels = c("net_photo_ug_h_cm2", "respiration_ug_h_cm2", "gross_photo_ug_h_cm2"))

spi <- ggplot(Spi, aes(x = name, y = value, fill = incubation)) +
  scale_color_manual(values=c("#000000")) +
  scale_fill_manual(values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(color = "#66FFFF", size = 0.5, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.35))+
  theme_classic() +
  scale_x_discrete (labels = c("net_photo_ug_h_cm2" = "Net photosynthesis",
                               "respiration_ug_h_cm2" = "Respiration",
                               "gross_photo_ug_h_cm2" = "Gross photosynthesis")) +
  labs(y = expression(Oxygen~flux~(µg~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,70)+
  ggtitle("Stylophora pistillata") +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.6), "cm"), 
        plot.title = element_text(size = 12, face = "italic"),
        axis.title.y = element_text(size= 12, hjust = 0.5), 
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust= 1),
        axis.title.x = element_blank()) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) + guides(color="none")

arranged_Figure2 <- ggarrange(pru, pve, spi,
                                               labels = c("a)", "b)", "c)"),
                                               label.x = 0,
                                               label.y = 0.95,
                                               common.legend = TRUE,
                                               legend = "bottom",
                                               ncol = 3, nrow = 1,
                                               hjust = -1.5,
                                               vjust = -0.5) +
  theme(legend.text = element_text(size = 22))

ggsave("Figures/Figure2.png", width=11, height = 5, 
       limitsize = FALSE, dpi = 700, arranged_Figure2)

rm(pru, pve, spi, Pru, Pve, Spi, arranged_Figure2, calc_long)


#_______________________________________________
# Figure 3
#_______________________________________________
#####Porites rus Net, resp, gross
pru<-calc_all %>% 
  filter(species=="Pru")

pru$cond_organism<- factor(pru$cond_organism, levels = c("Pru", "Amu","Mdi", "Ssp","Xsp","Csp","Hsp","Hcn"))
pru$genus<- factor(pru$genus, levels = c("stony coral","soft coral","macroalgae","sponge"))

#P. rus netto
net <- ggplot(pru, aes(x = cond_organism, y = net_photo_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#3366FF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 5.95, linetype = 2, color = "darkgrey") +
  ylim(0,15)+  
  labs(y=expression(Net~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Pru" = "Porites rus", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata"))+
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 2, y = 13, size = 7, label = c("**"), color = "black") +
  annotate("text", x = 3, y = 13, size = 7, label = c("**"), color = "black") +
  annotate("text", x = 4, y = 13, size = 7, label = c("*"), color = "black") +
  annotate("text", x = 6, y = 13, size = 7, label = c("**"), color = "black") +
  annotate("text", x = 7, y = 13, size = 7, label = c("*"), color = "black") +
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

# P. rus respiration
resp <- ggplot(pru, aes(x = cond_organism, y = respiration_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#3366FF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 3.65, linetype = 2, color = "darkgrey") +
  ylim(0,7)+  
  labs(y=expression(Respiration~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Pru" = "Porites rus", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 2, y = 6.2, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 3, y = 6.2, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 4, y = 6.2, size = 7, label = c("***"), color = "black")+
  annotate("text", x = 5, y = 6.2, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 6, y = 6.2, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 7, y = 6.2, size = 7, label = c("***"), color = "black")+
  annotate("text", x = 8, y = 6.2, size = 7, label = c("**"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

# P. rus gross
gross <- ggplot(pru, aes(x = cond_organism, y = gross_photo_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#3366FF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 9.5, linetype = 2, color = "darkgrey") +
  ylim(0,20)+  
  labs(y=expression(Gross~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Pru" = "Porites rus", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 2, y = 18, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 3, y = 18, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 4, y = 18, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 6, y = 18, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 7, y = 18, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 8, y = 18, size = 7, label = c("*"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

#####merge 3 plots of P. rus
arranged_Pru <- ggarrange(net, resp, gross,
                          labels = c("a)", "b)", "c)"),
                          label.x = 0.035,
                          label.y = 0.95,
                          common.legend = TRUE,
                          legend = "none",
                          ncol = 3, nrow = 1,
                          hjust = -1.5,
                          vjust = 0) 

rm(gross, net, resp, pru)

#####P. verrucosa Net, resp, gross
pve<-calc_all %>% 
  filter(species=="Pve")

pve$cond_organism<- factor(pve$cond_organism, 
                           levels = c("Pve", "Amu","Mdi", "Ssp","Xsp","Csp","Hsp","Hcn"))

# P. verrucosa netto
net <- ggplot(pve, aes(x = cond_organism, y = net_photo_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#00CCFF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 8.95, linetype = 2, color = "darkgrey") +
  ylim(0,25)+  
  labs(y=expression(Net~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Pve" = "Pocillopora verrucosa", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 3, y = 24, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 4, y = 24, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 5, y = 24, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 6, y = 24, size = 7, label = c("***"), color = "black")+
  annotate("text", x = 8, y = 24, size = 7, label = c("***"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

# P. verrucosa respiration
resp <- ggplot(pve, aes(x = cond_organism, y = respiration_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#00CCFF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 6.7, linetype = 2, color = "darkgrey") +
  ylim(0,15)+  
  labs(y=expression(Respiration~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Pve" = "Pocillopora verrucosa", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 7, y = 14.5, size = 7, label = c("**"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

# P. verrucosa gross
gross <- ggplot(pve, aes(x = cond_organism, y = gross_photo_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#00CCFF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 14.6, linetype = 2, color = "darkgrey") +
  ylim(0,35)+  
  labs(y=expression(Gross~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Pve" = "Pocillopora verrucosa", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 3, y = 34, size = 7, label = c("**"), color = "black")+
  annotate("text", x = 4, y = 34, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 5, y = 34, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 6, y = 34, size = 7, label = c("***"), color = "black")+
  annotate("text", x = 8, y = 34, size = 7, label = c("**"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

#####merge 3 plots of P. verrucosa
arranged_Pve <- ggarrange(net, resp, gross,
                          labels = c("d)", "e)", "f)"),
                          label.x = 0.035,
                          label.y = 0.95,
                          common.legend = TRUE,
                          legend = "none",
                          ncol = 3, nrow = 1,
                          hjust = -1.5,
                          vjust = 0)

rm(gross, net, resp, pve)


#####S. pistillata Net, resp, gross
spi<-calc_all %>% 
  filter(species=="Spi")

spi$cond_organism<- factor(spi$cond_organism, levels = c("Spi", "Amu","Mdi", "Ssp","Xsp","Csp","Hsp","Hcn"))

# S. pistillata netto
net <- ggplot(spi, aes(x = cond_organism, y = net_photo_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#66FFFF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 26.1, linetype = 2, color = "darkgrey") +
  ylim(0,50)+  
  labs(x="Conditioning organism", y=expression(Net~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Spi" = "Stylophora pistillata", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 4, y = 47, size = 7, label = c("*"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))


# S. pistillata respiraton
resp <- ggplot(spi, aes(x = cond_organism, y = respiration_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#66FFFF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 12.3, linetype = 2, color = "darkgrey") +
  ylim(0,25)+  
  labs(x="Conditioning organism", y=expression(Respiration~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Spi" = "Stylophora pistillata", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))



# S. pistillata gross
gross <- ggplot(spi, aes(x = cond_organism, y = gross_photo_ug_h_cm2)) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  geom_point(aes(colour = cond_organism)) +
  scale_colour_manual(name = NULL,
                      values = c("#66FFFF","#000066", "#0000CC","#FF33FF",
                                 "#990099","#66FF33","#33CC33", "#000000")) +
  geom_vline(xintercept = 1.5, linetype = 1, color = "darkgrey") +
  geom_hline(yintercept = 35, linetype = 2, color = "darkgrey") +
  ylim(0,70)+  
  labs(x="Conditioning organism", y=expression(Gross~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  scale_x_discrete (labels = c("Spi" = "Stylophora pistillata", "Amu" = "Acropora muricata",
                               "Mdi" = "Montipora digitata","Ssp" = "Sinularia sp.","Xsp"  = "Xenia sp.", 
                               "Csp" = "Caulerpa sp.","Hsp" = "Halimeda sp.", "Hcn" ="Haliclona cnidata")) +
  guides(fill=FALSE) +
  guides(alpha=FALSE) +
  theme_classic() +
  theme(plot.margin = unit(c(0.1,0.1,0.1,1), "cm"), axis.title.y = element_text(size = 12, hjust = 0.5))+
  annotate("text", x = 2.5, y = 0, size = 4, label = c("stony coral"), color = "darkgrey")+
  annotate("text", x = 4.5, y = 0, size = 4, label = c("soft coral"), color = "darkgrey")+
  annotate("text", x = 6.5, y = 0, size = 4, label = c("macroalgae"), color = "darkgrey")+
  annotate("text", x = 8, y = 0, size = 4, label = c("sponge"), color = "darkgrey") +
  annotate("text", x = 4, y = 65, size = 7, label = c("*"), color = "black")+
  annotate("text", x = 5, y = 65, size = 7, label = c("*"), color = "black")+
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 12, face = c("bold.italic", "italic", "italic", "italic","italic",
                                                       "italic", "italic", "italic"),
                                   angle = 45, vjust = 1, hjust=1))

#####merge 3 plots of S. pistillata
arranged_Spi <- ggarrange(net, resp, gross,
                          labels = c("g)", "h)", "i)"),
                          label.x = 0.035,
                          label.y = 0.95,
                          common.legend = TRUE,
                          legend = "none",
                          ncol = 3, nrow = 1,
                          hjust = -1.5,
                          vjust = 0)

rm(gross, net, resp, spi)

####merge all arranged_species to one plot
Figure3 <- ggarrange(arranged_Pru, arranged_Pve, arranged_Spi,
                         # label.x = 0,
                        #  label.y = 0.95,
                          common.legend = TRUE,
                          legend = "none",
                          ncol = 1, nrow = 3,
                          hjust = -0.5,
                          vjust = 0)

Figure3
ggsave("Figures/Figure3.png", width=15, height = 15, limitsize = FALSE, dpi = 700, Figure3)
rm(arranged_Pru, arranged_Pve, arranged_Spi, Figure3)


#___________________________________________________
# Figure4
#___________________________________________________
######create Score to identify the top and worst influencer
########Rank for Net photosynthesis
External <- calc_all %>% filter(incubation=="heterospecific-conditioned")

###separate by incubated species
Pru <- External %>% filter(species == "Pru")
Pve <- External %>% filter(species == "Pve")
Spi <- External %>% filter(species == "Spi")

rm(External)

# Mean per day net photo
Pru_merged <- Pru %>% group_by(cond_organism) %>% summarise(mean_Pru_net=mean(net_photo_ug_h_cm2))
Spi_merged <- Spi %>% group_by(cond_organism) %>% summarise(mean_Spi_net=mean(net_photo_ug_h_cm2))
Pve_merged <- Pve %>% group_by(cond_organism) %>% summarise(mean_Pve_net=mean(net_photo_ug_h_cm2))

# Merge tables net photo
Pru <- left_join(Pru, Pru_merged, by= "cond_organism")
Spi <- left_join(Spi, Spi_merged, by= "cond_organism")
Pve <- left_join(Pve, Pve_merged, by= "cond_organism")

# rank by conditioning organism
Pru_rank_net <- Pru %>% select(c(cond_organism, mean_Pru_net)) %>% distinct() %>% mutate(rank_Pru_net = rank(mean_Pru_net))
Spi_rank_net <- Spi %>% select(c(cond_organism, mean_Spi_net)) %>% distinct() %>% mutate(rank_Spi_net = rank(mean_Spi_net))
Pve_rank_net <- Pve %>% select(c(cond_organism, mean_Pve_net)) %>% distinct() %>% mutate(rank_Pve_net = rank(mean_Pve_net))

#left_join to join all rank tables
Net<- left_join(Pru_rank_net, Spi_rank_net, by= "cond_organism")
Net_rank<- left_join(Net, Pve_rank_net, by= "cond_organism")

#reduce table to necessary information
Net_rank <- Net_rank %>% select(c(cond_organism, rank_Pru_net, rank_Spi_net, rank_Pve_net))

#Name %>% pivot_longer
Net_rank <- Net_rank %>% pivot_longer(rank_Pru_net:rank_Pve_net)

# remove unnecessary tables
rm(Pru_merged, Spi_merged, Pve_merged, Pru, Pve, Spi, Net, Pru_rank_net, Spi_rank_net, Pve_rank_net)

#####################################
########Rank for Gross photosynthesis
External <- calc_all %>% filter(incubation=="heterospecific-conditioned")

###separate by incubated species
Pru <- External %>% filter(species == "Pru")
Pve <- External %>% filter(species == "Pve")
Spi <- External %>% filter(species == "Spi")

rm(External)

# Mean per day gross photo
Pru_merged <- Pru %>% group_by(cond_organism) %>% summarise(mean_Pru_gross=mean(gross_photo_ug_h_cm2))
Spi_merged <- Spi %>% group_by(cond_organism) %>% summarise(mean_Spi_gross=mean(gross_photo_ug_h_cm2))
Pve_merged <- Pve %>% group_by(cond_organism) %>% summarise(mean_Pve_gross=mean(gross_photo_ug_h_cm2))

# Merge tables gross photo
Pru <- left_join(Pru, Pru_merged, by= "cond_organism")
Spi <- left_join(Spi, Spi_merged, by= "cond_organism")
Pve <- left_join(Pve, Pve_merged, by= "cond_organism")

# rank by conditioning organism
Pru_rank_gross <- Pru %>% select(c(cond_organism, mean_Pru_gross)) %>% distinct() %>% mutate(rank_Pru_gross = rank(mean_Pru_gross))
Spi_rank_gross <- Spi %>% select(c(cond_organism, mean_Spi_gross)) %>% distinct() %>% mutate(rank_Spi_gross = rank(mean_Spi_gross))
Pve_rank_gross <- Pve %>% select(c(cond_organism, mean_Pve_gross)) %>% distinct() %>% mutate(rank_Pve_gross = rank(mean_Pve_gross))

#left_join to join all rank tables
Gross<- left_join(Pru_rank_gross, Spi_rank_gross, by= "cond_organism")
Gross_rank<- left_join(Gross, Pve_rank_gross, by= "cond_organism")

#reduce table to necessary information
Gross_rank <- Gross_rank %>% select(c(cond_organism, rank_Pru_gross, rank_Spi_gross, rank_Pve_gross))

#Name %>% pivot_longer
Gross_rank <- Gross_rank %>% pivot_longer(rank_Pru_gross:rank_Pve_gross)

# remove unnecessary tables
rm(Pru_merged, Spi_merged, Pve_merged, Pru, Pve, Spi, Gross, Pru_rank_gross, Spi_rank_gross, Pve_rank_gross)

############################
########Rank for Respiration
External <- calc_all %>% filter(incubation=="heterospecific-conditioned")

###separate by incubated species
Pru <- External %>% filter(species == "Pru")
Pve <- External %>% filter(species == "Pve")
Spi <- External %>% filter(species == "Spi")

rm(External)

# Mean per day respiration
Pru_merged <- Pru %>% group_by(cond_organism) %>% summarise(mean_Pru_resp=mean(respiration_ug_h_cm2))
Spi_merged <- Spi %>% group_by(cond_organism) %>% summarise(mean_Spi_resp=mean(respiration_ug_h_cm2))
Pve_merged <- Pve %>% group_by(cond_organism) %>% summarise(mean_Pve_resp=mean(respiration_ug_h_cm2))

# Merge tables respiration
Pru <- left_join(Pru, Pru_merged, by= "cond_organism")
Spi <- left_join(Spi, Spi_merged, by= "cond_organism")
Pve <- left_join(Pve, Pve_merged, by= "cond_organism")

# rank by conditioning organism
Pru_rank_resp <- Pru %>% select(c(cond_organism, mean_Pru_resp)) %>% distinct() %>% mutate(rank_Pru_resp = rank(mean_Pru_resp))
Spi_rank_resp <- Spi %>% select(c(cond_organism, mean_Spi_resp)) %>% distinct() %>% mutate(rank_Spi_resp = rank(mean_Spi_resp))
Pve_rank_resp <- Pve %>% select(c(cond_organism, mean_Pve_resp)) %>% distinct() %>% mutate(rank_Pve_resp = rank(mean_Pve_resp))

#left_join to join all rank tables
Resp<- left_join(Pru_rank_resp, Spi_rank_resp, by= "cond_organism")
Resp_rank<- left_join(Resp, Pve_rank_resp, by= "cond_organism")

#reduce table to necessary information
Resp_rank <- Resp_rank %>% select(c(cond_organism, rank_Pru_resp, rank_Spi_resp, rank_Pve_resp))

#Name %>% pivot_longer
Resp_rank <- Resp_rank %>% pivot_longer(rank_Pru_resp:rank_Pve_resp)

# remove unnecessary tables
rm(Pru_merged, Spi_merged, Pve_merged, Pru, Pve, Spi, Resp, Pru_rank_resp, Spi_rank_resp, Pve_rank_resp)

#####plot scores
Net_rank$cond_organism<- factor(Net_rank$cond_organism, levels = c("Amu","Mdi", "Ssp","Xsp","Csp","Hsp","Hcn"))

##Plot Net photosynthesis
rnet <- ggplot(Net_rank, aes(x= name, y=value, shape = cond_organism))+
  geom_point(size = 3) +
  geom_line(aes(group = cond_organism, color= cond_organism), lwd = 1)+
  scale_x_discrete (labels = c("rank_Spi_net" = expression(italic("Stylophora pistillata")), 
                               "rank_Pve_net" = expression(italic("Pocillopora verrucosa")),
                               "rank_Pru_net" = expression(italic("Porites rus")))) +
  ylab("\nNet photosynthesis ranks") +
  scale_shape_manual(name ="Conditioning organism", labels = c("Amu" = expression(italic("Acropora muricata")),
                                                               "Mdi" = expression(italic("Montipora digitata")),
                                                               "Ssp" = expression(paste(italic("Sinularia"), " sp.")),
                                                               "Xsp" = expression(paste(italic("Xenia"), " sp.")), 
                                                               "Csp" = expression(paste(italic("Caulerpa")," sp.")), 
                                                               "Hsp" = expression(paste(italic("Halimeda"), " sp.")),
                                                               "Hcn" = expression(italic("Haliclona cnidata"))),
                     values=c(0, 1, 2, 3, 4, 5, 6, 8))+
  scale_color_manual(name ="Conditioning organism", labels = c("Amu" = expression(italic("Acropora muricata")),
                                                               "Mdi" = expression(italic("Montipora digitata")),
                                                               "Ssp" = expression(paste(italic("Sinularia"), " sp.")),
                                                               "Xsp" = expression(paste(italic("Xenia"), " sp.")), 
                                                               "Csp" = expression(paste(italic("Caulerpa")," sp.")), 
                                                               "Hsp" = expression(paste(italic("Halimeda"), " sp.")),
                                                               "Hcn" = expression(italic("Haliclona cnidata"))),
                     values = c("Amu" = "#000066", "Mdi" = "#0000CC",
                                "Ssp" = "#FF33FF", "Xsp" = "#990099", 
                                "Csp" = "#66FF33", "Hsp" = "#33CC33",
                                "Hcn" = "#000000")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, face = "italic",angle = 45, vjust = 1, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12, vjust = 0.5, hjust = 0),
        legend.title = element_text(size = 12))

rnet

##Plot Gross photosynthesis
Gross_rank$cond_organism<- factor(Gross_rank$cond_organism, levels = c("Amu","Mdi", "Ssp","Xsp","Csp","Hsp","Hcn"))

rgross <- ggplot(Gross_rank, aes(x= name, y=value, shape = cond_organism))+
  geom_point(size = 3) +
  geom_line(aes(group = cond_organism, color= cond_organism), lwd = 1)+
  scale_x_discrete (labels = c("rank_Spi_gross" = expression(italic("Stylophora pistillata")),
                               "rank_Pve_gross" = expression(italic( "Pocillopora verrucosa")),
                               "rank_Pru_gross" = expression(italic("Porites rus")))) +
  ylab("\nGross photosynthesis ranks") +
  scale_shape_manual(name ="Conditioning organism", labels = c("Amu" = expression(italic("Acropora muricata")),
                                                               "Mdi" = expression(italic("Montipora digitata")),
                                                               "Ssp" = expression(paste(italic("Sinularia"), " sp.")),
                                                               "Xsp" = expression(paste(italic("Xenia"), " sp.")), 
                                                               "Csp" = expression(paste(italic("Caulerpa")," sp.")), 
                                                               "Hsp" = expression(paste(italic("Halimeda"), " sp.")),
                                                               "Hcn" = expression(italic("Haliclona cnidata"))),
                     values=c(0, 1, 2, 3, 4, 5, 6, 8))+
  scale_color_manual(name ="Conditioning organism", labels = c("Amu" = expression(italic("Acropora muricata")),
                                                               "Mdi" = expression(italic("Montipora digitata")),
                                                               "Ssp" = expression(paste(italic("Sinularia"), " sp.")),
                                                               "Xsp" = expression(paste(italic("Xenia"), " sp.")), 
                                                               "Csp" = expression(paste(italic("Caulerpa")," sp.")), 
                                                               "Hsp" = expression(paste(italic("Halimeda"), " sp.")),
                                                               "Hcn" = expression(italic("Haliclona cnidata"))),
                     values = c("Amu" = "#000066", "Mdi" = "#0000CC",
                                "Ssp" = "#FF33FF", "Xsp" = "#990099", 
                                "Csp" = "#66FF33", "Hsp" = "#33CC33",
                                "Hcn" = "#000000")) +
  theme_bw() +
  theme(axis.text.x = element_text(size =12, face = "italic",angle = 45, vjust = 1, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12, face = "italic", vjust = 0.5, hjust = 0),
        legend.title = element_text(size = 12))

##Plot Respiration
Resp_rank$cond_organism<- factor(Resp_rank$cond_organism, levels = c("Amu","Mdi", "Ssp","Xsp","Csp","Hsp","Hcn"))

rresp <- ggplot(Resp_rank, aes(x= name, y=value, shape = cond_organism))+
  geom_point(size = 3) +
  geom_line(aes(group = cond_organism, color= cond_organism), lwd = 1)+
  scale_x_discrete (labels = c("rank_Spi_resp" = expression(italic("Stylophora pistillata")),
                               "rank_Pve_resp" = expression(italic( "Pocillopora verrucosa")),
                               "rank_Pru_resp" = expression(italic("Porites rus")))) +
  ylab("\nRespiration ranks") +
  scale_shape_manual(name ="Conditioning organism", labels = c("Amu" = expression(italic("Acropora muricata")),
                                                               "Mdi" = expression(italic("Montipora digitata")),
                                                               "Ssp" = expression(paste(italic("Sinularia"), " sp.")),
                                                               "Xsp" = expression(paste(italic("Xenia"), " sp.")), 
                                                               "Csp" = expression(paste(italic("Caulerpa")," sp.")), 
                                                               "Hsp" = expression(paste(italic("Halimeda"), " sp.")),
                                                               "Hcn" = expression(italic("Haliclona cnidata"))),
                     values=c(0, 1, 2, 3, 4, 5, 6, 8))+
  scale_color_manual(name ="Conditioning organism", labels = c("Amu" = expression(italic("Acropora muricata")),
                                                               "Mdi" = expression(italic("Montipora digitata")),
                                                               "Ssp" = expression(paste(italic("Sinularia"), " sp.")),
                                                               "Xsp" = expression(paste(italic("Xenia"), " sp.")), 
                                                               "Csp" = expression(paste(italic("Caulerpa")," sp.")), 
                                                               "Hsp" = expression(paste(italic("Halimeda"), " sp.")),
                                                               "Hcn" = expression(italic("Haliclona cnidata"))),
                     values = c("Amu" = "#000066", "Mdi" = "#0000CC",
                                "Ssp" = "#FF33FF", "Xsp" = "#990099", 
                                "Csp" = "#66FF33", "Hsp" = "#33CC33",
                                "Hcn" = "#000000")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, face = "italic",angle = 45, vjust = 1, hjust=1)) +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(size = 12), 
        legend.position = "bottom",
        legend.text = element_text(size = 12, face = "italic", vjust = 0.5, hjust = 0),
        legend.title = element_text(size = 12))


#####merge 3 rank plots
Figure4 <- ggarrange(rnet, rresp, rgross,
                            labels = c("a)", "b)", "c)"),
                            label.x = 0,
                            label.y = 0.95,
                            common.legend = TRUE,
                            legend = "bottom",
                            ncol = 3, nrow = 1,
                            hjust = -1,
                            vjust = 0) 

ggsave("Figures/Figure4.png", width=11, height = 5, 
       limitsize = FALSE, dpi = 700, Figure4)

rm(calc_all, Figure4, Gross_rank, Net_rank, Resp_rank, rgross, rnet, rresp)
