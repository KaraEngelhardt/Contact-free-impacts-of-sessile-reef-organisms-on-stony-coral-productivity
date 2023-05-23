# Plots Exp2
library(tidyverse)
library(rstatix)
library(patchwork)
library(lubridate)
library(ggpubr)
library(scales)

oxy_calc <- read_csv("data_prep_exp2.csv")

oxy_calc <- oxy_calc %>% 
  filter(fragment_ID != "Spi_A2_P") %>%
  filter(fragment_ID != "Spi_A2_M")

oxy_calc <- oxy_calc %>% select(-c(X1))


Mono <- oxy_calc %>% filter(Category=="M")
Poly <- oxy_calc %>% filter(Category=="P")

oxy_calc <- oxy_calc %>% unite('Merged', Species:Category, remove = FALSE)


d <- ggplot(oxy_calc, aes(x = Species, y = net_photo_ug_h_cm2, fill = Category)) +
  scale_fill_manual(name = NULL, labels = c ("M" = "Monoculture", "P" = "Polyculture"), values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  scale_colour_manual(name = NULL, values = c("#000066", "#000066", "#0000CC", "#0000CC", "#3366FF", "#3366FF",
                                              "#00CCFF", "#00CCFF", "#66FFFF", "#66FFFF")) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = expression(Net~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,55)+
  theme_classic() +
  geom_point(aes(colour=Merged), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.02)) +
  scale_x_discrete (labels = c("Amu" = "Acropora muricata", "Mdi" = "Montipora digitata", "Pru" = "Porites rus", 
                               "Pve" = "Pocillopora verrucosa", "Spi" = "Stylophora pistillata")) +
  theme(legend.position="top")+
  annotate(geom = "text", x = 3.9, y = 53,
           label = "**", hjust = 0, size = 7) +
  annotate(geom = "text", x = 2.9, y = 53,
           label = "**", hjust = 0, size = 7) +
  theme(
    plot.margin = unit(c(1,0.2,0.2,1), "cm"),
    axis.text.x = element_text(size = 12, color = "black", face= "italic", angle = 45, hjust= 1),
    legend.text = element_text(size = 12), 
    axis.title.y =element_text(hjust = 0.5, size=12,face="bold",margin=margin(0,12,0,0))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title=element_blank()) +
  guides (color = "none") +
  theme(legend.position="bottom") 

# respiration
e <- ggplot(oxy_calc, aes(x = Species, y = respiration_ug_h_cm2, fill = Category)) +
  scale_fill_manual(name = NULL, labels = c ("M" = "Monoculture", "P" = "Polyculture"), values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  scale_colour_manual(name = NULL, values = c("#000066", "#000066", "#0000CC", "#0000CC", "#3366FF", "#3366FF",
                                              "#00CCFF", "#00CCFF", "#66FFFF", "#66FFFF")) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = expression(Respiration~(µg~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,20)+
  theme_classic() +
  geom_point(aes(colour=Merged), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.02)) +
  scale_x_discrete (labels = c("Amu" = "Acropora muricata", "Mdi" = "Montipora digitata", "Pru" = "Porites rus", 
                               "Pve" = "Pocillopora verrucosa", "Spi" = "Stylophora pistillata")) +
  theme(axis.text.x = element_text(face = "italic",angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position="top") +
  theme(
    plot.margin = unit(c(1,0.2,0.2,1), "cm"),
    axis.text.x = element_text(size = 12, color = "black", face= "italic", angle = 45, hjust= 1),
    #axis.text.y = element_text(size = 10, vjust = 0.5, hjust = 0, color = "black"),
    axis.title.y =element_text(hjust = 0.5, size=12,face="bold",margin=margin(0,12,0,0))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title=element_blank()) +
  guides (color = "none") +
  theme(legend.position="bottom",
        legend.text = element_text(size = 12))

# gross
f <- ggplot(oxy_calc, aes(x = Species, y = gross_photo_ug_h_cm2, fill = Category)) +
  scale_fill_manual(name = NULL, labels = c ("M" = "Monoculture", "P" = "Polyculture"), values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  scale_colour_manual(name = NULL, values = c("#000066", "#000066", "#0000CC", "#0000CC", "#3366FF", "#3366FF",
                                              "#00CCFF", "#00CCFF", "#66FFFF", "#66FFFF")) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = expression(Gross~photosynthesis~(µg~O[2]~cm^{-2}~h^{-1}))) +
  ylim(0,75)+
  theme_classic() +
  geom_point(aes(colour=Merged), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.02)) +
  scale_x_discrete (labels = c("Amu" = "Acropora muricata", "Mdi" = "Montipora digitata", "Pru" = "Porites rus", 
                               "Pve" = "Pocillopora verrucosa", "Spi" = "Stylophora pistillata")) +
  theme(axis.text.x = element_text(face = "italic",angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position="top") +
  annotate(geom = "text", x = 4, y = 73,
           label = "*",
           hjust = 0, size = 7) +
  theme(
    plot.margin = unit(c(1,0.2,0.2,1), "cm"),
    axis.text.x = element_text(size = 12, color = "black", face= "italic", angle = 45, hjust= 1),
    # axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0, color = "black"),
    axis.title.y =element_text(hjust = 0.5, size=12,face="bold",margin=margin(0,12,0,0))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title=element_blank()) +
  guides (color = "none") +
  theme(legend.position="bottom",
        legend.text = element_text(size = 12))

#calcification
Data <- read_csv(file = "Calcification.csv") %>% 
  mutate(Category = as.factor(Category)) %>% 
  mutate(Species = as.factor(Species))

Data <- Data %>% mutate(Colony = str_sub(fragment_ID, start = -4, end=-4))

Data <- Data %>% unite('Merged', Species:Category, remove = FALSE)

g <- ggplot(Data, aes(x = Species, y = Calc_umol_cm2_h, fill = Category)) +
  scale_fill_manual(name = NULL, labels = c ("M" = "Monoculture", "P" = "Polyculture"), values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  scale_colour_manual(name = NULL, values = c("#000066", "#000066", "#0000CC", "#0000CC", "#3366FF", "#3366FF",
                                              "#00CCFF", "#00CCFF", "#66FFFF", "#66FFFF")) +
  geom_boxplot(outlier.shape = NA) +
  labs(y = expression(Calcification~(µmol~CaCO[3]~cm^{-2}~h^{-1}))) +
  ylim(0,0.7)+
  theme_classic() +
  geom_point(aes(colour=Merged), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.02)) +
  scale_x_discrete (labels = c("Amu" = "Acropora muricata", "Mdi" = "Montipora digitata", "Pru" = "Porites rus", 
                               "Pve" = "Pocillopora verrucosa", "Spi" = "Stylophora pistillata")) +
  annotate(geom = "text", x = 1, y = 0.69,
           label = "*",
           hjust = 0, size = 7) +
  annotate(geom = "text", x = 3, y = 0.69,
           label = "*",
           hjust = 0, size = 7) +
  theme(
    plot.margin = unit(c(1,0.2,0.2,1), "cm"),
    axis.text.x = element_text(size = 12, color = "black", face= "italic", angle = 45, hjust= 1),
    # axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0, color = "black"),
    axis.title.y =element_text(hjust = 0.5, size=12,face="bold",margin=margin(0,12,0,0))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title=element_blank()) +
  guides (color = "none") +
  theme(legend.position="bottom",
        legend.text = element_text(size = 12))

# Symbionts
Dinos <- read_csv("Symbionts_prep.csv")

Dinos <- Dinos %>% unite('Merged', Species:Category, remove = FALSE)

h <- ggplot(Dinos, aes(x = Species, y = cells_cm2, fill = Category)) +
  scale_fill_manual(name = NULL, labels = c ("M" = "Monoculture", "P" = "Polyculture"), values=c("#FFFFFF", "#999999")) +
  geom_boxplot(outlier.shape = NA) +
  theme_classic() +
  scale_colour_manual(name = NULL, values = c("#000066", "#000066", "#0000CC", "#0000CC", "#3366FF", "#3366FF",
                                              "#00CCFF", "#00CCFF", "#66FFFF", "#66FFFF")) +
  labs(y = expression(symbiont~cells~cm^{2})) +
  scale_y_continuous(labels = c(expression("0"),expression("1\u00D710"^"6"), expression("2\u00D710"^"6"), 
                             expression("3\u00D710"^"6"), expression("4\u00D710"^"6"))) +
  geom_point(aes(colour=Merged), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.02)) +
  scale_x_discrete (labels = c("Amu" = "Acropora muricata", "Mdi" = "Montipora digitata", "Pru" = "Porites rus", 
                               "Pve" = "Pocillopora verrucosa", "Spi" = "Stylophora pistillata")) +
  annotate(geom = "text", x = 1, y = 4e+06,
           label = "*", hjust = 0, size = 7) +
  theme(
    plot.margin = unit(c(1,0.2,0.2,1), "cm"),
    axis.text.x = element_text(size = 12, color = "black", face= "italic", angle = 45, hjust= 1),
    # axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"),
    axis.title.y =element_text(hjust = 0.5, size=12,face="bold",margin=margin(0,0,0,0))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title=element_blank()) +
  guides (color = "none") +
  theme(legend.position="bottom",
        legend.title = element_text(size = 12))

# merge all plots
Figure5 <- ggarrange(d, e, f, g, h,
                                 labels = c("a)", "b)", "c)", "d)", "e)"),
                                 label.x = 0,
                                 label.y = 0.95,
                                 common.legend = TRUE,
                                 legend = "bottom",
                                 ncol = 3, nrow = 2,
                                 hjust = -2,
                                 vjust = 0.5)



ggsave("Figures/Figure5.png", width=11, height = 11, 
       limitsize = FALSE, dpi = 700, Figure5)

rm(list = ls())
