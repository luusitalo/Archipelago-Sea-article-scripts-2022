######################################
# Visualization of expected benefits from the Hugin model
# LU 2022
######################################

library(pheatmap)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)


setwd("D:/Users/uusitalol/RStudio/2021 AS EwE to Hugin/")

values <- read.csv("parsed_utilities.csv")

#create matrix for heatmap
heatmat <- acast(values,Climate.scenario + Nutrient.scenario +   Fishing.scenario ~ Value.type + Stakeholder.group, value.var="Overall.expected.utility")

#drawing heatmaps

colors <- brewer.pal(7, "RdYlBu") #color blind friendly palette
#breaksList = seq(-3, 3, by = 0.5)

#Overall expected utility, tiff file
pheatmap(heatmat, cluster_rows=FALSE, cluster_cols=FALSE, display_numbers=TRUE, 
         
         labels_col=c("Livelihood value, \ncabin and sailing", "Livelihood value, \nlocals", "Livelihood value, \nrecreation",
                      "Recretional value, \ncabin and sailing", "Recreational value, \nlocals", "Recreational value, \nrecreation"), 
         
   
          labels_row=c("RCP4.5, BSAP, Dec","RCP4.5, BSAP, Gillnet", "RCP4.5, BSAP, Inc","RCP4.5, BSAP, SQ",
                       "RCP4.5, PLC55, Dec","RCP4.5, PLC55, Gillnet","RCP4.5, PLC55, Inc","RCP4.5, PLC55, SQ",
                       "RCP8.5, BSAP, Dec","RCP8.5, BSAP, Gillnet","RCP8.5, BSAP, Inc","RCP8.5, BSAP, SQ",
                       "RCP8.5, PLC55, Dec","RCP8.5, PLC55, Gillnet","RCP8.5, PLC55, Inc","RCP8.5, PLC55, SQ"),
        
        number_color="black", fontsize=8, col=colors, gaps_col=3, gaps_row=c(4,8,12), angle_col=0, filename="values_heatmap.tiff",
        legend = FALSE, #breaks=breaksList
)

#Overall expected utility, png
pheatmap(heatmat, cluster_rows=FALSE, cluster_cols=FALSE, display_numbers=TRUE, 
         
         labels_col=c("Livelihood value, \ncabin and sailing", "Livelihood value, \nlocals", "Livelihood value, \nrecreation",
                      "Recretional value, \ncabin and sailing", "Recreational value, \nlocals", "Recreational value, \nrecreation"), 
         
         
         labels_row=c("RCP4.5, BSAP, Dec","RCP4.5, BSAP, Gillnet", "RCP4.5, BSAP, Inc","RCP4.5, BSAP, SQ",
                      "RCP4.5, PLC55, Dec","RCP4.5, PLC55, Gillnet","RCP4.5, PLC55, Inc","RCP4.5, PLC55, SQ",
                      "RCP8.5, BSAP, Dec","RCP8.5, BSAP, Gillnet","RCP8.5, BSAP, Inc","RCP8.5, BSAP, SQ",
                      "RCP8.5, PLC55, Dec","RCP8.5, PLC55, Gillnet","RCP8.5, PLC55, Inc","RCP8.5, PLC55, SQ"),
         
         number_color="black", fontsize=8, col=colors, gaps_col=3, gaps_row=c(4,8,12), angle_col=0, filename="values_heatmap.png",
         legend = FALSE, #breaks=breaksList
)

#Figure 5
Fig51 <- read.csv("Fig5_climates.csv")
Fig52 <- read.csv("Fig5_nutrients.csv")
Fig53 <- read.csv("Fig5_fishing.csv")
Fig53$Fishing.scen <- factor(Fig53$Fishing.scen,levels = c("Dec", "Gil", "SQ", "Inc"))

p1 <- ggplot(data=Fig51, aes(x=Climate, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Climate scenario", y = "Utility") +
  ylim(-0.4, 1.6) +
  scale_fill_grey() +
  theme_minimal() +
  theme(legend.position="none",
        legend.title = element_blank(),
        strip.text.x=element_text(margin=margin(t=20))) + 
  xlab(element_blank()) 

p2 <- ggplot(data=Fig52, aes(x=Nutrients, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Nutrient scenario", y = "Utility")+
  ylim(-0.4, 1.6) +
  scale_fill_grey() +
  theme_minimal() + 
  theme(legend.position="none",
        legend.title = element_blank(),
        strip.text.x = element_blank()) + 
  xlab(element_blank()) 

p3 <- ggplot(data=Fig53, aes(x=Fishing.scen, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Fishing scenario", y = "Utility") +
  ylim(-0.4, 1.6) +
  scale_fill_grey() +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        strip.text.x = element_blank()) + 
  xlab(element_blank()) 


fig5 <- ggarrange(p1, p2, p3,
                  labels = c("a) Climate scenarios", "b) Nutrient scenarios", "c) Fishing scenarios"), 
                  font.label = list(size = 11),
                   ncol = 1, nrow = 3,
                  common.legend = TRUE, legend = "bottom")

                 
fig5
ggsave("Figure5.png")
ggsave("Figure5.tiff")

#Figure 6

Fig6 <- read.csv("Fig6_fishing.csv")


p4 <- ggplot(data=Fig6[Fig6$Climate == "RCP4.5",], aes(x=Fishing.scen, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Fishing scenario", y = "Utility")+
  ylim(-0.8, 1.7) +
  scale_fill_grey() +
  theme_minimal() + 
  theme(legend.position="none",
        legend.title = element_blank(),
        strip.text.x=element_text(margin=margin(t=20))) + 
  xlab(element_blank()) 

p5 <- ggplot(data=Fig6[Fig6$Climate == "RCP8.5",], aes(x=Fishing.scen, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Fishing scenario", y = "Utility") +
  ylim(-0.8, 1.7) +
  scale_fill_grey() +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        strip.text.x = element_blank()) + 
  xlab(element_blank()) 


fig6 <- ggarrange(p4, p5,
                  ncol = 1, nrow = 2,
                  labels = c("a) Fishing scenarios under RCP4.5", "b) Fishing scenarios under RCP8.5"),
                  font.label = list(size = 11),
                  #vjust = -1.5,
                  common.legend = TRUE, legend = "bottom")

fig6
ggsave("Figure6.png")
ggsave("Figure6.tiff")




Fig7 <- read.csv("Fig7_nutrients.csv")

p6 <- ggplot(data=Fig7[Fig7$Climate == "RCP4.5",], aes(x=Nutrient.scen, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Fishing scenario", y = "Utility")+
  ylim(-0.8, 1.7) +
  scale_fill_grey() +
  theme_minimal() + 
  theme(legend.position="none",
        legend.title = element_blank(),
        strip.text.x=element_text(margin=margin(t=20))) + 
  xlab(element_blank()) 

p7 <- ggplot(data=Fig7[Fig7$Climate == "RCP8.5",], aes(x=Nutrient.scen, y=Value, fill=Sh.group)) +
  facet_wrap(~ Value.type) + 
  geom_col(position=position_dodge2()) + 
  labs(x="Fishing scenario", y = "Utility") +
  ylim(-0.8, 1.7) +
  scale_fill_grey() +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        strip.text.x = element_blank()) + 
  xlab(element_blank()) 


fig7 <- ggarrange(p6, p7,
                  ncol = 1, nrow = 2,
                  labels = c("a) Nutrient scenarios under RCP4.5", "b) Nutrient scenarios under RCP8.5"),
                  font.label = list(size = 11),
                  common.legend = TRUE, legend = "bottom")

fig7
ggsave("Figure7.png")
ggsave("Figure7.tiff")

