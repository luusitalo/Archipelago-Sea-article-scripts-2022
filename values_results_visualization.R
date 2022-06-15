######################################
# Visualization of expected benefits exported from the Hugin model
# Laura Uusitalo 24.3.2022
# laura.uusitalo@syke.fi
######################################

library(pheatmap)
library(reshape2)
library(RColorBrewer)
library(ggplot2)

setwd("D:/Users/uusitalol/RStudio/2021 AS EwE to Hugin/")

# This data file is available in this github folder. The data has been
# exported from Hugin software.
values <- read.csv("parsed_utilities.csv")

#create matrix for heatmap
heatmat <- acast(values,Climate.scenario + Nutrient.scenario + Fishing.scenario 
                 ~ Value.type + Stakeholder.group, 
                 value.var="Overall.expected.utility")


#Drawing heatmaps

colors <- brewer.pal(7, "RdYlBu") #color blind friendly palette
#breaksList = seq(-3, 3, by = 0.5)

#Overall expected utility, tiff file
pheatmap(heatmat, cluster_rows=FALSE, cluster_cols=FALSE, display_numbers=TRUE, 
         
         labels_col=c("Professional value, \ncabin and sailing", "Professional value, \nlocals", "Professional value, \nrecreation",
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
         
         labels_col=c("Professional value, \ncabin and sailing", "Professional value, \nlocals", "Professional value, \nrecreation",
                      "Recretional value, \ncabin and sailing", "Recreational value, \nlocals", "Recreational value, \nrecreation"), 
         
         
         labels_row=c("RCP4.5, BSAP, Dec","RCP4.5, BSAP, Gillnet", "RCP4.5, BSAP, Inc","RCP4.5, BSAP, SQ",
                      "RCP4.5, PLC55, Dec","RCP4.5, PLC55, Gillnet","RCP4.5, PLC55, Inc","RCP4.5, PLC55, SQ",
                      "RCP8.5, BSAP, Dec","RCP8.5, BSAP, Gillnet","RCP8.5, BSAP, Inc","RCP8.5, BSAP, SQ",
                      "RCP8.5, PLC55, Dec","RCP8.5, PLC55, Gillnet","RCP8.5, PLC55, Inc","RCP8.5, PLC55, SQ"),
         
         number_color="black", fontsize=8, col=colors, gaps_col=3, gaps_row=c(4,8,12), angle_col=0, filename="values_heatmap.png",
         legend = FALSE, #breaks=breaksList
)

