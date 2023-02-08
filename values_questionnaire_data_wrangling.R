##############################
## Susanna Jernberg's value questionnaire data wrangling
## Laura Uusitalo, June 2021
##############################

library(ggplot2)
library(lattice)
library(pheatmap)
library(RColorBrewer)
#library(gridExtra)


setwd("D:/Users/uusitalol/RStudio/2021 AS EwE to Hugin/")

values <- read.csv("data/Kysely Saaristomeren luontoarvoista_Perusraportti_raaka.csv", encoding="UTF-8")

#########################
# Data wrangling

#remove row 1
values <- values[-1,]

#tweak colnames
col <-c(
  # Do these species help or hinder your recreation, profession, should there be more of less/fewer of them
  #columns 1-31 
  "ClarityRec", "ClarityProf","ClarityPref","SealRec", "SealProf", "SealPref","FucusRec", "FucusProf", "FucusPref", "CormorantRec",  #col 1-10
  "CormorantProf", "cormorantPref", "SanderRec", "SanderProf", "SanderPref", "HerringRec", "HerringProf", "HerringPref", "PerchRec", "PerchProf", #col 11-20 
  "PerchPref", "NISRec", "NISProf", "NISPref", "FilAlgRec", "FilAlgProf", "FilAlgPref", "CyanoRec", "CyanoProf", "CyanoPref", #col 21-30
  #open comment field
  "Comment1", #col 31
  
  # Divide 10 coins --what is the importance of different elements for your recreation
  #columns 32-48
  "CoinFucus", "CoinClarity", "CoinSeals",	"CoinCormorants", "CoinSander", "CoinHerring",	"CoinPerch", "CoinNIS", "CoinFilAlg", #col 32-40
  "CoinCyano", "CoinViews", "CoinServices", "CoinOtherSpp", "CoinOther", #col 41-45
  "Comment2", "Comment3",	"Comment4", #col 46-48
  
  # Which activities have you engaged to in the past 12 months
  #cols 49-61
  "EngHiking", "EngJogBike", #col 49-50
  "EngObservingBirding", "EngFishing", "EngBerryMushroom", "EndPhotography", "EngSwimming", "EngDivingSnorkeling", "EngCanooing", "EngSailingBoating",  "EngOther", "EngNothing", # col 51-60
  "Comment5", # col 61
  
  #Which of these activities are most important to you? Pick 3
  #columns 62-74
  "ImpHiking", "ImpJogBike", "ImpObservingBirding", "ImpFishing", "ImpBerryMushroom", "EndPhotography", "ImpSwimming", "ImpDivingSnorkeling", "ImpCanooing", #col 62-70
  "ImpSailingBoating",  "ImpOther", #col 71-72
  "RecrDays", #col 73
  "Comment6", #col 74
  
  #Which groups do you belong to
  #cols 75-87
  "GrLocal", "GrProfFisher", "GrProfTourism", "GrGovPolitician", "GrResearcher", "GrEducator", #col 75-80
  "GrRecreation", "GrEnvNGO", "GrNGOProf", "GrCabin", "GrOther", # col 81-85
  "Comment7", #col 86
  
  #Which groups do you primarily belong to
  #cols 87-97
  "PrimGrLocal", "PrimGrProfFisher", "PrimGrProfTourism", "PrimGrGovPolitician", #col 87-90
  "PrimGrResearcher", "PrimGrEducator", "PrimGrRecreation", "PrimGrEnvNGO", "PrimGrNGOProf", "PrimGrCabin", "PrimGrOther", # col 91-97
  #cols 98-102
  "Gender", "Age", "Education",  # col 98-100
  "Postcode", "Municipality" #col 101-102
        ) 

colnames(values) <- col


# Replace text values with corresponding numbers

values[values == "En osaa sanoa"] <- ""

values[values == "Edistää paljon"] <- 2
values[values == "Edistää jonkin verran"] <- 1
values[values == "Ei edistä eikä haittaa"] <- 0
values[values == "Haittaa jonkin verran"] <- -1
values[values == "Haittaa paljon"] <- -2

values[values == "Paljon runsaampaa"] <- 2
values[values == "Jonkin verran runsaampaa"] <- 1
values[values == "Ei runsaampaa eikä vähäisempää"] <- 0
values[values == "Jonkin verran vähäisempää"] <- -1
values[values == "Paljon vähäisempää"] <- -2


# Replace text in Eng* and Gr* columns with value 1 -- if 1, 
# the person engages in this activity

values[, c(49:60, 75:85)][(values[, c(49:60, 75:85)] == "")] <- 0
values[, c(49:60, 75:85)][(values[, c(49:60, 75:85)] != 0)] <- 1

values[, c(1:30, 32:45, 49:60, 62:73, 75:85, 87:97, 99)] = as.numeric(unlist(values[, c(1:30, 32:45, 49:60, 62:73, 75:85, 87:97, 99)]))

#rename genders in English

values[,98][(values[,98] == "Nainen")] <- "F"
values[,98][(values[,98] == "Mies")] <- "M"

##################
#Remove all rows that have != 10 coins summed up
#
#vector for sum of coins
# coinsum = rowSums(values[,32:45], na.rm=TRUE)
#filter our rows with coinsum != 10
# values <- values[coinsum==10,]


# Make a Primary Group variable

values[ ,87:97][is.na(values[ ,87:97])] <- 0

values$PrimGroup <- NA

for (i in 1:nrow(values)) {
  if (values$PrimGrCabin[i] == 1) {
    values$PrimGroup[i] <- "CabinAndSailing"
  }
  if (values$PrimGrEducator[i] == 1){
    values$PrimGroup[i] <- "Educator"
  }
  if (values$PrimGrEnvNGO[i] == 1) {
    values$PrimGroup[i] <- "envNGO"
  }
  if (values$PrimGrGovPolitician[i] == 1) {
    values$PrimGroup[i] <- "GovPolitician"
  }
  if (values$PrimGrLocal[i] == 1) {
    values$PrimGroup[i] <- "Local"
  }
  if (values$PrimGrNGOProf[i] == 1) {
    values$PrimGroup[i] <- "NGOProf"
  }
  if (values$PrimGrOther[i] == 1) {
    values$PrimGroup[i] <- "Other"
  }
  if (values$PrimGrProfFisher[i] == 1) {
    values$PrimGroup[i] <- "ProFisher"
  }
  if (values$PrimGrProfTourism[i] == 1) {
    values$PrimGroup[i] <- "ProTourism"
  }
  if (values$PrimGrRecreation[i] == 1) {
    values$PrimGroup[i] <- "Recreation"
  }
  if (values$PrimGrResearcher[i] == 1) {
    values$PrimGroup[i] <- "Researcher"
  }
} # end for loop

table(values$PrimGroup)

write.csv(values, "values_v1.csv")

#Combine the classes where that seems appropriate
for (i in 1:nrow(values)) {
  #1: ProFishers combined to Local
    if (values$PrimGroup[i] == "ProFisher") {
    values$PrimGroup[i] <- "Local"
  }
  #2: ProTourism to Recreation
  if (values$PrimGroup[i] == "ProTourism") {
    values$PrimGroup[i] <- "Recreation"
  }
  
  #3: For the group "Other", look at their descriptions of activity and
  #   assign to groups in the following manner:
  #   - sailors with cabin owners
  #   - farmer to local
  #    - hotel owner to Recreation
  if (values$PrimGroup[i] == "Other" & values$Comment7[i] %in% c("Veneily", "Veneilijä", "Matkapurjehtija", "Purjehtija", "purjehtija", "med båt i gästhamnar")) {
    values$PrimGroup[i] <- "CabinAndSailing"
  }
  if (values$PrimGroup[i] == "Other" & values$Comment7[i] %in% c("Kalankasvattaja", "Jordbrukare")) {
    values$PrimGroup[i] <- "Local"
  }
  if (values$PrimGroup[i] == "Other" & values$Comment7[i] == "Hotellägare") {
    values$PrimGroup[i] <- "Recreation"
  }
  
}

table(values$PrimGroup)

#Group as a factor, organize according to frequency
values$PrimGroup <- factor(values$PrimGroup, 
                           levels = c("Local", "Recreation", "CabinAndSailing","GovPolitician", "envNGO",
                                                        "NGOProf", "Researcher", "Other")
                           )


#remove PrimGr... columns
#values <- values[ , -c(87:97)]

##################
##Data exploration

#Explore data: Draw histograms of values per primary group 
histogram(~values$PrimGroup, type="count", main="primary group") 



# Make a heatmap of mean values per the three main groups
#  for each ecosystem component and for recreation and profession

# "ClarityRec", "ClarityProf","ClarityPref","SealRec", "SealProf", "SealPref","FucusRec", "FucusProf", "FucusPref", "CormorantRec",  #col 1-10
# "CormorantProf", "cormorantPref", "SanderRec", "SanderProf", "SanderPref", "HerringRec", "HerringProf", "HerringPref", "PerchRec", "PerchProf", #col 11-20 
# "PerchPref", "NISRec", "NISProf", "NISPref", "FilAlgRec", "FilAlgProf", "FilAlgPref", "CyanoRec", "CyanoProf", "CyanoPref", #col 21-30

EnvVars <- c("Water clarity", "Seals", "Fucus", "Cormorants", "Sander", 
             "Herring", "Perch", "NIS", "Filamentous algae", "Cyanobacteria")
meanRecrValues <- data.frame(matrix(ncol = 3, nrow = 10))
colnames(meanRecrValues) <- c("Local", "Cabin and sailing", "Recreational user or professional")
rownames(meanRecrValues) <- EnvVars

meanProfValues <- meanRecrValues

#Effect on recreation 

#Water clarity
meanRecrValues[1,1] <- mean(values[,1][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[1,2] <- mean(values[,1][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[1,3] <- mean(values[,1][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# seals
meanRecrValues[2,1] <- mean(values[,4][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[2,2] <- mean(values[,4][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[2,3] <- mean(values[,4][(values$PrimGroup == "Recreation")], na.rm=TRUE)
#Fucus
meanRecrValues[3,1] <- mean(values[,7][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[3,2] <- mean(values[,7][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[3,3] <- mean(values[,7][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Cormorants
meanRecrValues[4,1] <- mean(values[,10][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[4,2] <- mean(values[,10][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[4,3] <- mean(values[,10][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Sander
meanRecrValues[5,1] <- mean(values[,13][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[5,2] <- mean(values[,13][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[5,3] <- mean(values[,13][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Herring
meanRecrValues[6,1] <- mean(values[,16][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[6,2] <- mean(values[,16][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[6,3] <- mean(values[,16][(values$PrimGroup == "Recreation")], na.rm=TRUE)
#Perch
meanRecrValues[7,1] <- mean(values[,19][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[7,2] <- mean(values[,19][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[7,3] <- mean(values[,19][(values$PrimGroup == "Recreation")], na.rm=TRUE)
#NIS
meanRecrValues[8,1] <- mean(values[,22][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[8,2] <- mean(values[,22][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[8,3] <- mean(values[,22][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Fil algae
meanRecrValues[9,1] <- mean(values[,25][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[9,2] <- mean(values[,25][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[9,3] <- mean(values[,25][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# cyano
meanRecrValues[10,1] <- mean(values[,28][(values$PrimGroup == "Local")], na.rm=TRUE)
meanRecrValues[10,2] <- mean(values[,28][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanRecrValues[10,3] <- mean(values[,28][(values$PrimGroup == "Recreation")], na.rm=TRUE)

#order rows according to row mean

for (i in 1:nrow(meanRecrValues)) {
  meanRecrValues$mean[i] <- mean(as.numeric(meanRecrValues[i,1:3]))
}

# make a heatmap in decreasing order

colors <- brewer.pal(7, "RdYlBu") #color blind friendly palette
breaksList = seq(-2, 2, by = 0.5)

pheatmap(meanRecrValues[order(meanRecrValues$mean, decreasing=TRUE), 1:3], cluster_rows=FALSE, cluster_cols=FALSE, 
         display_numbers=TRUE, number_color="black", col=colors,
         angle_col=0, legend = FALSE,
         breaks = breaksList,
         labels_col = c("Local\nn=58", "Cabin and sailing\nn=42", "Recreational users and professionals\nn=50"),
         fontsize = 14, fontsize_col = 10, fontsize_row = 10
)

# Effect on professional life

#Water clarity
meanProfValues[1,1] <- mean(values[,2][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[1,2] <- mean(values[,2][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[1,3] <- mean(values[,2][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# seals
meanProfValues[2,1] <- mean(values[,5][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[2,2] <- mean(values[,5][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[2,3] <- mean(values[,5][(values$PrimGroup == "Recreation")], na.rm=TRUE)
#Fucus
meanProfValues[3,1] <- mean(values[,8][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[3,2] <- mean(values[,8][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[3,3] <- mean(values[,8][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Cormorants
meanProfValues[4,1] <- mean(values[,11][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[4,2] <- mean(values[,11][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[4,3] <- mean(values[,11][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Sander
meanProfValues[5,1] <- mean(values[,14][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[5,2] <- mean(values[,14][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[5,3] <- mean(values[,14][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Herring
meanProfValues[6,1] <- mean(values[,17][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[6,2] <- mean(values[,17][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[6,3] <- mean(values[,17][(values$PrimGroup == "Recreation")], na.rm=TRUE)
#Perch
meanProfValues[7,1] <- mean(values[,20][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[7,2] <- mean(values[,20][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[7,3] <- mean(values[,20][(values$PrimGroup == "Recreation")], na.rm=TRUE)
#NIS
meanProfValues[8,1] <- mean(values[,23][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[8,2] <- mean(values[,23][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[8,3] <- mean(values[,23][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# Fil algae
meanProfValues[9,1] <- mean(values[,26][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[9,2] <- mean(values[,26][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[9,3] <- mean(values[,26][(values$PrimGroup == "Recreation")], na.rm=TRUE)
# cyano
meanProfValues[10,1] <- mean(values[,29][(values$PrimGroup == "Local")], na.rm=TRUE)
meanProfValues[10,2] <- mean(values[,29][(values$PrimGroup == "CabinAndSailing")], na.rm=TRUE)
meanProfValues[10,3] <- mean(values[,29][(values$PrimGroup == "Recreation")], na.rm=TRUE)


meanProfValues[order(meanRecrValues$mean, decreasing=TRUE),]

# make a heatmap in the same order as recreational values

pheatmap(meanProfValues[order(meanRecrValues$mean, decreasing=TRUE), 1:3], cluster_rows=FALSE, cluster_cols=FALSE, 
         display_numbers=TRUE, number_color="black", col=colors,
         angle_col=0, legend = FALSE, breaks = breaksList,
         labels_col = c("Local\nn=58", "Cabin and sailing\nn=42", "Recreational users and professionals\nn=50"),
         fontsize = 14, fontsize_col = 10, fontsize_row = 10
)



#anova for the differences of the mean values of ESS between the groups
#strip the data down to Local, cabin & recreation
values2 <- values[values$PrimGroup %in% c("Local", "Cabin", "Recreation"), ]
#water clarity
var.mod<-lm(ClarityRec~PrimGroup, data = values2)
summary(var.mod) #no statistical differences 

#seal
var.mod<-lm(SealRec~PrimGroup, data = values2)
summary(var.mod) #there is a stat differene here!

#fucus
var.mod<-lm(FucusRec~PrimGroup, data = values2)
summary(var.mod) #no statistical differences 

#cormorant
var.mod<-lm(CormorantRec~PrimGroup, data = values2)
summary(var.mod) #no statistical differences 

#sander
var.mod<-lm(SanderRec~PrimGroup, data = values2)
summary(var.mod) #no statistical differences 

#herring
var.mod<-lm(HerringRec~PrimGroup, data = values2)
summary(var.mod) #no statistical differences 



#... etc. I don't think there will be more differences in the recreational use values

write.csv(values, "values_final.csv")
