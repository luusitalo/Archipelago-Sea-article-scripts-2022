############################
# Discretize continuous data tables
# Laura Uusitalo 18.5.2021
############################ 

library(arules) # for discretization

setwd("D:/Users/uusitalol/RStudio/2021 AS EwE to Hugin/")

#doesn't help, apparently dicretizeDF doesn't use this :( 
#options(scipen = 999)

cont <- read.csv("ArchipelagoSeaResultsForHugin.csv")

#remove zero-filled columns:
cont<-cont[,c(1:49)]

# Add a Total_phytoplankton_BM variable by summing PhytoBM and CyanoBM
# This is for the purposes of the modelling where we have a value for water clarity
cont$Total_phytoplankton_BM <- cont$Phytoplankton_BM + cont$Cyanobacteria_BM


#make a version with only the variables we're going to use
keep <- c(1,2,3,6,9,12,13,15,18,26,27,29,31,32,33,36,38,39,41,45,46,50)
cont <- cont[, keep]

#discretize into 10 bins with equal frequency
discrFreq <- discretizeDF(
  cont, 
  methods=list(
    year = list(method="interval", breaks = 10
    )),
  default = list(method = "frequency", breaks = 10)
)


# Format the file so that Hugin recognizes it as interval data,
# i.e. remove the parentheses & replace comma with dash & e with E

discrFreq[] <- lapply(discrFreq, gsub, pattern='\\(', replacement='')
discrFreq[] <- lapply(discrFreq, gsub, pattern='\\)', replacement='')
discrFreq[] <- lapply(discrFreq, gsub, pattern='\\[', replacement='')
discrFreq[] <- lapply(discrFreq, gsub, pattern='\\]', replacement='')
discrFreq[] <- lapply(discrFreq, gsub, pattern=',', replacement=' - ')
discrFreq[] <- lapply(discrFreq, gsub, pattern='e', replacement='E')


#save the year as a label rather than an interval
#"Y"s are needed so Hugin doesn't insist on reading these as numerical

discrFreq[] <- lapply(discrFreq, gsub, pattern='2E\\+03 - 2\\.01E\\+03', replacement='Y 2000-2009')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.01E\\+03 - 2\\.02E\\+03', replacement='Y 2010-2019')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.02E\\+03 - 2\\.03E\\+03', replacement='Y 2020-2029')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.03E\\+03 - 2\\.04E\\+03', replacement='Y 2030-2039')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.04E\\+03 - 2\\.05E\\+03', replacement='Y 2040-2049')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.05E\\+03 - 2\\.06E\\+03', replacement='Y 2050-2059')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.06E\\+03 - 2\\.07E\\+03', replacement='Y 2060-2069')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.07E\\+03 - 2\\.08E\\+03', replacement='Y 2070-2079')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.08E\\+03 - 2\\.09E\\+03', replacement='Y 2080-2089')
discrFreq[] <- lapply(discrFreq, gsub, pattern='2\\.09E\\+03 - 2\\.1E\\+03', replacement='Y 2090-2099')


################
# Save both files
write.csv(discrFreq, "ArchipelagoSeaResultsDiscretizedEqualFreq.csv", row.names = FALSE)  
