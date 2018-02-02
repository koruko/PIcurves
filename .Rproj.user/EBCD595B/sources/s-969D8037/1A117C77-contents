#This is a R script for managing data from PAM curves obtained by Diving-PAM (Heinz Walz, Effeltrich, Germany)
#
#
##First we need to read the .csv file and create as many data frames as measurements we have in our raw file
#
#Check the files in our WD
list.files()
#Change the name of the file if needed
file.rename("PAM.CSV","PAMDATA.CSV")
#Now let's read the csv and create a data frame from it in a new variable
data <- read.csv("PAMDATA.CSV",header = TRUE, sep = ",", dec = ".")
par <- read.csv("PAR.CSV", header = FALSE)
colnames(par) = c("PAR") #If you want to enter externally measured PAR values
#
#
#Now we have a data frame to manage our data
#First let's choose the columns we need
data2 <- data[complete.cases(data),c("No.", "F", "Fm.", "Yield", "qP", "qN", "NPQ")]

#Let's add a new column for the n=4 replicates and PAr irradiances of each curve
n <- c(1, 2, 3, 4)
I <- rep(par$PAR, times = nrow(par), length.out = nrow(data2))
replicate <- rep(n, each = nrow(par), length.out = nrow(data2)) #Change each = number of PAR irradiances
data2$n <- replicate
data2$PAR <- I
head(data2)

#Rearrange and rename the columns
data3 <- data2[,c("No.", "n", "PAR", "F", "Fm.", "Yield", "qP", "qN", "NPQ")]
colnames(data3) <- c("LC", "n", "PAR", "Fo", "Fm", "Y(II)", "qP", "qN", "NPQ")

#Parameter calculations (Schreiber et al. 1986)
#Maximum quantum yield of PSII = "Yield". This parameter reflects the physiologicla fitness in photosynthetic organisms (Maxwell and Johnson, 2000)
#  Fo <- data3$Fo #Initial fluorescence after dark-acclimation
#  Fm <- data3$Fm #Saturating pulse of actinic light that induces maximal fluorescence
# FvFm <- (Fm - Fo) / Fm
#Electron transport rates (ETR) as an estimate of gross photosynthesis
AB <- 0.5 #Estimated fraction of photons absorbed by the photosynthetic pigments associated with PSII
AF <- 0.7 #Absorption factor, calculated by measuring the fraction of the incident PAR absorbed by thalli, as A = 1 - T, where T is the transmitance and assuming no significant reflectance
ETR <- data3$`Y(II)` * data3$PAR * AB * AF
data3$ETR <- ETR

#Now let's modify the row names for LC (pending task!)

#Checking the data structure, dimensions, etc.
dim.data.frame(data3)
summary.data.frame(data3)
str(data3)

#Now let's create and plot individual ETRs for each replicate
library(ggplot2)
pp <- nrow(par)*length(n) #PAR points* nº replicates
nr <- nrow(data3)
ETR_split <- split(data3, rep(1:ceiling(nr/pp), each=pp, length.out=nr))
for (i in 1:length(ETR_split)) 
  {plot  (ETR_split[[i]]$PAR, ETR_split[[i]]$`ETR`)
}


