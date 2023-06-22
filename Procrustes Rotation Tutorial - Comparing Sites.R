#Procrustes Analysis Tutorial - Comparing Sites
#Author: Nicole Regimbal - Just One Bird's Opinion 
#Date: June 22, 2023

#Load in necessary libraries
#If these libraries are not yet installed, first use the function install.packages()
library(ggplot2)
library(vegan)

#Set your working directory
getwd() #This tells R that you want to get a new working directory
setwd("Insert your path here") #This tells R where to find that new directory
dir() #This tells R to retrieve this new directory to use 

#Comparing two PCA's 
#Comparing inventory/mortality for same species at the same sites
#Are the species present and experiencing mortality the same for each site?

#Inventory PCA
inv <- read.csv("inventory_data.csv", row.names = 1)
inv <- na.omit(inv)
inv.pca <- rda(inv, scale=TRUE)
summary(inv.pca)

#Mortality PCA
mort <- read.csv("mort_data.csv", row.names = 1)
mort <- na.omit(mort)
mort.pca <- rda(mort, scale=TRUE)
summary(mort.pca)


#Biplots for species mortality
par(mfrow=c(1,2))
biplot(mort.pca, scaling =1, type="text",  xlab = "PC1 (45.8%)", ylab = "PC2 (17.4%)")
biplot(inv.pca, scaling =1,type="text", xlab = "PC1 (30.1%)", ylab = "PC2 (21.6%)")


#Inventory and Mortality Procrustes Rotation
proc <- procrustes(inv.pca, mort.pca, scale=TRUE)

par(mfrow=c(1,1))
plot(proc)
text(
  proc,
  display = "target",
  col = "red",
  pos = 4,
  cex=0.6
)


#Significance test of Procrustes
protest <- protest(X=inv.pca, Y=mort.pca, scores="sites", permutations=999)
protest
