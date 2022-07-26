library(scales)
library(deSolve)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(fields)

#load in zero dispersal values
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/twopatch_ext_zero_ID_full.RData')
#head(twopatch_ext_zero_ID_full)
names(twopatch_ext_zero_ID_full)

#twopatch_ext_complete <- twopatch_ext_zero_ID_full
#re-order by ID not by equilibrium number
twopatch_ext_zero_ID_full$ID[twopatch_ext_zero_ID_full$stability == "unstable_node"] <- -1
twopatch_ext_zero_ID_full <- twopatch_ext_zero_ID_full %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

twopatch_ext_zero_ID_full$Colour[twopatch_ext_zero_ID_full$Colour == "green"] <- "white" #removing the green dot

#load in equal dispersal values (disp > 0)
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/complete_run/Equi tracking/twopatch_ext_complete_IDs.RData')
head(twopatch_ext_complete_IDs)

#re-order by ID not by equilibrium number
twopatch_ext_complete_IDs$ID[twopatch_ext_complete_IDs$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete_IDs %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot

#semi-transparent - all but black are transparent, black on top
pdf(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/SemiTransparent_DispBifurcationGraph_all_updated_nolegend.pdf"))
#png(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/SemiTransparent_DispBifurcationGraph_all_updated_nolegend.png"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == 0.29], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == 0.29], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$g == 0.29],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Dispersal Level", ylab = "Coral Cover Reef 1", main = paste("Grazing = ", 0.29), cex.lab = 2)

points(x = twopatch_ext_zero_ID_full$p_c[twopatch_ext_zero_ID_full$g == 0.29], y = twopatch_ext_zero_ID_full$C1[twopatch_ext_zero_ID_full$g == 0.29], col = alpha(twopatch_ext_zero_ID_full$Colour[twopatch_ext_zero_ID_full$g == 0.29],0.4), pch = 16)

points(x = twopatch_ext_zero_ID_full$p_c[twopatch_ext_zero_ID_full$g == 0.29 & twopatch_ext_zero_ID_full$ID > 0], y = twopatch_ext_zero_ID_full$C1[twopatch_ext_zero_ID_full$g == 0.29 & twopatch_ext_zero_ID_full$ID > 0], col = twopatch_ext_zero_ID_full$Colour[twopatch_ext_zero_ID_full$g == 0.29 & twopatch_ext_zero_ID_full$ID > 0], pch = 16)

points(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == 0.29 & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == 0.29 & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == 0.29 & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", alpha(c("gold","purple"),0.4)), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()


