library(scales)
library(deSolve)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(fields)

#ZERO DISPERSAL GRAPHING

load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/twopatch_ext_zero_ID_full.RData')
#head(twopatch_ext_zero_ID_full)
names(twopatch_ext_zero_ID_full)

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/") #zero dispersal folder

twopatch_ext_complete <- twopatch_ext_zero_ID_full
#re-order by ID not by equilibrium number
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]


#grazing bifurcation graphs
recruitvalue = 0	

twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot

#semi-transparent - all but black are transparent, black on top
pdf(paste0("SemiTransparentZeroDisp_BifurcationGraph_all_updated_nolegend.pdf"))
#png(paste0("SemiTransparentZeroDisp_BifurcationGraph_all_updated_nolegend.png"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue), cex.lab = 2)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", alpha(c("gold","purple"),0.4)), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

pdf(paste0("SemiTransparentZeroDisp_BifurcationGraph_StableOnly_nolegend.pdf"))
#png(paste0("SemiTransparentZeroDisp_BifurcationGraph_StableOnly_nolegend.png"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0],xlim = c(0,1), ylim = c(0,1),pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue), cex.lab=2)
#legend("topleft", c("Stable Node"), col = c("black"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- -20 #otherwise they will look like saddle nodes

pdf(paste0("SemiTransparentZeroDisp_BifurcationGraph_SaddlesOnly_nolegend.pdf"))
#png(paste0("SemiTransparentZeroDisp_BifurcationGraph_SaddlesOnly_nolegend.png"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID == 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID == 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID == 0],xlim = c(0,1), ylim = c(0,1),pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue),cex.lab=2)
#legend("topleft", c("Saddle"), col = c("purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

pdf(paste0("SemiTransparentZeroDisp_BifurcationGraph_UnstableOnly_nolegend.pdf"))
#png(paste0("SemiTransparentZeroDisp_BifurcationGraph_UnstableOnly_nolegend.png"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID == -1], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID == -1], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID == -1],xlim = c(0,1), ylim = c(0,1),pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue), cex.lab=2)
#legend("topleft", c("Unstable Node"), col = c("gold"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Black on top
pdf(paste0("StableonTop_ZeroDisp_BifurcationGraph_all.pdf"))
#png(paste0("StableonTop_ZeroDisp_BifurcationGraph_all.png"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue),cex.lab=2)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$ID > 0], pch = 16)
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#Trajectory Plots/Phase Portraits
#need g = 0.29, recruitment = 0
load('~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr0g0.29_lvl100_20000.RData')
g_val = 0.29
recruitvalue = 0
bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#bluefunc <- colorRampPalette(c("lightblue", "red")) #looked sort of misleading
bluefunc_ten <- bluefunc(15)
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1,500))] <- bluefunc_ten[1]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(501,1000))] <- bluefunc_ten[2]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1001,1500))] <- bluefunc_ten[3]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1501,2000))] <- bluefunc_ten[4]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2001,2500))] <- bluefunc_ten[5]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2501,3000))] <- bluefunc_ten[6]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(3501,4000))] <- bluefunc_ten[7]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4001,4500))] <- bluefunc_ten[8]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4501,5000))] <- bluefunc_ten[9]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5001,5500))] <- bluefunc_ten[10]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5501,6000))] <- bluefunc_ten[11] 
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(6001,7000))] <- bluefunc_ten[12]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(7001,8000))] <- bluefunc_ten[13]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(8001,9000))] <- bluefunc_ten[14]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(9001,10000))] <- bluefunc_ten[15]

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
#pdf(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,".pdf"))
png(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,".png"))
par(mfrow = c(2,2))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalga1 Cover Reef 1", ylab = "Coral Cover Reef 1",cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalga1 Cover Reef 1", ylab = "Macroalga1 Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Coral Cover Reef 1", ylab = "Coral Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalga1 Cover Reef 2", ylab = "Coral Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

dev.off()



#EQUAL DISPERSAL GRAPHING
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/complete_run/Equi tracking/twopatch_ext_complete_IDs.RData')
head(twopatch_ext_complete_IDs)

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/complete_run/Grazing Bifurcation Plots/FinalGraphs") 

#re-order by ID not by equilibrium number
twopatch_ext_complete_IDs$ID[twopatch_ext_complete_IDs$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete_IDs %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

recruitvalue_it = c(0.06,0.3)
SemiTransparentGrazingBifurc_Dispersal <- list()

twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot

for(i in 1:2){
recruitvalue <- recruitvalue_it[i]

#semi-transparent - all but black are transparent, black on top
png(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_updated_nolegend.png"))
#pdf(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_updated_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue), cex.lab=2)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", alpha(c("gold","purple"),0.4)), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Stable Only
png(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_StableOnly_updated_nolegend.png"))
#pdf(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_StableOnly_updated_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue), cex.lab=2)
#legend("topleft", c("Stable Node"), col = c("black"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- -20 #otherwise they will look like saddle nodes

png(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_SaddlesOnly_updated_nolegend.png"))
#pdf(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_SaddlesOnly_updated_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID == 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID == 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID == 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue),cex.lab=2)
#legend("topleft", c("Saddle"), col = c("black"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

png(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_UnstableOnly_updated_nolegend.png"))
#pdf(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"_UnstableOnly_updated_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID == -1], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID == -1], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID == -1], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue), cex.lab=2)
#legend("topleft", c("Unstable Node"), col = c("gold"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()
}


#Trajectory Plots/Phase Portraits
load('~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr0.06g0.29_lvl100_20000.RData')
g_val = 0.29
recruitvalue = 0.06
#need g = 0.29, recruitment = 0.06, 0.3
bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#bluefunc <- colorRampPalette(c("lightblue", "red")) #looked sort of misleading
bluefunc_ten <- bluefunc(15)
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1,500))] <- bluefunc_ten[1]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(501,1000))] <- bluefunc_ten[2]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1001,1500))] <- bluefunc_ten[3]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1501,2000))] <- bluefunc_ten[4]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2001,2500))] <- bluefunc_ten[5]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2501,3000))] <- bluefunc_ten[6]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(3501,4000))] <- bluefunc_ten[7]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4001,4500))] <- bluefunc_ten[8]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4501,5000))] <- bluefunc_ten[9]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5001,5500))] <- bluefunc_ten[10]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5501,6000))] <- bluefunc_ten[11] 
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(6001,7000))] <- bluefunc_ten[12]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(7001,8000))] <- bluefunc_ten[13]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(8001,9000))] <- bluefunc_ten[14]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(9001,10000))] <- bluefunc_ten[15]

#range(twopatch_ext_complete$Equilibrium)
twopatch_ext_complete$symbol <- NA
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 1] <- 1
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 2] <- 2
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 3] <- 3
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 4] <- 4
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 5] <- 5
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 6] <- 6
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 7] <- 15
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 8] <- 8
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 9] <- 16
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 10] <- 17

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
pdf(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,".pdf"))
#png(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,".png"))
par(mfrow = c(2,2))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalga1 Cover Reef 1", ylab = "Coral Cover Reef 1",cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalga1 Cover Reef 1", ylab = "Macroalga1 Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Coral Cover Reef 1", ylab = "Coral Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalga1 Cover Reef 2", ylab = "Coral Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

dev.off()

    
    
    

#UNEQUAL DISPERSAL GRAPHING

load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete_IDs.RData')
head(twopatch_ext_unequal_complete_IDs) #the colour column is from the ID troubleshooting
twopatch_ext_unequal_complete_IDs$colour <- NULL

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Bifurcation Graphs/Grazing Bifurcation Graphs/FinalGraphs") 


#re-order by ID not by equilibrium number
twopatch_ext_unequal_complete_IDs$ID[twopatch_ext_unequal_complete_IDs$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_unequal_complete_IDs %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

#to get rid of floating point errors
twopatch_ext_complete$percentofcoral <- round(twopatch_ext_complete$percentofcoral, digits = 0)
#levels(as.factor(twopatch_ext_complete$p_m)) 4 decimal points is the longest
twopatch_ext_complete$p_m <- round(twopatch_ext_complete$p_m,4)
twopatch_ext_complete$q_m <- round(twopatch_ext_complete$q_m,4)

twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot


recruitvalue = 0.1
lvl = c(0.05,0.1,0.25,0.5)

twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- -20 #otherwise they will look like saddle nodes

for(i in 1:4){
#grazing bifurcation plot
#semi-transparent - all but black are transparent, black on top
png(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_updated_nolegend.png"))
#pdf(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_updated_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
 plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]), cex.lab=2,cex.main=1.5)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", alpha(c("gold","purple"),0.4)), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()
    
#Black on top
png(paste0("StableonTop_GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_updated_nolegend.png"))
#pdf(paste0("StableonTop_GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_updated_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]), cex.lab=2,cex.main=1.5)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black","gold","purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Stable Only
png(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_StableOnly_nolegend.png"))
#pdf(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_StableOnly_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]), cex.lab=2,cex.main=1.5)
#legend("topleft", c("Stable Node"), col = c("black"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Saddles Only
png(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_SaddlesOnly_nolegend.png"))
#pdf(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_SaddlesOnly_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID == 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID == 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID == 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]), cex.lab=2,cex.main=1.5)
#legend("topleft", c("Saddle"), col = c("purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Unstable Only
png(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_UnstableOnly_nolegend.png"))
#pdf(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],"_UnstableOnly_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID == -1], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID == -1], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID == -1], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]), cex.lab=2,cex.main=1.5)
#legend("topleft", c("Unstable Node"), col = c("gold"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()
}

#phase portraits
setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Trajectory Plots/FinalGraphs") 

recruitvalue = 0.1
g_val = 0.27
lvl = c(0.05, 0.1, 0.25, 0.5) #c(1,1,1) #c(1,1,0.05,0.25,0.5)
for(i in 1:length(lvl)){
load(paste0("~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr",recruitvalue,"g",g_val,"_lvl",lvl[j]*100,"_20000.RData"))
#g_val = 0.27
#recruitvalue = 0.1
#lvl = 0.05 #0.1, 0.25, 0.5
#i=1
bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#bluefunc <- colorRampPalette(c("lightblue", "red")) #looked sort of misleading
bluefunc_ten <- bluefunc(15)
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1,500))] <- bluefunc_ten[1]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(501,1000))] <- bluefunc_ten[2]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1001,1500))] <- bluefunc_ten[3]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1501,2000))] <- bluefunc_ten[4]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2001,2500))] <- bluefunc_ten[5]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2501,3000))] <- bluefunc_ten[6]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(3501,4000))] <- bluefunc_ten[7]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4001,4500))] <- bluefunc_ten[8]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4501,5000))] <- bluefunc_ten[9]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5001,5500))] <- bluefunc_ten[10]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5501,6000))] <- bluefunc_ten[11] 
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(6001,7000))] <- bluefunc_ten[12]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(7001,8000))] <- bluefunc_ten[13]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(8001,9000))] <- bluefunc_ten[14]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(9001,10000))] <- bluefunc_ten[15]

#range(twopatch_ext_complete$Equilibrium)
twopatch_ext_complete$symbol <- NA
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 1] <- 1
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 2] <- 2
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 3] <- 3
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 4] <- 4
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 5] <- 5
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 6] <- 6
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 7] <- 15
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 8] <- 8
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 9] <- 16
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 10] <- 17

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
png(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,"_lvl",lvl[i]*100,".png"))
#pdf(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,"_lvl",lvl[i]*100,".pdf"))
par(mfrow = c(2,2))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 1", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val),cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 1", ylab = "Macroalgal Cover Reef 2", cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Coral Cover Reef 1", ylab = "Coral Cover Reef 2", cex.lab=1.5)
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 2", ylab = "Coral Cover Reef 2", cex.lab=1.5)
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))
dev.off()
}

##heat map plots
##figure 4
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/with zero disp included/basinsfinalboth.RData')
basinsfinalabr <- basinsfinalboth[basinsfinalboth[,3] <= 0.45,]  #bc Marty wanted to make the heatmaps ignoring the coral dispersal values greater than 0.4 bc nothing happens above that
setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/Final Graphs")

#RAINBOW VERSION (no legend)  
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lessthan45_wzero_no10_nolabel_nolegend_fixed.pdf")
#png("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lessthan45_wzero_no10_nolabel_nolegend.png")
par(mfrow = c(4,4))
par(mar=c(3,3,3,0), oma = c(1,1,1,1))
#mtext("Grazing Rate", side=1,line=1,cex=2,col="black", outer=TRUE) #isn't working
#double high coral
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,4], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", main = "5%",zlim=c(0,100), ylim = c(0,0.45))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,4], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", main = "25%",zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,4], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", xaxt = "n", main = "50%",zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,4], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", main = "Equal",zlim=c(0,100))

#double high macroalgae
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,5], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", zlim=c(0,100), ylim = c(0,0.45))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,5], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,5], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,5], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100)) 

#mixed mismatch
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,8] + basinsfinalabr[basinsfinalabr[,1] == 0.05,9], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", zlim=c(0,100), ylim = c(0,0.45))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,8] + basinsfinalabr[basinsfinalabr[,1] == 0.25,9], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,8] + basinsfinalabr[basinsfinalabr[,1] == 0.50,9], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,8]+basinsfinalabr[basinsfinalabr[,1] == 1,9], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100)) 

#pure mismatch
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,6] + basinsfinalabr[basinsfinalabr[,1] == 0.05,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", ylab = "dispersal", zlim=c(0,100), ylim = c(0,0.45))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,6] + basinsfinalabr[basinsfinalabr[,1] == 0.25,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,6] + basinsfinalabr[basinsfinalabr[,1] == 0.50,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100))
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,6]+basinsfinalabr[basinsfinalabr[,1] == 1,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100)) 

dev.off()

#RAINBOW VERSION (no legend) - 10% only
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lessthan45_wzero_ONLY10_nolabel_nolegend_fixed.pdf")
#png("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lessthan45_wzero_ONLY10_nolabel_nolegend_fixed.png")
par(mfrow = c(4,1))
par(mar=c(3,3,3,0), oma = c(1,1,1,1))
#double high coral
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.1,4], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", main = "10%",zlim=c(0,100), ylim = c(0,0.45))

#double high macroalgae
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.1,5], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", zlim=c(0,100), ylim = c(0,0.45))

#mixed mismatch
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.1,8] + basinsfinalabr[basinsfinalabr[,1] == 0.05,9], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", zlim=c(0,100), ylim = c(0,0.45))

#pure mismatch
image(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.1,6] + basinsfinalabr[basinsfinalabr[,1] == 0.05,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", ylab = "dispersal", zlim=c(0,100), ylim = c(0,0.45)) 

dev.off()

pdf("justneedtherainbow.pdf")
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,6] + basinsfinalabr[basinsfinalabr[,1] == 0.05,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", ylab = "dispersal", zlim=c(0,100), ylim = c(0,0.45))
dev.off()



#HETEROGENEOUS GRAZING

load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_ID_full.RData')
#head(twopatch_ext_heterograz_ID_full)
names(twopatch_ext_heterograz_ID_full)

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/FinalBifurcPlots") #heterogeneous grazing folder

twopatch_ext_complete <- twopatch_ext_heterograz_ID_full 
#re-order by ID not by equilibrium number
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

#supp fig bifurcation plots
#dispersal bifurc plots for supp fig x1: g = 0.6, g_lvl = 0.1; g = 0.2, g_lvl = 0.5
gval = c(0.6, 0.2)
g_level = c(0.1, 0.5)


twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- -20 #otherwise they will look like saddle nodes
twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot
for(i in 1:2){
#semi-transparent - all but black are transparent, black on top
#png(paste0("SemiTransparentDispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("SemiTransparentDispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6.5,4,1)+.1)
 plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i]], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i]], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i]],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Dispersal Level", ylab = "Coral Cover Reef 1", main = paste("Grazing Rate = ", gval[i], "and Grazer Level =", g_level[i]), cex.lab=2, cex.main=1.5)
 points(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", alpha(c("gold","purple"),0.4)), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Black on Top
#png(paste0("StableOnTop_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("StableonTop_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6.5,4,1)+.1)
 plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i]], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i]], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i]], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "Coral Cover Reef 1", main = paste("Grazing Rate = ", gval[i], "and Grazer Level =", g_level[i]), cex.lab=2, cex.main=1.5)
 points(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", "gold","purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()    

#Stable Only
#png(paste0("StableOnly_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("StableOnly_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6.5,4,1)+.1)
 plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "Coral Cover Reef 1", main = paste("Grazing Rate = ", gval[i], "and Grazer Level =", g_level[i]), cex.lab=2, cex.main=1.5)
#legend("topleft", c("Stable Node"), col = c("black"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off() 

#Saddles Only
#png(paste0("SaddlesOnly_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("SaddlesOnly_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6.5,4,1)+.1)
 plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "Coral Cover Reef 1", main = paste("Grazing Rate = ", gval[i], "and Grazer Level =", g_level[i]), cex.lab=2, cex.main=1.5)
#legend("topleft", c("Saddle"), col = c("purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off() 

#Unstable Only
#png(paste0("UnstableOnly_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("UnstableOnly_DispersalBifurc_Grazing", gval[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6.5,4,1)+.1)
 plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == -1], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == -1], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == -1], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "Coral Cover Reef 1", main = paste("Grazing Rate = ", gval[i], "and Grazer Level =", g_level[i]), cex.lab=2, cex.main=1.5)
#legend("topleft", c("Unstable Node"), col = c("gold"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off() 
}

#Grazing Bifurcation Graphs
recruitvalue = c(0.1, 0.05, 0.17, 0,0)
g_level = c(0.5, 0.5, 0.1, 0.5,0.1)

twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- -20 #otherwise they will look like saddle nodes
twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot


for(i in 1:5){
#grazing bifurcation plot
#semi-transparent - all but black are transparent, black on top
#png(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("SemiTransparentGrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
 plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i]], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i]], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i]],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal Level = ", recruitvalue[i], "and Grazer Level =", g_level[i]),cex.lab=2,cex.main=1.5)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black", alpha(c("gold","purple"),0.4)), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()
    
#Black on top
#png(paste0("StableonTop_GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_nolegend.png"))
pdf(paste0("StableonTop_GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i]], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i]], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i]], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal Level = ", recruitvalue[i], "and Grazer Level =", g_level[i]),cex.lab=2,cex.main=1.5)
points(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], pch = 16)
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle"), col = c("black","gold","purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Stable Only
#png(paste0("GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_StableOnly_nolegend.png"))
pdf(paste0("GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_StableOnly_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID >0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal Level = ", recruitvalue[i], "and Grazer Level =", g_level[i]),cex.lab=2,cex.main=1.5)
#legend("topleft", c("Stable Node"), col = c("black"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Saddles Only
#png(paste0("GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_SaddlesOnly_nolegend.png"))
pdf(paste0("GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_SaddlesOnly_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID ==0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal Level = ", recruitvalue[i], "and Grazer Level =", g_level[i]),cex.lab=2,cex.main=1.5)
#legend("topleft", c("Saddle"), col = c("purple"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

#Unstable Only
#png(paste0("GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_UnstableOnly_nolegend.png"))
pdf(paste0("GrazingBifurc_Dispersal", recruitvalue[i],"grazinglevel",g_level[i],"_UnstableOnly_nolegend.pdf"))
par(mar=c(5,6,4,1)+.1)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == -1], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID == -1], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue[i] & twopatch_ext_complete$g2 == g_level[i] & twopatch_ext_complete$ID ==-1], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Grazing Rate", ylab = "Coral Cover Reef 1", main = paste("Dispersal Level = ", recruitvalue[i], "and Grazer Level =", g_level[i]),cex.lab=2,cex.main=1.5)
#legend("topleft", c("Unstable Node"), col = c("gold"), pch = c(20,20))
#legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point"), col = c("black", alpha(c("gold","purple","green"),0.4)), pch = c(20,20))
dev.off()

}

#making some mumbytraj plots for the supp mat
#plotting mumbytraj to check what's going on
#load in equi
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_ID_full.RData')
twopatch_ext_complete <- twopatch_ext_heterograz_ID_full #range(ID) = 302, good

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/Trajectory Plots")

recruitvalue_vr = c(0.8,0.8,0.1, 0.05, 0.17, 0,0)
g_level_vr = c(0.1,0.5,0.5, 0.5, 0.1, 0.5,0.1)
g_val_vr = c(0.6,0.2,0.1,0.25,0.55,0.1,0.5)

for(i in 1:length(g_val_vr)){
#load in mumbytraj of choice
mumbytrajectories <- NULL
#folder based on dispersal level
#load('/Volumes/BackupPlus/mumbytraj_heteroggrazlong_full/mumbytraj_heteroggrazlong0/mumbytrajectories_recr0g0.5_glvl0.1_20000.RData')
g_val = g_val_vr[i]
recruitvalue = recruitvalue_vr[i]
glvl = g_level_vr[i]
load(paste0("~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_20000.RData"))


bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#bluefunc <- colorRampPalette(c("lightblue", "red")) #looked sort of misleading
bluefunc_ten <- bluefunc(15)
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1,500))] <- bluefunc_ten[1]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(501,1000))] <- bluefunc_ten[2]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1001,1500))] <- bluefunc_ten[3]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1501,2000))] <- bluefunc_ten[4]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2001,2500))] <- bluefunc_ten[5]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2501,3000))] <- bluefunc_ten[6]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(3001,4000))] <- bluefunc_ten[7]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4001,5000))] <- bluefunc_ten[8]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5001,6000))] <- bluefunc_ten[9]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(6001,7000))] <- bluefunc_ten[10]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(7001,10000))] <- bluefunc_ten[11] 
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(10001,12000))] <- bluefunc_ten[12]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(12001,15000))] <- bluefunc_ten[13]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(15001,17000))] <- bluefunc_ten[14]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(17001,20000))] <- bluefunc_ten[15]

#would ideally give every ID a unique symbol but there's 95 of them...need to think about this, going to stick with the equi version for now bc that's more reliable
range(twopatch_ext_complete$Equilibrium) #1 to 16 
twopatch_ext_complete$symbol <- NA
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 1] <- 1
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 2] <- 2
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 3] <- 3
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 4] <- 4
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 5] <- 5
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 6] <- 6
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 7] <- 11
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 8] <- 8
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 9] <- 16
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 10] <- 17
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 11] <- 15
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 12] <- 18
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 13] <- 19
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 14] <- 20
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 15] <- 12
twopatch_ext_complete$symbol[twopatch_ext_complete$Equilibrium == 16] <- 14

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
#PUT STABLE NODES ON TOP
#png(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,"heterograz_glvl",glvl,".png"))
pdf(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,"heterograz_glvl",glvl,".pdf"))
par(mfrow = c(2,2))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 1", ylab = "Coral Cover Reef 1", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl), cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 1", ylab = "Macroalgal Cover Reef 2", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl),cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Coral Cover Reef 1", ylab = "Coral Cover Reef 2", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl),cex.lab=1.5)
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 2", ylab = "Coral Cover Reef 2", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl),cex.lab=1.5)
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID <= 0],  xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$ID > 0],  xlim = c(0,1), ylim = c(0,1))

dev.off()
}


#scenario 3 (figure 5)
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/afterdealingwith2856-2900problems/hg_basinsfinal_done_alllong.RData')
sum(basinsfinal[,13]) #41 of these still haven't finished, 2968131/2970000
didntfinish_alllong <- which(round(basinsfinal[,13],4) < 100)
numtrajnotdone <- 210*length(didntfinish_alllong)-sum(basinsfinal[didntfinish_alllong,13]*(210/100)) #3925/6,237,000 trajectories didn't finish
iteratorhg_stillnotdone <- basinsfinal[didntfinish_alllong,1:3]

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/FinalBOAplots")

#RAINBOW VERSION (low label, joined equi)
#equitypeone: high coral zero macroalgae both patches, equitypetwo: high macroalgae zero coral both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0, equitypeseven: high coral M > 0 both patches, equitypeeight: high macroalgae C > 0 both patches, equitypenine: C = M both patches

basinsfinalabr <- basinsfinal[basinsfinal[,3] <= 0.45,]

pdf("AllHeatMapsOrganizedbyEqui_heterograz_nolabel_joinedequi_nolegend_truncbottomrow.pdf")
#png("AllHeatMapsOrganizedbyEqui_heterograz_nolabel_joinedequi_nolegend_truncbottomrow.png")
par(mfrow = c(4,3))
par(mar=c(2,3,2,0), oma = c(1,1,1,1))
#equitypeone
image(t(matrix(basinsfinal[1:9900,4], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  ylab = "dispersal", xaxt = "n", main = "low grazer level",zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,4], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", main = "medium grazer level",zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,4], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", xaxt = "n", main = "high grazer level",zlim=c(0,100))

#equitypetwo
image(t(matrix(basinsfinal[1:9900,5], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,5], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,5], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100))

#equitypefive + equitypesix
image(t(matrix(basinsfinal[1:9900,8]+basinsfinal[1:9900,9], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,8]+basinsfinal[9901:19800,9], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", xaxt = "n", zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,8]+basinsfinal[19801:29700,9], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", xaxt = "n", zlim=c(0,100))

#equitypethree + equitypefour
image(t(matrix(basinsfinalabr[1:4554,6]+basinsfinalabr[1:4554,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", ylab = "dispersal", zlim=c(0,100),ylim = c(0,0.45))
image(t(matrix(basinsfinalabr[4555:9108,6]+basinsfinalabr[4555:9108,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100), ylim = c(0,0.45))
image(t(matrix(basinsfinalabr[9109:13662,6]+basinsfinalabr[9109:13662,7], nrow = 46, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100), ylim = c(0,0.45))

dev.off()



pdf("AllHeatMapsOrganizedbyEqui_heterograz_lowlabel_joinedequi_nolegend.pdf")
par(mfrow = c(4,3))
par(mar=c(2,3,2,0), oma = c(1,1,1,1))
#equitypeone
image(t(matrix(basinsfinal[1:9900,4], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  ylab = "dispersal", main = "low grazer level",zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,4], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", main = "medium grazer level",zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,4], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", main = "high grazer level",zlim=c(0,100))

#equitypetwo
image(t(matrix(basinsfinal[1:9900,5], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal", zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,5], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,5], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n",zlim=c(0,100))

#equitypefive + equitypesix
image(t(matrix(basinsfinal[1:9900,8]+basinsfinal[1:9900,9], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), ylab = "dispersal",zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,8]+basinsfinal[9901:19800,9], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), yaxt="n", zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,8]+basinsfinal[19801:29700,9], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)),  yaxt="n", zlim=c(0,100))

#equitypethree + equitypefour
image(t(matrix(basinsfinal[1:9900,6]+basinsfinal[1:9900,7], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", ylab = "dispersal", zlim=c(0,100))
image(t(matrix(basinsfinal[9901:19800,6]+basinsfinal[9901:19800,7], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100))
image(t(matrix(basinsfinal[19801:29700,6]+basinsfinal[19801:29700,7], nrow = 100, ncol = 99)), col = c("grey90", tim.colors(300)), xlab = "grazing", yaxt="n", zlim=c(0,100))

#equitypeeight
#image.plot(t(matrix(basinsfinal[1:9900,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2, C non-zero, low grazing",zlim=c(0,100))
#image.plot(t(matrix(basinsfinal[9901:19800,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
#image.plot(t(matrix(basinsfinal[19801:29700,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeseven (high C1C2, M nonzero), equitypenine (M=C everywhere) never happen

dev.off()


pdf("AllHeatMapsOrganizedbyEqui_heterograz_lowlabel_joinedequi_v2.pdf")
par(mfrow = c(4,3))

#equitypeone
image.plot(t(matrix(basinsfinal[1:9900,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypetwo
image.plot(t(matrix(basinsfinal[1:9900,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypefive + equitypesix
image.plot(t(matrix(basinsfinal[1:9900,8]+basinsfinal[1:9900,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Mismatch mixed, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,8]+basinsfinal[9901:19800,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,8]+basinsfinal[19801:29700,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypethree + equitypefour
image.plot(t(matrix(basinsfinal[1:9900,6]+basinsfinal[1:9900,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Mismatch nonmixed, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,6]+basinsfinal[9901:19800,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,6]+basinsfinal[19801:29700,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeeight
#image.plot(t(matrix(basinsfinal[1:9900,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2, C non-zero, low grazing",zlim=c(0,100))
#image.plot(t(matrix(basinsfinal[9901:19800,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
#image.plot(t(matrix(basinsfinal[19801:29700,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeseven (high C1C2, M nonzero), equitypenine (M=C everywhere) never happen

dev.off()

