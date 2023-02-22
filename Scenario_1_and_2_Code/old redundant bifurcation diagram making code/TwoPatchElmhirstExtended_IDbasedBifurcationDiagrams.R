load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/complete_run/Equi tracking/twopatch_ext_complete_IDs.RData')
head(twopatch_ext_complete_IDs)

library(scales)
library(deSolve)
library(tidyverse)


#re-order by ID not by equilibrium number
twopatch_ext_complete_IDs$ID[twopatch_ext_complete_IDs$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete_IDs %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]



#testing...
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Only Stable Nodes, Dispersal = ", recruitvalue))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))


for(k in 1:99){
	for(j in 1:99){
recruitvalue = k/100
g_value =j/100
print(paste("recruitvalue =", recruitvalue, "g_value =", g_value))

#grazing bifurcation plot
png(paste0("GrazingBifurc_Dispersal", recruitvalue,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Dispersal = ", recruitvalue))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#grazing bifurcation plot - only stable nodes, think this is the best i can do
png(paste0("StableOnly_GrazingBifurc_Dispersal", recruitvalue,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Only Stable Nodes, Dispersal = ", recruitvalue))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#recruitment bifurcation plot
png(paste0("DispersalBifurc_Grazing", g_value,".png"))
plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal (p_c)", ylab = "C cover", main = paste("g =", g_value))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent grazing bifurcation plot
    png(paste0("TransparentGrazingBifurc_Dispersal", recruitvalue,".png"))
    plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Dispersal = ", recruitvalue))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
    #stable only dispersal bifurcation plot
    png(paste0("StableOnly_DispersalBifurc_Grazing", g_value,".png"))
    plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal (p_c)", ylab = "C cover", main = paste("Only Stable Nodes, g =", g_value))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    #transparent dispersal bifurcation plot
    png(paste0("TransparentDispersalBifurc_Grazing", g_value,".png"))
    plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal (p_c)", ylab = "C cover", main = paste("g =", g_value))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()

}}
