load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete_IDs.RData')
head(twopatch_ext_unequal_complete_IDs) #the colour column is from the ID troubleshooting
twopatch_ext_unequal_complete_IDs$colour <- NULL

library(scales)
library(deSolve)
library(tidyverse)


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


recruitvalue = 0.01
g_value = 0.01
lvl = 0.05

#testing...
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue*lvl & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue*lvl & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue*lvl & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue*lvl & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue*lvl & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue*lvl & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Only Stable Nodes, Dispersal = ", recruitvalue, "and level =", lvl))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
setwd('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Bifurcation Graphs')
for(i in 1:4){
	for(k in 1:99){
	for(j in 1:99){
recruitvalue = k/100
g_value =j/100
lvl = c(0.05,0.1,0.25,0.5)
print(paste("recruitvalue =", recruitvalue, "g_value =", g_value, "percentofcoral", lvl[i]))

#grazing bifurcation plot
png(paste0("GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#grazing bifurcation plot - only stable nodes, think this is the best i can do
png(paste0("StableOnly_GrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Only Stable Nodes, Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#dispersal bifurcation plot
#missing: q_c = 0.35,0.69,0.70 (not for 5,10 but yes for 25,50) - weird (5.30.2019), FIXED - floating point error, see rounding above
png(paste0("DispersalBifurc_Grazing", g_value,"percentofcoral",lvl[i],".png"))
plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal (q_c)", ylab = "C cover", main = paste("g =", g_value, "level = ", lvl[i]))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent grazing bifurcation plot
    png(paste0("TransparentGrazingBifurc_Dispersal", recruitvalue,"percentofcoral",lvl[i],".png"))
    plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl[i],4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl[i],4)],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Coral Dispersal = ", recruitvalue, "Macroalgal Dispersal = ", recruitvalue*lvl[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
    #stable only dispersal bifurcation plot
    png(paste0("StableOnly_DispersalBifurc_Grazing", g_value,"percentofcoral",lvl[i],".png"))
    plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value & twopatch_ext_complete$ID > 0 & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value & twopatch_ext_complete$ID > 0 & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value & twopatch_ext_complete$ID > 0 & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal (q_c)", ylab = "C cover", main = paste("Only Stable Nodes, g =", g_value, "level = ", lvl[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    #transparent dispersal bifurcation plot
    png(paste0("TransparentDispersalBifurc_Grazing", g_value,"percentofcoral",lvl[i],".png"))
    plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value & twopatch_ext_complete$percentofcoral ==  round(lvl[i]*100,0)],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal (q_c)", ylab = "C cover", main = paste("g =", g_value, "level = ", lvl[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()

}}
}

#troubleshooting why those dispersal values are missing: ...and floating point error, wwhoo
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete.RData')
head(twopatch_ext_unequal_complete)
twopatch_ext_unequal_complete$percentofcoral <- round(twopatch_ext_unequal_complete$percentofcoral,0) #that seemed to fix the problem
twopatch_ext_unequal_complete[twopatch_ext_unequal_complete$q_c == 0.35 & twopatch_ext_unequal_complete$percentofcoral == 5,] 
twopatch_ext_unequal_complete[twopatch_ext_unequal_complete$q_c == 0.35 & twopatch_ext_unequal_complete$percentofcoral == 10,] 
twopatch_ext_unequal_complete[twopatch_ext_unequal_complete$q_c == 0.35 & twopatch_ext_unequal_complete$p_m == 0.35*0.05,]
twopatch_ext_unequal_complete[twopatch_ext_unequal_complete$q_c == 0.35 & twopatch_ext_unequal_complete$p_m == 0.0175,]  
str(twopatch_ext_unequal_complete)