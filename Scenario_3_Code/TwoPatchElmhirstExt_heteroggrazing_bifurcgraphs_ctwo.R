library(scales)
library(deSolve)
library(tidyverse)


load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_ID_full.RData')
#head(twopatch_ext_heterograz_ID_full)
names(twopatch_ext_heterograz_ID_full)

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/C2graphs/") #heterogeneous grazing folder

twopatch_ext_complete <- twopatch_ext_heterograz_ID_full 
#re-order by ID not by equilibrium number
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

#loop through all recruitment values, and all 3 levels of g2
g_lvl = c(0.1,0.3,0.5)
for(i in 1:100){
	for(j in 1:3){
recruitvalue = round((i-1)/100,4)
g_level = g_lvl[j]	

#stable only
#pdf(paste0("StableOnly_Heterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".pdf"))
png(paste0("Grazing_Bifurcation_Graphs/Stable_Only/StableOnly_Heterograz_GrazingBifurcationGraph_C2_dispersal",recruitvalue,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C2 cover", main = paste("Only Stable Nodes, Dispersal = ", recruitvalue, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#all
#pdf(paste0("Heterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".pdf"))
png(paste0("Grazing_Bifurcation_Graphs/Normal/Heterograz_GrazingBifurcationGraph_C2_dispersal",recruitvalue,"grazinglevel",g_level,".png")) 
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C2 cover", main = paste("Dispersal = ", recruitvalue, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent
#pdf(paste0("TransparentHeterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".pdf"))
png(paste0("Grazing_Bifurcation_Graphs/Transparent/TransparentHeterograz_GrazingBifurcationGraph_C2_dispersal",recruitvalue,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C2 cover", main = paste("Dispersal = ", recruitvalue, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
dev.off()
	}
}

#loop through all grazing values, and all 3 levels of g2
g_lvl = c(0.1,0.3,0.5)
for(i in 1:99){
	for(j in 1:3){
gval = round(i/100,4)
g_level = g_lvl[j]	

#stable only
#pdf(paste0("Dispersal_Bifurcation_Graphs/Stable_Only/StableOnly_Heterograz_DispersalBifurcationGraph_C2_g",gval,"grazinglevel",g_level,".pdf"))
png(paste0("Dispersal_Bifurcation_Graphs/Stable_Only/StableOnly_Heterograz_DispersalBifurcationGraph_C2_g",gval,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "C2 cover", main = paste("Only Stable Nodes, g = ", gval, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#all
#pdf(paste0("Dispersal_Bifurcation_Graphs/Normal/Heterograz_DispersalBifurcationGraph_C2_g",gval,"grazinglevel",g_level,".pdf")) 
png(paste0("Dispersal_Bifurcation_Graphs/Normal/Heterograz_DispersalBifurcationGraph_C2_g",gval,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level], y = twopatch_ext_complete$C2[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "C2 cover", main = paste("g = ", gval, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent
#pdf(paste0("Dispersal_Bifurcation_Graphs/Transparent/TransparentHeterograz_DispersalBifurcationGraph_C2_g",gval,"grazinglevel",g_level,".pdf"))
png(paste0("Dispersal_Bifurcation_Graphs/Transparent/TransparentHeterograz_DispersalBifurcationGraph_C2_g",gval,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$p_c[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level], y = twopatch_ext_complete$C2[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$g == gval & twopatch_ext_complete$g2 == g_level],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "dispersal level", ylab = "C2 cover", main = paste("g = ", gval, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
dev.off()
	}
}

