library(scales)
library(deSolve)
library(fields)

#load + compile data
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_ID_full.RData')
twopatch_ext_complete <- twopatch_ext_heterograz_ID_full #range of ID: 0 -> 302 (added in ID 302 to deal with 2855-2900)

#should put all of the basinsabr dataframes into one big dataframe...or matrix to make looping easier
glvl <- c(rep(0.1,100*99),rep(0.3,100*99),rep(0.5,100*99))
grazing <- c(rep(seq(0.01,0.99,0.01), each = 100),rep(seq(0.01,0.99,0.01), each = 100),rep(seq(0.01,0.99,0.01), each = 100))
recruit <-c(rep(seq(0,0.99,0.01), 99),rep(seq(0,0.99,0.01), 99),rep(seq(0,0.99,0.01), 99))
equitypeone <- equitypetwo <- equitypethree <- equitypefour <- equitypefive <- equitypesix <- equitypeseven <- equitypeeight <- equitypenine <- rep(0,length(glvl))
summ <- rep(0,length(glvl))
#exist <- rep(-1,length(glvl)) #-1 if still need to load in basins file, 0 if don't
basinsfinal <-  matrix(c(glvl,grazing,recruit,equitypeone,equitypetwo,equitypethree,equitypefour,equitypefive,equitypesix, equitypeseven, equitypeeight, equitypenine,summ),nrow=length(glvl),ncol=13)
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/didntfinish_hg.RData')
#length(didntfinish) #11344
#equitypeone: high coral zero macroalgae both patches, equitypetwo: high macroalgae zero coral both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0, equitypeseven: high coral M > 0 both patches, equitypeeight: high macroalgae C > 0 both patches, equitypenine: C = M both patches
basinsabr <- 0
stopp = 500

#i = 2855 STILL not working...so odd - the file doesn't exist still...why
#i = 2855-2900? still not working - not working because need to fix the ones that follow from the one with no stable node (g = 0.29, recruitvalue = 0.54, glvl = 0.1) as they just have ID = NA. Could give them all ID = 302 bc they all seem to be the same equi and even if it is similar to another it apparently wasn't similar enough
#i = 2855 is never going to work but 2856-2900 work now
for(i in 1:length(summ)){ #1:length(summ)
print(paste("i =",i))
glvl <- basinsfinal[i,1]		
g_val <- basinsfinal[i,2]	
recruitvalue <- recruit <- round(basinsfinal[i,3],4)
load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggrazlong_abr/basins_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_20000.RData"))
#if(i %in% didntfinish){ #comes from below #OLD: just re-did all of them in the long form
#load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggrazlong_abr/basins_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_20000.RData"))
#}	
#else{load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggraz_abr/basins_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_5850.RData"))}

for(j in 1:350){ #bc set to 350 diff possible ID values
print(paste("j =",j))
if(basinsabr$Size[basinsabr$EquilibriumID == j] > 0){
#equitypeone = high coral zero macroalgae both patches - M1=M2=0, C1 >0, C2 > 0
if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,4] <-  basinsfinal[i,4] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypetwo = high macroalgae zero coral both patches - C1=C2=0, M1 >0, M2 > 0
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,5] <-  basinsfinal[i,5] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypethree: High C1 High M2 C2=M1=0 (basically: C2=M1=0, C1 > 0, M2 > 0)
if((twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,6] <-  basinsfinal[i,6] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypefour: High C2 High M1 C1=M2=0 (basically: M2=C1=0, M1 > 0, C2 > 0)
if((twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,7] <-  basinsfinal[i,7] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypefive: High C1 High M2 C2>0 M1>0 (basically: C1 > C2, M2 > M1 but C2,M1>0 and C1 > M1, M2 > C2)
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,8] <- basinsfinal[i,8] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

#equitypesix: High C2 High M1 C1>0 M2>0 (basically: C2 > C1, M1 > M2 but M2,C1>0 and C1 < M1, M2 < C2)
if((twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,9] <- basinsfinal[i,9] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

#equitypeseven: high coral M > 0 both patches (basically: C1 > M1, C2 > M2, but M1,M2 > 0)
if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,10] <-  basinsfinal[i,10] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypeeight: high macroalgae C > 0 both patches (basically: M1 > C1, M2 > C2, but C1,C2 > 0)
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,11] <-  basinsfinal[i,11] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypenine: C = M both patches
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,12] <-  basinsfinal[i,12] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

	
} #end of if statement	

basinsfinal[i,13] <- basinsfinal[i,4]+basinsfinal[i,5]+basinsfinal[i,6]+basinsfinal[i,7] + basinsfinal[i,8]+basinsfinal[i,9]+basinsfinal[i,10]+basinsfinal[i,11]+basinsfinal[i,12]

if(i == stopp){
save(basinsfinal, file = paste0("hg_basinsfinal_",stopp,".RData"))
stopp = stopp + 1000
}

} #end of j loop
basinsabr <- 0 #clear it, just in case
} #end of i loop

save(basinsfinal, file = "hg_basinsfinal_done_alllong.RData")

###NOTE: everything after this is redundant or irrelevant, TwoPatchElm_makingfinalgraphs.RData has the relevant parts of the below code

load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/afterdealingwith2856-2900problems/hg_basinsfinal_done_alllong.RData')
sum(basinsfinal[,13]) #2963631 well that is much better (after 2856-2900 added: 2968131)
didntfinish_alllong <- which(round(basinsfinal[,13],4) < 100) #41 afer 2856-2900 done; #2855-2900 issues still (i skipped them so that's fair) and then a smattering of others but not many (only 86 total)

basinsfinal_df <- data.frame(glvl = basinsfinal[,1],grazing = basinsfinal[,2], dispersal = basinsfinal[,3], equi_one = basinsfinal[,4], equi_two = basinsfinal[,5], equi_three = basinsfinal[,6], equi_four = basinsfinal[,7], equi_five = basinsfinal[,8], equi_six = basinsfinal[,9], equi_seven = basinsfinal[,10], equi_eight = basinsfinal[,11], equi_nine = basinsfinal[,12], sum = basinsfinal[,10])
basinsfinal_df[basinsfinal_df$glvl == 0.1 & basinsfinal_df$grazing == 0.29,]

#RAINBOW VERSION (low label)
#equitypeone: high coral zero macroalgae both patches, equitypetwo: high macroalgae zero coral both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0, equitypeseven: high coral M > 0 both patches, equitypeeight: high macroalgae C > 0 both patches, equitypenine: C = M both patches
#reef 1 has the low/med/high grazing
pdf("AllHeatMapsOrganizedbyEqui_heterograz_lowlabel.pdf")
par(mfrow = c(4,3)) #per page in the pdf

#equitypeone
image.plot(t(matrix(basinsfinal[1:9900,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypetwo
image.plot(t(matrix(basinsfinal[1:9900,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighM1M2,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypethree
image.plot(t(matrix(basinsfinal[1:9900,6], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1M2,C2=M1=0,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,6], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,6], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypefour
image.plot(t(matrix(basinsfinal[1:9900,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High C2M1,C1=M2=0,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypefive
image.plot(t(matrix(basinsfinal[1:9900,8], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High C1M2,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,8], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,8], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypesix
image.plot(t(matrix(basinsfinal[1:9900,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High C2M1,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeseven
image.plot(t(matrix(basinsfinal[1:9900,10], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2,M>0,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,10], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,10], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeeight
image.plot(t(matrix(basinsfinal[1:9900,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2,C>0,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypenine
image.plot(t(matrix(basinsfinal[1:9900,12], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "M=C,low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,12], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,12], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

dev.off()

#RAINBOW VERSION (low label, joined equi)
#equitypeone: high coral zero macroalgae both patches, equitypetwo: high macroalgae zero coral both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0, equitypeseven: high coral M > 0 both patches, equitypeeight: high macroalgae C > 0 both patches, equitypenine: C = M both patches
pdf("AllHeatMapsOrganizedbyEqui_heterograz_lowlabel_joinedequi.pdf")
par(mfrow = c(4,3))

#equitypeone
image.plot(t(matrix(basinsfinal[1:9900,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypetwo
image.plot(t(matrix(basinsfinal[1:9900,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypethree + equitypefour
image.plot(t(matrix(basinsfinal[1:9900,6]+basinsfinal[1:9900,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Mismatch nonmixed, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,6]+basinsfinal[9901:19800,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,6]+basinsfinal[19801:29700,7], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypefive + equitypesix
image.plot(t(matrix(basinsfinal[1:9900,8]+basinsfinal[1:9900,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Mismatch mixedlow grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,8]+basinsfinal[9901:19800,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,8]+basinsfinal[19801:29700,9], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeeight
image.plot(t(matrix(basinsfinal[1:9900,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2, C non-zero, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeseven (high C1C2, M nonzero), equitypenine (M=C everywhere) never happen

dev.off()

#RAINBOW VERSION (low label, v joined equi)
#equitypeone: high coral zero macroalgae both patches, equitypetwo: high macroalgae zero coral both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0, equitypeseven: high coral M > 0 both patches, equitypeeight: high macroalgae C > 0 both patches, equitypenine: C = M both patches
pdf("AllHeatMapsOrganizedbyEqui_heterograz_lowlabel_vjoinedequi.pdf")
par(mfrow = c(3,3))

#equitypeone
image.plot(t(matrix(basinsfinal[1:9900,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,4], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypetwo
image.plot(t(matrix(basinsfinal[1:9900,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "High M1M2, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,5], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypethree + equitypefour +5+6+8
image.plot(t(matrix(basinsfinal[1:9900,6]+basinsfinal[1:9900,7] + basinsfinal[1:9900,8]+basinsfinal[1:9900,9] + basinsfinal[1:9900,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Mixed, low grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9901:19800,6]+basinsfinal[9901:19800,7]+basinsfinal[9901:19800,8]+basinsfinal[9901:19800,9] + basinsfinal[9901:19800,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "medium grazing",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19801:29700,6]+basinsfinal[19801:29700,7] + basinsfinal[19801:29700,8]+basinsfinal[19801:29700,9] + basinsfinal[19801:29700,11], nrow = 100, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "high grazing",zlim=c(0,100))

#equitypeseven (high C1C2, M nonzero), equitypenine (M=C everywhere) never happen

dev.off()

#everything below seems due to me not giving enough equi IDs (only 100 when there's 301)
sum(basinsfinal[,13]) #2217393 vs 2220010 before; out of 2970000 
didntfinish_long <- which(round(basinsfinal[,13],4) < 100) #this includes 2855-2900 that didn't work
length(didntfinish_long) #11096 vs 11344 <- so it got shorter but not by much, i guess more didnt finish by a larger amount than before (as 2217393 is smaller than 2220010)
#almost all of the numbers b/w 6826-7797, lots off long sequences that didnt finish
basinsfinal[didntfinish_long[100:120],] #many of the sums are super small
#load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggrazlong_abr/basins_recr0.62g0.69_glvl0.1_20000.RData")) #lines up with the number in basinsfinal, no NAs though which is odd
#load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggraz_abr/basins_recr0.62g0.69_glvl0.1_5850.RData"))
#same size
#which(!(didntfinish %in% didntfinish_long)) #in didntfinish, not in didntfinish_long <- quite a few
which(!(didntfinish_long %in% didntfinish)) #in didntfinish_long, not in didntfinish; just a handful
basinsfinal[which(!(didntfinish_long %in% didntfinish)),] #all are actually at a sum of 100, except one (glvl = 0.1, gval = 0.81, recr = 0.05)
didntfinish_longrpt <- which(round(basinsfinal[,13],0) < 100)
length(didntfinish_longrpt) #11006
length(which(round(basinsfinal[,13],0) < 100 & round(basinsfinal[,13],0) > 90)) #1195
length(which(round(basinsfinal[,13],0) < 10)) #4836
basinsfinal[didntfinish_longrpt[1000:1020],]
load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggrazlong_abr/basins_recr0.01g0.79_glvl0.1_20000.RData"))
#i checked - the error file on computecanada gave no errors

#plotting mumbytraj to check what's going on
#load in equi
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_ID_full.RData')
twopatch_ext_complete <- twopatch_ext_heterograz_ID_full #range(ID) = 302, good

setwd("~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/Trajectory Plots")

#load in mumbytraj of choice
mumbytrajectories <- NULL
#folder based on dispersal level
load('/Volumes/BackupPlus/mumbytraj_heteroggrazlong_full/mumbytraj_heteroggrazlong17/mumbytrajectories_recr0.17g0.55_glvl0.1_20000.RData')
g_val = 0.55
recruitvalue = 0.17
glvl = 0.1

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
png(paste0("TrajectoryPlot_Grazing", g_val,"Dispersal",recruitvalue,"heterograz_glvl",glvl,".png"))
par(mfrow = c(2,2))
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "M1 cover", ylab = "C1 cover", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "M1 cover", ylab = "M2 cover", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "C1 cover", ylab = "C2 cover", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl],  xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "M2 cover", ylab = "C2 cover", main = paste("Dispersal = ", recruitvalue, ", grazing = ", g_val, "glvl",glvl))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl], pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl],  xlim = c(0,1), ylim = c(0,1))

dev.off()



#OLD
basinsabr <- 0

stopp = 500
#i = 2855-2900 doesn't exist (no basinsabr) #glvl = 0.1,grazing=0.29 bscly (ran some of these again on computecanada and still had problems)
#basinsfinal[2900:3000,]
#for(i in 2901:length(summ)){
for(i in 1:2854){
print(paste("i =",i))
glvl <- basinsfinal[i,1]		
g_val <- basinsfinal[i,2]	
recruitvalue <- recruit <- round(basinsfinal[i,3],4)	
load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggraz_abr/basins_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_5850.RData"))

for(j in 1:100){ #bc set to 100 diff possible ID values
print(paste("j =",j))
if(basinsabr$Size[basinsabr$EquilibriumID == j] > 0){

#equitypeone = high coral zero macroalgae both patches - M1=M2=0, C1 >0, C2 > 0
if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,4] <-  basinsfinal[i,4] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypetwo = high macroalgae zero coral both patches - C1=C2=0, M1 >0, M2 > 0
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,5] <-  basinsfinal[i,5] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypethree: High C1 High M2 C2=M1=0 (basically: C2=M1=0, C1 > 0, M2 > 0)
if((twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,6] <-  basinsfinal[i,6] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypefour: High C2 High M1 C1=M2=0 (basically: M2=C1=0, M1 > 0, C2 > 0)
if((twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,7] <-  basinsfinal[i,7] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypefive: High C1 High M2 C2>0 M1>0 (basically: C1 > C2, M2 > M1 but C2,M1>0)
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,8] <- basinsfinal[i,8] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

#equitypesix: High C2 High M1 C1>0 M2>0 (basically: C2 > C1, M1 > M2 but M2,C1>0)
if((twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,9] <- basinsfinal[i,9] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

#equitypeseven: high coral M > 0 both patches (basically: C1 > M1, C2 > M2, but M1,M2 > 0)
if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,10] <-  basinsfinal[i,10] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypeeight: high macroalgae C > 0 both patches (basically: M1 > C1, M2 > C2, but C1,C2 > 0)
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,11] <-  basinsfinal[i,11] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

#equitypenine: C = M both patches
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
basinsfinal[i,12] <-  basinsfinal[i,12] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

}

#back when equitypeone was C1=C2>0 and M1=M2=0	
#if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
#basinsfinal[i,4] <- (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 #only one of these equi per paramcombo
#}

#back when equitypetwo was M1=M2>0 and C1=C2=0
#if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
#basinsfinal[i,5] <- (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 #only one of these equi per paramcombo
#}
	
}	

basinsfinal[i,13] <- basinsfinal[i,4]+basinsfinal[i,5]+basinsfinal[i,6]+basinsfinal[i,7] + basinsfinal[i,8]+basinsfinal[i,9]+basinsfinal[i,10]+basinsfinal[i,11]+basinsfinal[i,12]

if(i == stopp){
save(basinsfinal, file = paste0("hg_basinsfinal_",stopp,".RData"))
stopp = stopp + 1000
}
basinsabr <- 0 #clear it, just in case
}
#basinsfinal[c(1:2854),c(4:12)] <- 0
save(basinsfinal, file = "hg_basinsfinal_done.RData")
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/hg_basinsfinal_done.RData')
basinsfinal[c(2854:2901),] #glvl = 0.1, g = 0.29, disp = 0.54-0.99
basinsfinal[5000,] 
sum(basinsfinal[,13]) #2220010 out of 2970000 
basinsfinal[which(round(basinsfinal[,13],4) < 100),] #there are so many, like >7000 
didntfinish <- which(round(basinsfinal[,13],4) < 100) #this includes 2855-2900 that didn't work, since that's so few and to avoid doing this again may as well just leave them in
save(didntfinish,file="didntfinish_hg.RData")
#didntfinish <- didntfinish[-c(2:47)] #remove 2855-2900 as those just didn't work at all
iteratorhg_didntfinish <- basinsfinal[didntfinish,c(1:3)]
save(iteratorhg_didntfinish, file = "iteratorhg_didntfinish.RData")
iteratorhg_didntfinish[90:99,]
#load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggraz_abr/basins_recr0.23g0.08_glvl0.3_5850.RData"))
#^ this one has literally 0 trajectories that have finished, there are many that seem to be in the same boat...i wonder why, it seems kind of random and i guess the easiest way to attempt to fix is to up the traj time?
#load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggraz_abr/basins_recr0.07g0.44_glvl0.1_5850.RData"))
#this one is 209/210 and has 1 NA and basinsfinal[didntfinish[90],] aligns with that (sum = 99.52 = 209/210)

#checking whether i was missing any cases (the ones that don't add up to 100 - missing cases or didnt finish)
comprsn <- rep(NA, length(didntfinish))
for(i in 48:length(didntfinish)){ #2-47 dont have basinsabr files
glvl <- basinsfinal[didntfinish[i],1]		
g_val <- basinsfinal[didntfinish[i],2]	
recruitvalue <- recruit <- round(basinsfinal[didntfinish[i],3],4)	
load(paste0("~/Documents/computecanadajune2019/HeterogeneousGrazing12.2019run/bsns_heteroggraz_abr/basins_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_5850.RData"))
comprsn[i] <- round(basinsfinal[didntfinish[i],13],4) - round((sum(basinsabr$Size)/210)*100, 4)
basinsabr <- 0
}
sum(comprsn[48:length(didntfinish)]) #16.667
comprsn[which(comprsn > 0)] #16.667 at i = 181 (19.5 in basinsabr (equi77,78) vs 36.2 in basinsfinal)
i <- 181
basinsfinal[didntfinish[i],] #equi 2 (%age corresponds with ID 77),6,8 (%age corresponds with ID 78)
#basically: no cases where an equi wasn't included in one of the categories...at least within the ones that didnt sum to 100 ()