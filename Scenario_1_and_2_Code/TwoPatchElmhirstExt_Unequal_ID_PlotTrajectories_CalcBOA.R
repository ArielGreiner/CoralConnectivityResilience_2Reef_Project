library(scales)
library(deSolve)
library(fields)


#load + compile data
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete_IDs.RData')
twopatch_ext_unequal_complete_IDs$colour <- NULL


#need to add twopatch_ext_complete_IDs in
load('~/Documents/computecanadajune2019/twopatch_ext_complete_IDs.RData') #probably should replace this with a link to something in dropbox
twopatch_ext_complete_IDs$symbol <- NULL
twopatch_ext_complete_IDs$percentofcoral <- 100

twopatch_ext_complete <- rbind(twopatch_ext_complete_IDs,twopatch_ext_unequal_complete_IDs)

#to get rid of floating point errors
twopatch_ext_complete$percentofcoral <- round(twopatch_ext_complete$percentofcoral, digits = 0)
#levels(as.factor(twopatch_ext_complete$p_m)) 4 decimal points is the longest
twopatch_ext_complete$p_m <- round(twopatch_ext_complete$p_m,4)
twopatch_ext_complete$q_m <- round(twopatch_ext_complete$q_m,4)


#basinofattractionID <- might not do anything with this bc no longer plotting BOAs but if ever do, can use these
#should put all of the basinsabr dataframes into one big dataframe...or matrix to make looping easier
lvls <- c(rep(1,99*99),rep(0.05,99*99),rep(0.1,99*99),rep(0.25,99*99),rep(0.5,99*99))
grazing <- c(rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99))
recruit <-c(rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99))
sum <- rep(NA,length(lvls)) #wasnt here when i ran it...
equitypeone <- equitypetwo <- equitypethree <- equitypefour <- equitypefive <- equitypesix <- rep(0,length(lvls))
exist <- rep(-1,length(lvls)) #-1 if still need to load in basins file, 0 if don't
basinsfinal <-  matrix(c(lvls,grazing,recruit,equitypeone,equitypetwo,equitypethree,equitypefour,equitypefive,equitypesix,sum),nrow=length(lvls),ncol=10)

#equitypeone: high equal coral both patches, equitypetwo: high equal macroalgae both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0
basinsabr <- 0
#errors with: i = 1-12 don't seem to exist, did 20-496, re-started at 7999 (double check this one) and then again at 49005 (double check also)..oh nvmnd it's done..still double check 7999 - it looks fine #UPDATE: added in 1-12
stopp = 500
load('~/Documents/computecanadajune2019/didntfinish_trajectoriestorecalc.RData') 
head(didntfinish)

for(i in 1:length(lvls)){
print(paste("i =",i))
lvl <- basinsfinal[i,1]		
g_val <- basinsfinal[i,2]	
recruitvalue <- recruit <- basinsfinal[i,3]	
#load in relevant basinsabr file...apparently not all of them exist...
load(paste0("~/Documents/computecanadajune2019/sept2019run/bsns_better_abr/basins_recr",recruitvalue,"g",g_val,"_",lvl*100,"percentofcoral_5850.RData"))
if((lvl*1000000 + g_val*1000 + recruitvalue*1) %in% (didntfinish[,1]*1000000 + didntfinish[,2]*1000 + didntfinish[,3]*1)){
	basinsabr <- 0 #clear it, just in case
	load(paste0("~/Documents/computecanadajune2019/extralongtraj_SMALL/bsns_better_abr2/basins_recr",recruitvalue,"g",g_val,"_",lvl*100,"percentofcoral_10000.RData"))
}


for(j in 1:100){ #bc set to 100 diff possible ID values
print(paste("j =",j))
if(basinsabr$Size[basinsabr$EquilibriumID == j] > 0){
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,4] <- (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 #only one of these equi per paramcombo
}

if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,5] <- (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 #only one of these equi per paramcombo
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == 0)){
basinsfinal[i,6] <- basinsfinal[i,6] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] < twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4)& twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == 0)){
basinsfinal[i,7] <- basinsfinal[i,7] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,8] <- basinsfinal[i,8] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] < twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4)& twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinal[i,9] <- basinsfinal[i,9] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}
	
}}	

basinsfinal[i,10] <- basinsfinal[i,4]+basinsfinal[i,5]+basinsfinal[i,6]+basinsfinal[i,7] + basinsfinal[i,8]+basinsfinal[i,9]

if(i == stopp){
save(basinsfinal, file = paste0("basinsfinal_",stopp,".RData"))
stopp = stopp + 1000
}
basinsabr <- 0 #clear it, just in case
}
#check: 
#sum(rowSums(basinsfinal[,4:7]) == 100) #47299 so not all of them = 100
#troule <- c(1:49005)
#troule[which(rowSums(basinsfinal[,4:7]) < 100)]


save(basinsfinal,file="basinsfinal_done.RData")
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/basinsfinal_done_6equi_longtrajincl.RData')
sum(basinsfinal[,10] == 100) #47308/49005 work....ugh
sum(basinsfinal[,10]) #4900412 vs 4900500
basinsfinal[which(round(basinsfinal[,10],4) < 100),] #okay there's only 8 conditions that didn't finish, 4 of them got to 99.5% and the others are 81.4,72.4,85.7,74.8 which isn't so bad...no new equi and they all were in didntfinish and all went up incrementally so i kind of feel like it's not worth running them again for even longer


#old - before added extralong traj
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/basinsfinal_done_6equi.rdata')
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/basinsfinal_done_6equi_redo.rdata') #added 'sum' in better, everything directly below here used the non-redo version 
sum(basinsfinal[,10] == 100) #47234/49005 work
sum(basinsfinal[,10]) #4900254 vs 4900500 = (100*49005) bc many of the ones less than 100 are >99
sum((basinsfinal[,10]*210)/100) #10290533 trajectories finished out of a total of 10,291,050 (0.005% didn't finish)
didntfinish <- basinsfinal[which(round(basinsfinal[,10],4) < 100),]
dim(didntfinish) #121 rows
sum(didntfinish[,10]) #4900254+(121*100 - 11853.81) = 4900500 so that's all of them :)
hist(didntfinish[,3]) #1/2 from lvl = 1, most from g=0.4, even spread re: dispersal 
save(didntfinish,file="didntfinish_trajectoriestorecalc.RData") 
#troule <- c(1:49005)
#troule[basinsfinal[,10] < 100]
basinsfinal[2000:4000,10] #some here that aren't =100
basinsfinal[2080,] #NAs also
basinsfinal[3367,] #this one has NAs in the respective basinsabr file, 73% total
basinsfinal[3960,] #1 NA
#^ issue with NAs due to other code, not issue with forloop above..also sometimes this happens when oonly one of first twoo equi are present so unlikely to be disrupting things too much...
load(paste0("~/Documents/computecanadajune2019/sept2019run/BoAID_better/basinofattractionID_recr",recruitvalue,"g",g_val,"_",lvl*100,"percentofcoral_5850.RData"))
head(basinofattractionID) #yup a bunch of the initial conditions don't finish

#generate a basinsfinal file for zero dispersal level
#load + compile data
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/twopatch_ext_zero_ID_full.RData')
twopatch_ext_zero_ID_full$colour <- NULL
#twopatch_ext_zero_ID_full$percentofcoral <- 100

twopatch_ext_complete <- twopatch_ext_zero_ID_full



#basinofattractionID <- might not do anything with this bc no longer plotting BOAs but if ever do, can use these
#should put all of the basinsabr dataframes into one big dataframe...or matrix to make looping easier
lvls <- rep(1,99)
grazing <- seq(0.01,0.99,0.01)
recruit <- rep(0, 99)
equitypeone <- equitypetwo <- equitypethree <- equitypefour <- equitypefive <- equitypesix <- rep(0,length(lvls))
exist <- rep(-1,length(lvls)) #-1 if still need to load in basins file, 0 if don't
sum <- rep(NA,length(lvls))
basinsfinalzero <-  matrix(c(lvls,grazing,recruit,equitypeone,equitypetwo,equitypethree,equitypefour,equitypefive,equitypesix,sum),nrow=length(lvls),ncol=10)

#equitypeone: high equal coral both patches, equitypetwo: high equal macroalgae both patches, equitypethree: High C1 High M2 C2=M1=0, equitypefour: High C2 High M1 C1=M2=0, equitypefive: High C1 High M2 C2>0 M1>0, equitypesix: High C2 High M1 C1>0 M2>0
basinsabr <- 0
stopp = 500
for(i in 1:length(lvls)){
print(paste("i =",i))
lvl <- basinsfinalzero[i,1]		
g_val <- basinsfinalzero[i,2]	
recruitvalue <- recruit <- basinsfinalzero[i,3]	
#load in relevant basinsabr file...apparently not all of them exist...
load(paste0("~/Documents/computecanadajune2019/ZeroDispersal_12.31.2019run/bsns_zero_abr/basins_recr",recruitvalue,"g",g_val,"_",lvl*100,"percentofcoral_10000.RData"))

for(j in 1:100){ #bc set to 100 diff possible ID values
print(paste("j =",j))
if(basinsabr$Size[basinsabr$EquilibriumID == j] > 0){
if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinalzero[i,4] <- basinsfinalzero[i,4] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 
}

if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinalzero[i,5] <- basinsfinalzero[i,5] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100 #only one of these equi per paramcombo
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == 0)){
basinsfinalzero[i,6] <- basinsfinalzero[i,6] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] < twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4)& twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == 0)){
basinsfinalzero[i,7] <- basinsfinalzero[i,7] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinalzero[i,8] <- basinsfinalzero[i,8] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}

if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] < twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4)& twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
basinsfinalzero[i,9] <- basinsfinalzero[i,9] + (basinsabr$Size[basinsabr$EquilibriumID == j]/210)*100
}
	
}}	

basinsfinalzero[i,10] <- basinsfinalzero[i,4]+basinsfinalzero[i,5]+basinsfinalzero[i,6]+basinsfinalzero[i,7] + basinsfinalzero[i,8]+basinsfinalzero[i,9]

if(i == stopp){
save(basinsfinalzero, file = paste0("basinsfinalzero_",stopp,".RData"))
stopp = stopp + 1000
}
basinsabr <- 0 #clear it, just in case
}
#check: 
#sum(rowSums(basinsfinal[,4:7]) == 100) #47299 so not all of them = 100
#troule <- c(1:49005)
#troule[which(rowSums(basinsfinal[,4:7]) < 100)]


save(basinsfinalzero,file="basinsfinalzero_done.RData")
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/basinsfinalzero_done.RData')

sum(basinsfinalzero[,10] == 100) #90/99 work
sum(basinsfinalzero[,10]) #9895.238 vs 9900 
sum((basinsfinalzero[,10]*210)/100) #20780 trajectories finished out of a total of 20,790 (0.05% didn't finish)
which(round(basinsfinalzero[,10],4) < 100) #40, so just 1 didnt totally finish - i can live with that
#load(paste0("~/Documents/computecanadajune2019/ZeroDispersal_12.31.2019run/bsns_zero_abr/basins_recr0g0.4_100percentofcoral_10000.RData"))
#yeah only 200/210 finished so im not missing any equi possibilities or anything
########

#Plot Trajectories + Equi Points
#load equi points (SHOULD PROBABLY REDO THE EQUAL ONES TOO)
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete_IDs.RData')
twopatch_ext_complete <- twopatch_ext_unequal_complete_IDs #dont use for plotting BoAs

#for the really small dispersal ones
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Small Coral Dispersal_full/dataframes with all values_1e-80smallest/twopatch_ext_equal_ID_full.RData')
twopatch_ext_complete <-twopatch_ext_equal_ID_full

#would ideally give every ID a unique symbol but there's 95 of them...need to think about this, going to stick with the equi version for now bc that's more reliable
range(twopatch_ext_complete$Equilibrium) #1 to 10 
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

#loop through iterator to avoid the use of 3 nested for loops
lvls <- c(rep(1,99*99),rep(0.05,99*99),rep(0.1,99*99),rep(0.25,99*99),rep(0.5,99*99))
grazing <- c(rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99),rep(seq(0.01,0.99,0.01), each = 99))
recruit <-c(rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99),rep(seq(0.01,0.99,0.01), 99))
iterator <- matrix(c(lvls,grazing,recruit),nrow=length(lvls),ncol=3)

#for loop - looping through the mumbytrajectories folder

#load in lvl, recruitvalue,g_val from iterator

#load mumbytrajectories file
#load('~/Documents/computecanadajune2019/mumbytrajectories_recr0.15g0.26_5percentofcoral.RData')
#lvl = 0.05
#recruitvalue = 0.15
#g_val = 0.26

#load('/Volumes/Ariel PhD Stuff/smallrun_full_better/mumbytraj_better/mumbytrajectories_recr0.1g0.27_5percentofcoral.RData')
#lvl = 0.05
#recruitvalue = 0.1
#g_val = 0.27

#load('/Volumes/Ariel PhD Stuff/smallrun_full_better/mumbytraj_better/mumbytrajectories_recr0.1g0.27_10percentofcoral.RData')
#lvl = 0.10
#recruitvalue = 0.1
#g_val = 0.27

#load('/Volumes/Ariel PhD Stuff/smallrun_full_better/mumbytraj_better/mumbytrajectories_recr0.1g0.27_25percentofcoral.RData')
#lvl = 0.25
#recruitvalue = 0.1
#g_val = 0.27

#load('/Volumes/Ariel PhD Stuff/smallrun_full_better/mumbytraj_better/mumbytrajectories_recr0.1g0.27_50percentofcoral.RData')
#lvl = 0.50
#recruitvalue = 0.1
#g_val = 0.27

load('~/Documents/computecanadajune2019/mumbytrajectories_recr1e-20g0.3_100percentofcoral.RData')
lvl = 1
recruitvalue = 1e-20
g_val = 0.3

bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#bluefunc <- colorRampPalette(c("lightblue", "red")) #looked sort of misleading
bluefunc_ten <- bluefunc(11)
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1,500))] <- bluefunc_ten[1]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(501,1000))] <- bluefunc_ten[2]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1001,1500))] <- bluefunc_ten[3]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1501,2000))] <- bluefunc_ten[4]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2001,2500))] <- bluefunc_ten[5]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(2501,3000))] <- bluefunc_ten[6]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(3001,3500))] <- bluefunc_ten[7]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(3501,4000))] <- bluefunc_ten[8]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4001,4500))] <- bluefunc_ten[9]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(4501,5001))] <- bluefunc_ten[10]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(5001,5850))] <- bluefunc_ten[11]

rounddig <- 90 #needs to be like this for twopatch_ext_unequal_complete_IDs but for the _full_ ones need it to be 90

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
#pdf(paste0("TrajectoryPlot_Grazing", g_val,"Recruitment",recruitvalue,"percentofcoral",lvl*100,".pdf"))
png(paste0("TrajectoryPlot_Grazing", g_val,"Recruitment",recruitvalue,"percentofcoral",lvl*100,".png"))
par(mfrow = c(2,2))
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "M1 cover", ylab = "C1 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val, "percentofcoral",lvl*100))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val])

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "M1 cover", ylab = "M2 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val, "percentofcoral",lvl*100))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val])

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "C1 cover", ylab = "C2 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val, "percentofcoral",lvl*100))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val])

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "M2 cover", ylab = "C2 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val, "percentofcoral",lvl*100))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,rounddig) & twopatch_ext_complete$g == g_val])

dev.off()







#Plot change in BoA size?
#plot a heatmap of basin sizes for all percentcoral levels and all 4 equi types
#basinsfinal
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/basinsfinal_done_6equi_longtrajincl.RData')
head(basinsfinal)
#basinsfinalzero
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/basinsfinalzero_done.RData')
head(basinsfinalzero)

#need to put the basinsfinal and basinsfinalzero dataframes together
equal_basinsfinalzero <- basinsfinalzero
five_basinsfinalzero <- ten_basinsfinalzero <- twentyfive_basinsfinalzero <- fifty_basinsfinalzero <- basinsfinalzero #want a zero in every graph
equal_basinsfinalzero[,1] <- 1
five_basinsfinalzero[,1] <- 0.05
ten_basinsfinalzero[,1] <- 0.10
twentyfive_basinsfinalzero[,1] <- 0.25
fifty_basinsfinalzero[,1] <- 0.50

basinsfinalzero_all <- rbind(equal_basinsfinalzero,five_basinsfinalzero,ten_basinsfinalzero,twentyfive_basinsfinalzero,fifty_basinsfinalzero)


#make empty matrix where every different 'g' and diff dispersal inequality level have a zero disp case
lvls <- c(rep(1,100*99),rep(0.05,100*99),rep(0.1,100*99),rep(0.25,100*99),rep(0.5,100*99))
grazing <- c(rep(seq(0.01,0.99,0.01), each = 100),rep(seq(0.01,0.99,0.01), each = 100),rep(seq(0.01,0.99,0.01), each = 100),rep(seq(0.01,0.99,0.01), each = 100),rep(seq(0.01,0.99,0.01), each = 100))
recruit <-c(rep(seq(0,0.99,0.01), 99),rep(seq(0,0.99,0.01), 99),rep(seq(0,0.99,0.01), 99),rep(seq(0,0.99,0.01), 99),rep(seq(0,0.99,0.01), 99))
sum <- rep(NA,length(lvls)) #wasnt here when i ran it...
equitypeone <- equitypetwo <- equitypethree <- equitypefour <- equitypefive <- equitypesix <- rep(0,length(lvls))
exist <- rep(-1,length(lvls)) #-1 if still need to load in basins file, 0 if don't
basinsfinalboth <-  matrix(c(lvls,grazing,recruit,equitypeone,equitypetwo,equitypethree,equitypefour,equitypefive,equitypesix,sum),nrow=length(lvls),ncol=10)

for(i in 1:(dim(basinsfinalzero_all)[1])){
basinsfinalboth[(1+((i-1)*100)),] <- basinsfinalzero_all[i,]	
basinsfinalboth[(1+((i-1)*100)+1):(1+((i-1)*100)+99),] <- basinsfinal[(((i-1)*99)+1):((i)*99),]
}
save(basinsfinalboth,file="basinsfinalboth.RData")
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/with zero disp included/basinsfinalboth.RData')

#these don't work bc need to intersperse the rows, see above
#basinsfinalboth <- rbind(basinsfinalzero, basinsfinal) #NEED TO FEED IT IN IN A FANCIER WAY
#basinsfinalboth <- rbind(basinsfinalzero,five_basinsfinalzero,ten_basinsfinalzero,twentyfive_basinsfinalzero,fifty_basinsfinalzero, basinsfinal)



basinsfinalabr <- basinsfinalboth[basinsfinalboth[,3] <= 0.45,]  #bc Marty wanted to make the heatmaps ignoring the coral dispersal values greater than 0.4 bc nothing happens above that

#RAINBOW VERSION (abridged, mixed+pure mismatch)
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lessthan45_wzero_allmismatch.pdf")
par(mfrow = c(3,5))

#double high coral
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100))

#double high macroalgae
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighM1M2, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

#all mismatch
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,6] + basinsfinalabr[basinsfinalabr[,1] == 0.05,7]+basinsfinalabr[basinsfinalabr[,1] == 0.05,8]+basinsfinalabr[basinsfinalabr[,1] == 0.05,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "All mismatch, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,6] + basinsfinalabr[basinsfinalabr[,1] == 0.10,7]+basinsfinalabr[basinsfinalabr[,1] == 0.10,8]+basinsfinalabr[basinsfinalabr[,1] == 0.10,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,6] + basinsfinalabr[basinsfinalabr[,1] == 0.25,7]+basinsfinalabr[basinsfinalabr[,1] == 0.25,8]+basinsfinalabr[basinsfinalabr[,1] == 0.25,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,6] + basinsfinalabr[basinsfinalabr[,1] == 0.50,7]+basinsfinalabr[basinsfinalabr[,1] == 0.50,8]+basinsfinalabr[basinsfinalabr[,1] == 0.50,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,6]+basinsfinalabr[basinsfinalabr[,1] == 1,7]+basinsfinalabr[basinsfinalabr[,1] == 1,8]+basinsfinalabr[basinsfinalabr[,1] == 1,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

dev.off()


#RAINBOW VERSION (abridged)  
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lessthan45_wzero.pdf")
par(mfrow = c(4,5))

#double high coral
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighC1C2, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,4], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100))

#double high macroalgae
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "HighM1M2, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,5], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

#pure mismatch
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,6] + basinsfinalabr[basinsfinalabr[,1] == 0.05,7], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Pure mismatch, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,6] + basinsfinalabr[basinsfinalabr[,1] == 0.10,7], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,6] + basinsfinalabr[basinsfinalabr[,1] == 0.25,7], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,6] + basinsfinalabr[basinsfinalabr[,1] == 0.50,7], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,6]+basinsfinalabr[basinsfinalabr[,1] == 1,7], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

#mixed mismatch
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,8] + basinsfinalabr[basinsfinalabr[,1] == 0.05,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "mixed mismatch, 5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,8] + basinsfinalabr[basinsfinalabr[,1] == 0.10,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,8] + basinsfinalabr[basinsfinalabr[,1] == 0.25,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,8] + basinsfinalabr[basinsfinalabr[,1] == 0.50,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,8]+basinsfinalabr[basinsfinalabr[,1] == 1,9], nrow = 46, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

dev.off()


#no zero dispersal included below here

#use basinsfinal - make into a dataframe
# matrix(c(lvls,grazing,recruit,equitypeone,equitypetwo,equitypethree,equitypefour,exist),nrow=length(lvls),ncol=8)
#basinsfinal_df <- data.frame(lvl = basinsfinal[,1],grazing = basinsfinal[,2], recruit = basinsfinal[,3], equi_one = basinsfinal[,4], equi_two = basinsfinal[,5], equi_three = basinsfinal[,6], equi_four = basinsfinal[,7], equi_five = basinsfinal[,8], equi_six = basinsfinal[,9], sum = basinsfinal[,10])

basinsfinalabr <- basinsfinal[basinsfinal[,3] <= 0.45,]  #bc Marty wanted to make the heatmaps ignoring the coral dispersal values greater than 0.4 bc nothing happens above that

#RAINBOW VERSION (low label, abridged)  
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lowlabel_lessthan45.pdf")
par(mfrow = c(3,5))

#double high coral
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,4], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,4], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,4], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,4], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,4], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100))

#double high macroalgae
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,5], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,5], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,5], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,5], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,5], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

#mixed mismatch
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.05,8] + basinsfinalabr[basinsfinalabr[,1] == 0.05,9], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%",zlim=c(0,100), ylim = c(0,0.45))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.10,8] + basinsfinalabr[basinsfinalabr[,1] == 0.10,9], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.25,8] + basinsfinalabr[basinsfinalabr[,1] == 0.25,9], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 0.50,8] + basinsfinalabr[basinsfinalabr[,1] == 0.50,9], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinalabr[basinsfinalabr[,1] == 1,8]+basinsfinalabr[basinsfinalabr[,1] == 1,9], nrow = 45, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 

dev.off()

#RAINBOW VERSION (low label)
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_reordered_lowlabel.pdf")
par(mfrow = c(3,5))

#double high coral
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100))

#double high macroalgae
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100))  

#mixed mismatch
image.plot(t(matrix(basinsfinal[9802:19602,8]+basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8]+basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "10%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8]+basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "25%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8]+basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "50%",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,8]+basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", yaxt="n", main = "Equal",zlim=c(0,100)) 
dev.off()

#PRO GREYSCALE VERSION - NEED TO PUT THE TITLES BACK
#pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_grey_ydispersal_proversion.pdf")
png("AllHeatMapsOrganizedbyEqui_moreabr6equi_grey_ydispersal_proversion.png")
par(mfrow = c(3,5), mar = c(1,1,1.5,1), oma = c(1,3,1,2)) #bottom,left,top,right
image(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", main = "10%, C1C2 high",zlim=c(0,100))
image(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", main = "25%, C1C2 high",zlim=c(0,100))
image(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", main = "50%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", main = "Equal, C1C2 high",zlim=c(0,100))

image(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal",zlim=c(0,100)) 
image(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n",zlim=c(0,100)) 
image(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n",zlim=c(0,100)) 
image(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt = "n", zlim=c(0,100)) 

image(t(matrix(basinsfinal[9802:19602,8]+basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", zlim=c(0,100)) 
image(t(matrix(basinsfinal[19603:29403,8]+basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", zlim=c(0,100)) 
image(t(matrix(basinsfinal[29404:39204,8]+basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", zlim=c(0,100)) 
image(t(matrix(basinsfinal[39205:49005,8]+basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt="n", zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,8]+basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", yaxt = "n", zlim=c(0,100)) 
dev.off()

#BASIC GREYSCALE VERSION
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi_gray.pdf")
#png("AllHeatMapsOrganizedbyEqui_moreabr6equi_gray.png")
par(mfrow = c(3,5))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))

image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[9802:19602,8]+basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "5%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8]+basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "10%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8]+basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "25%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8]+basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "50%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,8]+basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("white", rev(gray.colors(300))), xlab = "grazing", ylab = "dispersal", main = "Equal, mixed mismatch",zlim=c(0,100)) 

dev.off()
######## old ones below

#heat maps organized by equilibrium

pdf("AllHeatMapsOrganizedbyEqui_6equiversion.pdf") #figure margins too large
par(mfrow = c(6,5))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))

image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high M1 = 0",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high M2 = 0",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high M1 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high M1 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high M1 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high M1 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high M1 > 0",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high M2 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high M2 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high M2 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high M2 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high M2 > 0",zlim=c(0,100)) 
dev.off()


pdf("HeatMapsforDoubleHighCoralEqui.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))
dev.off()

pdf("HeatMapsforDoubleHighMacroaglaeEqui.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforHighC1EquiM1zero.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high M1 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high M1 = 0",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforHighC2EquiM2zero.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high M2 = 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high M2 = 0",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforHighC1EquiM1nonzero.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high M1 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high M1 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high M1 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high M1 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high M1 > 0",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforHighC2EquiM2nonzero.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high M2 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high M2 >0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high M2 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high M2 > 0",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high M2 > 0",zlim=c(0,100)) 
dev.off()

#USE THIS ONE
pdf("AllHeatMapsOrganizedbyEqui_moreabr6equi.pdf")
par(mfrow = c(3,5))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))

image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100))  

image.plot(t(matrix(basinsfinal[9802:19602,8]+basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8]+basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8]+basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8]+basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,8]+basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, mixed mismatch",zlim=c(0,100)) 
dev.off()


pdf("AllHeatMapsOrganizedbyEqui_abr6equi.pdf")
par(mfrow = c(4,5))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))

image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,6]+basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6] + basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6]+basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6]+basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6]+basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, mismatch",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,8]+basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,8]+basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8]+basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8]+basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8]+basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, mixed mismatch",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforMismatchEqui.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,6]+basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6] + basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6]+basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6]+basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6]+basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, mismatch",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforMixedMismatchEqui.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,8]+basinsfinal[1:9801,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,8]+basinsfinal[9802:19602,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,8]+basinsfinal[19603:29403,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,8]+basinsfinal[29404:39204,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, mixed mismatch",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,8]+basinsfinal[39205:49005,9], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, mixed mismatch",zlim=c(0,100)) 
dev.off()



#old versions with basinsfinal with 4 equi
head(basinsfinal_df[basinsfinal_df$lvl == 0.5,])
#basinsfinal[9802:19602,] - 5% dispersal zone
#heatmap of equi 1 at 5% dispersal
#heatmap(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99), xlab = "grazing", ylab = "dispersal") #not sure about the x and y here
pdf("HeatMapsforEqualandUnequalDispersalCases.pdf")
par(mfrow = c(5,4))

image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high",zlim=c(0,100)) 

dev.off()

#heat maps organized by case
pdf("HeatMapsfor5PercentDispersalCases.pdf")
par(mfrow = c(1,4))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsfor10PercentDispersalCases.pdf")
par(mfrow = c(1,4))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsfor25PercentDispersalCases.pdf")
par(mfrow = c(1,4))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsfor50PercentDispersalCases.pdf")
par(mfrow = c(1,4))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsfor100PercentDispersalCases.pdf")
par(mfrow = c(1,4))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high",zlim=c(0,100)) 
dev.off()

#heat maps organized by equilibrium

pdf("AllHeatMapsOrganizedbyEqui.pdf")
par(mfrow = c(4,5))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))

image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high",zlim=c(0,100)) 

image.plot(t(matrix(basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high",zlim=c(0,100)) 
dev.off()


pdf("HeatMapsforDoubleHighCoralEqui.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[9802:19602,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[19603:29403,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[29404:39204,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1C2 high",zlim=c(0,100))
image.plot(t(matrix(basinsfinal[39205:49005,4], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1C2 high",zlim=c(0,100))
dev.off()

pdf("HeatMapsforDoubleHighMacroaglaeEqui.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, M1M2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,5], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, M1M2 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforHighC1Equi.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C1 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,6], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C1 high",zlim=c(0,100)) 
dev.off()

pdf("HeatMapsforHighC2Equi.pdf")
par(mfrow = c(1,5))
image.plot(t(matrix(basinsfinal[1:9801,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "Equal, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[9802:19602,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "5%, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[19603:29403,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "10%, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[29404:39204,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "25%, C2 high",zlim=c(0,100)) 
image.plot(t(matrix(basinsfinal[39205:49005,7], nrow = 99, ncol = 99)), col = c("gray", tim.colors(300)), xlab = "grazing", ylab = "dispersal", main = "50%, C2 high",zlim=c(0,100)) 
dev.off()

