library(scales)
library(deSolve)
library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(fields)

##EQUAL DISPERSAL GRAPHING
#load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/complete_run/Equi tracking/twopatch_ext_complete_IDs.RData')
#head(twopatch_ext_complete_IDs)

#twopatch_ext_complete <- twopatch_ext_complete_IDs

##ZERO DISPERSAL GRAPHING

#load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/twopatch_ext_zero_ID_full.RData')
#head(twopatch_ext_zero_ID_full)
#names(twopatch_ext_zero_ID_full)
#twopatch_ext_complete <- twopatch_ext_zero_ID_full

##UNEQUAL DISPERSAL GRAPHING
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete_IDs.RData')
head(twopatch_ext_unequal_complete_IDs) #the colour column is from the ID troubleshooting
twopatch_ext_unequal_complete_IDs$colour <- NULL
twopatch_ext_complete <- twopatch_ext_unequal_complete_IDs

#to get rid of floating point errors
twopatch_ext_complete$percentofcoral <- round(twopatch_ext_complete$percentofcoral, digits = 0)
#levels(as.factor(twopatch_ext_complete$p_m)) 4 decimal points is the longest
twopatch_ext_complete$p_m <- round(twopatch_ext_complete$p_m,4)
twopatch_ext_complete$q_m <- round(twopatch_ext_complete$q_m,4)

########

#re-order by ID not by equilibrium number
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

#this line isn't needed for the unequal ones?
twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot


#scenario 1, 2
#need g = 0.29, recruitment = 0.06 and then recruitment = 0.3 and then recruitment = 0
load('~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr0.1g0.27_lvl10_20000.RData')
g_val = 0.27#0.29
recruitvalue <- recruit <- 0.1#0 #0.3 #0.06
lvl <- 0.1 #0.50 #0.25 #0.1 #0.05 #1

#need basinofattractionID for g = 0.29, recruitvalue = 0.06
#this one has the reef 2 initial conditions in a different order so can't use it 
#load("~/Documents/computecanadajune2019/sept2019run/BoAID_better/basinofattractionID_recr0.06g0.29_100percentofcoral_5850.RData")

#have to remake basinofattractionID$Equilibrium column for this 20000 run because don't have it saved on laptop, just going to put it straight into mumbytraj
mumbytrajectories$equiID <- NA
radius <- 0.005 #needs to be within a 0.005 radius from the equi point (i.e. within the same grid point? i think?) in all components?
times <- seq(0,2000, by = 0.1) #changed from 0,100 by = 0.1
finaltime <- floor(length(times)*0.1) #needs to spend last tenth of the total time within a 0.005 radius of the stable_node

numequi <- dim(twopatch_ext_complete[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node",])[1]
#coordinates of the stable equi at that recruitment and grazing value combo
M1equi <- twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$g == g_val &  twopatch_ext_complete$stability == "stable_node"]
M2equi <- twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val &  twopatch_ext_complete$stability == "stable_node"]
C1equi <- twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val &  twopatch_ext_complete$stability == "stable_node"]
C2equi <- twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val &  twopatch_ext_complete$stability == "stable_node"]

for(n in 1:210){
  for(m in 1:numequi){
    print(paste("n = ",n,"m = ", m))
    #if stay within that radius for the final 10th of the time, initial conditions + run # assigned that colour + equi number
    if((((M1equi[m] - radius) < mumbytrajectories$M1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((M1equi[m] + radius) > mumbytrajectories$M1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((C1equi[m] - radius) < mumbytrajectories$C1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((C1equi[m] + radius) > mumbytrajectories$C1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((M2equi[m] - radius) < mumbytrajectories$M2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((M2equi[m] + radius) > mumbytrajectories$M2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((C2equi[m] - radius) < mumbytrajectories$C2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((C2equi[m] + radius) > mumbytrajectories$C2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]))){
      mumbytrajectories$equiID[mumbytrajectories$Run == n] <- 	twopatch_ext_complete$ID[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val &twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$M1 == M1equi[m] & twopatch_ext_complete$M2 == M2equi[m] & twopatch_ext_complete$C1 == C1equi[m] & twopatch_ext_complete$C2 == C2equi[m]]
    }}}

#map the equilibria achieved at a certain initial condition
#mumbytrajectories$equiID <- NA
#for(i in 1:210){ #don't map properly because the initial conditions aren't in the same order since using 5850 boaID with 20000 mumbytraj
#  mumbytrajectories$equiID[mumbytrajectories$Run == i] <- basinofattractionID$Equilibrium[basinofattractionID$InitCond == i]
#}

#now, need to figure out which basin that equiID corresponds to 
#in this model i only ever saw (1) C1 = C2 > 0, M1 = M2 = 0, (2) M1 = M2 > 0, C1 = C2 = 0, (3) C1 > C2, M1 = 0, C2 = 0, M2 > 0, (4) M1 > M2, C1 = 0, C2 > 0, (5) C1 > C2, M1 < M2, all > 0, (6) C1 < C2, M1 > M2, all > 0 because i checked those conditions explicitly and they added up to 100% and the other conditions didn't exist
#basin = 1 = A, 2 = B, 3 = C, 4 = D
mumbytrajectories$basin <- NA
mumbytrajectories$colour <- NA
equiID_vals <- levels(as.factor(mumbytrajectories$equiID))
for(i in 1:210){
j <- mumbytrajectories$equiID[mumbytrajectories$Run == i][1]
#print(paste("j =",j, "i=", i))
    
    #C1 = C2, C1 > 0...under this model, when these two things are the case then M1 = M2 = 0 because C1 and C2 being equal requires it being a certain equilibrium
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 1") 
       break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 1
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#e895a1" #"pink"
    }
    
    #M1 = M2, M1 > 0...under this model, when these two things are the case then C1 = C2 = 0 because M1 and M2 being equal requires it being a certain equilibrium
    if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 2") 
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 2
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#8f745a" #"brown"
    }
    
    #C1 > C2, M1 == 0 #pure mismatch aka D...because when C1 > C2 then M1 < M2 and if M1 = 0 then M2 must be > 0 
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 3") 
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 4
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#c4c3c0" #"grey"
    }
    
    #C1 < C2, M2 == 0 #pure mismatch aka D
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] < twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4)& twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] == 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 4") 
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 4
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#c4c3c0" #"grey"
    }
    
    #C1 > C2, M1 > 0 #mixed mismatch aka C
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 5")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 3
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#87a86c" #"green"
    }
    
    #C1 < C2, M2 > 0 #mixed mismatch aka C
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] < twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4)& twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == round(recruit,4) & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#87a86c" #"green"
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 3
    }
    
}	



#doing the above for loops instead
#loading basin IDs in (what is used for the heat map plots)
#load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/Basin of Attraction Work/BoAwork_6equibadversion/with zero disp included/basinsfinalboth.RData')
#note: basinsfinalboth[basinsfinalboth[,1] == 1,4] <- equal dispersal, double high coral scenario

#Trajectory Plots/Phase Portraits
#need g = 0.29, recruitment = 0.06
#load('~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr0.06g0.29_lvl100_20000.RData')
#g_val = 0.29
#recruitvalue = 0.06

#setting 'colour' above instead of here

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
pdf(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/MultiColourPhasePortraits_Grazing", g_val,"Dispersal",recruitvalue,"_lvl",lvl*100,".pdf"))
#png(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/MultiColourPhasePortraits_Grazing", g_val,"Dispersal",recruitvalue,"_lvl",lvl*100,".png"))
par(mfrow = c(2,2))
par(mar=c(5,6,4,1)+.1) #set left to 6, the default is 4
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 1", ylab = "Coral Cover Reef 1",cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 1", ylab = "Macroalgal Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Coral Cover Reef 1", ylab = "Coral Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "Macroalgal Cover Reef 2", ylab = "Coral Cover Reef 2",cex.lab=1.5)
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID <= 0], xlim = c(0,1), ylim = c(0,1))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], pch = twopatch_ext_complete$Equilibrium[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruit*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruit*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1))

dev.off()

###SCENARIO 3 - heterogeneous grazing, equal dispersal

#HETEROGENEOUS GRAZING
load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_ID_full.RData')
#head(twopatch_ext_heterograz_ID_full)
names(twopatch_ext_heterograz_ID_full)


twopatch_ext_complete <- twopatch_ext_heterograz_ID_full 
#re-order by ID not by equilibrium number
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- -1
twopatch_ext_complete <- twopatch_ext_complete %>% arrange(paramcombo, -ID) #sort by paramcombo first, then by ID
#reverse ID technically, because ID = 0,-1 are the unstable or saddlenodes and i sort of want those behind the stable ones
#twopatch_ext_complete[1:100,]

#supp fig bifurcation plots
#dispersal bifurc plots for supp fig x1: g = 0.6, g_lvl = 0.1; g = 0.2, g_lvl = 0.5
#gval = c(0.6, 0.2)
#g_level = c(0.1, 0.5)


twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- -20 #otherwise they will look like saddle nodes
#^ had this for the others but removed bc wasn't dealing with it 
twopatch_ext_complete$Colour[twopatch_ext_complete$Colour == "green"] <- "white" #removing the green dot

#Grazing Bifurcation Graphs
#recruitvalue = c(0.1, 0.05, 0.17, 0,0)
#g_level = c(0.5, 0.5, 0.1, 0.5,0.1)


#making some mumbytraj plots for the supp mat
#plotting mumbytraj to check what's going on
#LAST ONE DIDN"T WORK
recruitvalue_vr = c(0.8,0.8,0.1, 0.05, 0.17, 0,0)
g_level_vr = c(0.1,0.5,0.5, 0.5, 0.1, 0.5,0.1)
g_val_vr = c(0.6,0.2,0.1,0.25,0.55,0.1,0.5)
lvl <- 1

for(i in 1:length(g_val_vr)){
  #load in mumbytraj of choice
  mumbytrajectories <- NULL
  #folder based on dispersal level
  #load('/Volumes/BackupPlus/mumbytraj_heteroggrazlong_full/mumbytraj_heteroggrazlong0/mumbytrajectories_recr0g0.5_glvl0.1_20000.RData')
  g_val = g_val_vr[i]
  recruitvalue <- recruit <- recruitvalue_vr[i]
  glvl = g_level_vr[i]
  load(paste0("~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_20000.RData"))
  
  #have to remake basinofattractionID$Equilibrium column for this 20000 run because don't have it saved on laptop, just going to put it straight into mumbytraj
  mumbytrajectories$equiID <- NA
  radius <- 0.005 #needs to be within a 0.005 radius from the equi point (i.e. within the same grid point? i think?) in all components?
  times <- seq(0,2000, by = 0.1) #changed from 0,100 by = 0.1
  finaltime <- floor(length(times)*0.1) #needs to spend last tenth of the total time within a 0.005 radius of the stable_node
  
  numequi <- dim(twopatch_ext_complete[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node",])[1]
  #coordinates of the stable equi at that recruitment and grazing value combo
  M1equi <- twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl &  twopatch_ext_complete$stability == "stable_node"]
  M2equi <- twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl &  twopatch_ext_complete$stability == "stable_node"]
  C1equi <- twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl &  twopatch_ext_complete$stability == "stable_node"]
  C2equi <- twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue &  twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) &  twopatch_ext_complete$q_c == recruitvalue &  twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl &  twopatch_ext_complete$stability == "stable_node"]
  
  for(n in 1:210){
    for(m in 1:numequi){
      print(paste("n = ",n,"m = ", m))
      #if stay within that radius for the final 10th of the time, initial conditions + run # assigned that colour + equi number
      if((((M1equi[m] - radius) < mumbytrajectories$M1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((M1equi[m] + radius) > mumbytrajectories$M1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((C1equi[m] - radius) < mumbytrajectories$C1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((C1equi[m] + radius) > mumbytrajectories$C1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((M2equi[m] - radius) < mumbytrajectories$M2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((M2equi[m] + radius) > mumbytrajectories$M2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((C2equi[m] - radius) < mumbytrajectories$C2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((C2equi[m] + radius) > mumbytrajectories$C2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]))){
        mumbytrajectories$equiID[mumbytrajectories$Run == n] <- 	twopatch_ext_complete$ID[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == round(recruitvalue*lvl,4) & twopatch_ext_complete$g == g_val & twopatch_ext_complete$g2 == glvl &twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$M1 == M1equi[m] & twopatch_ext_complete$M2 == M2equi[m] & twopatch_ext_complete$C1 == C1equi[m] & twopatch_ext_complete$C2 == C2equi[m]]
      }}}
  
  #map the equilibria achieved at a certain initial condition
  #mumbytrajectories$equiID <- NA
  #for(i in 1:210){ #don't map properly because the initial conditions aren't in the same order since using 5850 boaID with 20000 mumbytraj
  #  mumbytrajectories$equiID[mumbytrajectories$Run == i] <- basinofattractionID$Equilibrium[basinofattractionID$InitCond == i]
  #}
  
  #now, need to figure out which basin that equiID corresponds to 
  #in this model i only ever saw (1) C1 = C2 > 0, M1 = M2 = 0, (2) M1 = M2 > 0, C1 = C2 = 0, (3) C1 > C2, M1 = 0, C2 = 0, M2 > 0, (4) M1 > M2, C1 = 0, C2 > 0, (5) C1 > C2, M1 < M2, all > 0, (6) C1 < C2, M1 > M2, all > 0 because i checked those conditions explicitly and they added up to 100% and the other conditions didn't exist
  #basin = 1 = A, 2 = B, 3 = C, 4 = D
  mumbytrajectories$basin <- NA
  mumbytrajectories$colour <- NA
  equiID_vals <- levels(as.factor(mumbytrajectories$equiID))
  for(i in 1:210){
    j <- mumbytrajectories$equiID[mumbytrajectories$Run == i][1]
    #print(paste("j =",j, "i=", i))
    
    #equitypeone = high coral zero macroalgae both patches - M1=M2=0, C1 >0, C2 > 0
    if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 1
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#e895a1" #"pink"
    }
    
    #equitypetwo = high macroalgae zero coral both patches - C1=C2=0, M1 >0, M2 > 0
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 2
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#8f745a" #"brown"
    }
    
    #equitypethree: High C1 High M2 C2=M1=0 (basically: C2=M1=0, C1 > 0, M2 > 0)
    if((twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 4
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#c4c3c0" #"grey"
    }
    
    #equitypefour: High C2 High M1 C1=M2=0 (basically: M2=C1=0, M1 > 0, C2 > 0)
    if((twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0)){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 4
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#c4c3c0" #"grey"
    }
    
    #equitypefive: High C1 High M2 C2>0 M1>0 (basically: C1 > C2, M2 > M1 but C2,M1>0 and C1 > M1, M2 > C2)
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#87a86c" #"green"
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 3
    }
    
    #equitypesix: High C2 High M1 C1>0 M2>0 (basically: C2 > C1, M1 > M2 but M2,C1>0 and C1 < M1, M2 < C2)
    if((twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "#87a86c" #"green"
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 3
    }
    
    #equitypeseven: high coral M > 0 both patches (basically: C1 > M1, C2 > M2, but M1,M2 > 0) #this is kinda C but not rly
    if((twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "green" #no mismatch, but mixed
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 5
    }
    
    #equitypeeight: high macroalgae C > 0 both patches (basically: M1 > C1, M2 > C2, but C1,C2 > 0)
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "green" #no mismatch, but mixed
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 5
    }
    
    #equitypenine: C = M both patches
    if((twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] > 0) & (twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"]) & (twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & twopatch_ext_complete$g == round(g_val,4) & twopatch_ext_complete$q_c == recruit & twopatch_ext_complete$q_m == recruit & twopatch_ext_complete$g2 == glvl & twopatch_ext_complete$stability == "stable_node"])){
      if(!is.na(mumbytrajectories$basin[mumbytrajectories$Run == i][1])){
        print("break in if statement 6")  
        break #just because should only enter one of these if statements for each i and if that's not happening that's bad
      }
      mumbytrajectories$colour[mumbytrajectories$Run == i] <- "black" #this one is weird
      mumbytrajectories$basin[mumbytrajectories$Run == i] <- 6
    }
    
  }	
  
  
  
  
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
  #png(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/MultiColourPhasePortraits_Grazing", g_val,"Dispersal",recruitvalue,"heterograz_glvl",glvl,".png"))
  pdf(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/MultiColourPhasePortraits_Grazing", g_val,"Dispersal",recruitvalue,"heterograz_glvl",glvl,".pdf"))
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
  
  png(paste0("~/GitHub/PhDThesisProjects/CoralConnectivityResilience_2Reef_Project/finalplots/MultiColourPhasePortraits_Grazing", g_val,"Dispersal",recruitvalue,"heterograz_glvl",glvl,".png"))
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
