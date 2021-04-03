library(scales)
library(deSolve)


#load in the dataset from MATLAB, convert to correct form (From 'TwoPatchElmhirst_MATLABoutputgraphing_automatic')

twopatch_ext_completeorig <- read.csv(file = "~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/twopatch_ext_fullsolve_fullrecr.csv")
head(twopatch_ext_completeorig)

#need to remove the '2's
twopatch_ext_complete <- twopatch_ext_completeorig[twopatch_ext_completeorig$C1 <= 1,]

#need to add a 'stability' column
twopatch_ext_complete$stability <- NA
twopatch_ext_complete$stability[twopatch_ext_complete$eig_1 > 0 & twopatch_ext_complete$eig_2 > 0 & twopatch_ext_complete$eig_3 > 0 & twopatch_ext_complete$eig_4 > 0] <- 'unstable_node'
twopatch_ext_complete$stability[twopatch_ext_complete$eig_1 < 0 & twopatch_ext_complete$eig_2 < 0 & twopatch_ext_complete$eig_3 < 0 & twopatch_ext_complete$eig_4 < 0] <- 'stable_node'
twopatch_ext_complete$stability[twopatch_ext_complete$eig_1 == 0 | twopatch_ext_complete$eig_2 == 0 | twopatch_ext_complete$eig_3 == 0 | twopatch_ext_complete$eig_4 == 0] <- 'bifurcation_point'
twopatch_ext_complete$stability[is.na(twopatch_ext_complete$stability)] <- 'saddle_node'

#give each of the stability designations an associated colour
twopatch_ext_complete$Colour <- NA
twopatch_ext_complete$Colour[twopatch_ext_complete$stability == "stable_node"] <- 'black'
twopatch_ext_complete$Colour[twopatch_ext_complete$stability == "unstable_node"] <- 'gold'
twopatch_ext_complete$Colour[twopatch_ext_complete$stability == "saddle_node"] <- 'purple'
twopatch_ext_complete$Colour[twopatch_ext_complete$stability == "bifurcation_point"] <- 'green'

save(twopatch_ext_complete, file = "twopatch_ext_complete_INSERTDESIGNATION.RData")
######################

#add 'ID' column to track stable equilibria appropriately (From 'TwoPatchElmhirstExt_DefiningStableEqui_2')

load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/twopatch_ext_complete.RData')
names(twopatch_ext_complete)
#column for IDs
twopatch_ext_complete$ID <- NA

#column indicating when change to a new parameter combination
twopatch_ext_complete$paramcombo <- NA
paramcomb <- 1
twopatch_ext_complete$paramcombo[1] <- 1
for(i in 2:dim(twopatch_ext_complete)[1]){
	if(twopatch_ext_complete$Equilibrium[i] == 1){
	paramcomb <- paramcomb + 1
	twopatch_ext_complete$paramcombo[i] <- paramcomb	
	}else{twopatch_ext_complete$paramcombo[i] <- paramcomb} 
}

#reset ID column 
twopatch_ext_complete$ID <- NA
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- 0
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- 0
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "saddle_node"] <- 0
#because only want to define the stable nodes anyways (at least for now)

#determine starting values
twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] <- twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
numequi_old <- length(twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]) 
equival_storage <- data.frame(ID_num=seq(1,paramcomb, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA, ID = NA, norep = 0)
MaxID <- max(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"])
equivals_old <- data.frame(EquiNum=seq(1,numequi_old, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA)
equivals_old$C1 <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 
equivals_old$M1 <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 
equivals_old$C2 <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 
equivals_old$M2 <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 

#only 1 to start with
idid <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
equival_storage$ID[idid] <-  idid
equival_storage$C1[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
equival_storage$C2[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
equival_storage$M1[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
equival_storage$M2[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]


prop_cutoff<- 0.05 #0.02,0.03 seemed a bit too picky
cutoff_add <- 0.01

warning <- rep(NA,paramcomb)
na.catcher <- rep("no",paramcomb)
bifurc <- rep(0, paramcomb)

#match the equilibria value with an ID
#check whether at bifurcation point or not (change in number of equi, OTHER THINGS?)
#check which of the next set of equilibria correspond to which of the first set, assign them IDs in line w said identification
for(i in 2:paramcomb){
print(paste("Parameter combination ", i))
numequi <- length(twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) #want the number of stable equi
equivals <- data.frame(EquiNum=seq(1,numequi, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA, matched <- 0)
equivals$C1 <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] 
equivals$M1 <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] 
equivals$C2 <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] 
equivals$M2 <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] 
if(numequi == numequi_old){ #match the IDs
	for(j in 1:numequi){ 
			Cone <- equivals$C1[j]
			Mone <- equivals$M1[j]
			Ctwo <- equivals$C2[j]
			Mtwo <- equivals$M2[j]
			for(k in 1:numequi){
			if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C1[k]) & (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C1[k])) & ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M1[k]) & (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M1[k])) & ((Ctwo + (prop_cutoff*Ctwo + cutoff_add) > equivals_old$C2[k]) & (Ctwo - (prop_cutoff*Ctwo + cutoff_add) < equivals_old$C2[k])) & ((Mtwo + (prop_cutoff*Mtwo + cutoff_add) > equivals_old$M2[k]) & (Mtwo - (prop_cutoff*Mtwo + cutoff_add) < equivals_old$M2[k])) & equivals$matched[j] < 1){
			twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j] <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == (i-1) & twopatch_ext_complete$stability == "stable_node"][k]

			idid <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
			equival_storage$ID[idid] <-  idid
		equival_storage$C1[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$C2[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M1[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M2[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]

			equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
			}
			}
		}

	ifelse(sum(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == (i-1) & twopatch_ext_complete$stability == "stable_node"]) < numequi,warning[i]<-"re-ordered", warning[i]<-":)")
	#issue: ^ after hit a 'no matches' an NA is introduced and doesn't go away until there's a bifurcation
	
	#same number of equi, not all matched
	if(sum(equivals$matched) < numequi){
	numID <- length(na.omit(equival_storage$ID))
	
	for(j in 1:numequi){
		if(equivals$matched[j] < 1){ #not currently matched
			Cone <- equivals$C1[j]
			Mone <- equivals$M1[j]
			Ctwo <- equivals$C2[j]
			Mtwo <- equivals$M2[j]
			for(k in 1:numID){ #shouldn't get repeat values from this bc same criteria as above for the already set IDs
			ConeID <- equival_storage$C1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			CtwoID <- equival_storage$C2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			MoneID <- equival_storage$M1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			MtwoID <- equival_storage$M2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			if(((Cone + (prop_cutoff*Cone + cutoff_add) > ConeID) & (Cone - (prop_cutoff*Cone + cutoff_add) < ConeID)) & ((Mone + (prop_cutoff*Mone + cutoff_add) > MoneID) & (Mone - (prop_cutoff*Mone + cutoff_add) < MoneID)) & ((Ctwo + (prop_cutoff*Ctwo + cutoff_add) > CtwoID) & (Ctwo - (prop_cutoff*Ctwo + cutoff_add) < CtwoID)) & ((Mtwo + (prop_cutoff*Mtwo + cutoff_add) > MtwoID) & (Mtwo - (prop_cutoff*Mtwo + cutoff_add) < MtwoID)) & equivals$matched[j] < 1 & equival_storage$norep[equival_storage$ID == k & (is.na(equival_storage$ID)  == F)] < 1){

	twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j] <- k
		#keep thinking about whether this is a problem to re-define thesee within the loop - made it so can't redo ID == k
	equival_storage$ID[k] <-  k
		equival_storage$C1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$C2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$norep[equival_storage$ID == k  & (is.na(equival_storage$ID)  == F)] <- 1 
		equivals$matched[j] <- 1
		
		}
	}
}
}
	}

if(sum(equivals$matched) < numequi){	#still not matched?
	warning[i] <- "new IDs assigned"
	
	for(m in 1:numequi){
	if(equivals$matched[m] == 0){
	MaxID <- MaxID + 1
	twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m] <- MaxID
	
	equival_storage$ID[MaxID] <- MaxID
		equival_storage$C1[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		equival_storage$C2[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		equival_storage$M1[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		equival_storage$M2[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		}	
	}

} 
    if(sum(is.na(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]))>0){
    	na.catcher[i] <- "yes"
        warning[i] <- "NAs detected"} 
    if(length(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) > length(unique(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"])) & na.catcher[i] == "no"){warning[i] <- "repeat values"}		
	
	
}
if(numequi != numequi_old){ #bifurcation, try to continue assigning old IDs...assign new ones otherwise
	warning[i] <- "bifurcation"
	
	for(j in 1:numequi){ 
			Cone <- equivals$C1[j]
			Mone <- equivals$M1[j]
			Ctwo <- equivals$C2[j]
			Mtwo <- equivals$M2[j]
			for(k in 1:numequi_old){
			if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C1[k]) & (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C1[k])) & ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M1[k]) & (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M1[k])) & ((Ctwo + (prop_cutoff*Ctwo + cutoff_add) > equivals_old$C2[k]) & (Ctwo - (prop_cutoff*Ctwo + cutoff_add) < equivals_old$C2[k])) & ((Mtwo + (prop_cutoff*Mtwo + cutoff_add) > equivals_old$M2[k]) & (Mtwo - (prop_cutoff*Mtwo + cutoff_add) < equivals_old$M2[k])) & equivals$matched[j] < 1){
			twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j] <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == (i-1) & twopatch_ext_complete$stability == "stable_node"][k]

idid <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
equival_storage$ID[idid] <-  idid
		equival_storage$C1[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$C2[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M1[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M2[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]

			equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
			}
			}
		}
if(sum(equivals$matched) < numequi){
	#found in old ID?
	numID <- length(na.omit(equival_storage$ID))
	
	for(j in 1:numequi){
		if(equivals$matched[j] < 1){ #not currently matched
			Cone <- equivals$C1[j]
			Mone <- equivals$M1[j]
			Ctwo <- equivals$C2[j]
			Mtwo <- equivals$M2[j]
			for(k in 1:numID){ #shouldn't get repeat values from this bc same criteria as above for the already set IDs
			ConeID <- equival_storage$C1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			CtwoID <- equival_storage$C2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			MoneID <- equival_storage$M1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			MtwoID <- equival_storage$M2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
			if(((Cone + (prop_cutoff*Cone + cutoff_add) > ConeID) & (Cone - (prop_cutoff*Cone + cutoff_add) < ConeID)) & ((Mone + (prop_cutoff*Mone + cutoff_add) > MoneID) & (Mone - (prop_cutoff*Mone + cutoff_add) < MoneID)) & ((Ctwo + (prop_cutoff*Ctwo + cutoff_add) > CtwoID) & (Ctwo - (prop_cutoff*Ctwo + cutoff_add) < CtwoID)) & ((Mtwo + (prop_cutoff*Mtwo + cutoff_add) > MtwoID) & (Mtwo - (prop_cutoff*Mtwo + cutoff_add) < MtwoID)) & equivals$matched[j] < 1 & equival_storage$norep[equival_storage$ID == k & (is.na(equival_storage$ID)  == F)] < 1){

	twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j] <- k
		#keep thinking about whether this is a problem to re-define thesee within the loop - made it so can't redo ID == k
	equival_storage$ID[k] <-  k
		equival_storage$C1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$C2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M1[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$M2[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j]
		equival_storage$norep[equival_storage$ID == k & (is.na(equival_storage$ID)  == F)] <- 1 
		equivals$matched[j] <- 1
		
		}
	}
}
} 
}	

#still  unmatched? need to assign new IDs
if(sum(equivals$matched) < numequi){	
	warning[i] <- "new IDs assigned_bifurc"
	
	for(m in 1:numequi){
	if(equivals$matched[m] == 0){
	MaxID <- MaxID + 1
	twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m] <- MaxID
	
	equival_storage$ID[MaxID] <- MaxID
		equival_storage$C1[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		equival_storage$C2[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		equival_storage$M1[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		equival_storage$M2[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m]
		}	
	}

}
	
    if(sum(is.na(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]))>0){
    	na.catcher[i] <- "yes"
    	warning[i] <- "bifurc_NA"} 
    if(length(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) > length(unique(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"])) & na.catcher[i] == "no"){warning[i] <- "repeat values_bifurc"}		
	 
}

equivals_old <- equivals
numequi_old <- numequi
equival_storage$norep  <- 0 #because just don't want reps w/n 1 paramcomb
}

#TROUBLESHOOTING - refer to 'TwoPatchElmhirstExt_DefiningStableEqui_2' for more
#warning()
nomatch <- which(is.na(warning))
nomatch <- nomatch[2:length(nomatch)] #removing 1 bc that NA isn't a mistake
#none of these have NAs anymore, but do they look ok - nomatch[4], maybe 6 are a bit...i think they're ok actually
for(m in 1:length(nomatch)){
print(m)
print(twopatch_ext_complete[twopatch_ext_complete$paramcombo == nomatch[m]-1,])
print(twopatch_ext_complete[twopatch_ext_complete$paramcombo == nomatch[m],])
}

twopatch_ext_complete$colour <- NA
#cols <- rainbow(26)
cols <- sample(tim.colors(24)) 
for(i in 1:24){
twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == i] <- cols[i] 
}

par(mfrow = c(2,1)) #if these two plots look different...some stable points didn't get an ID designation
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20, xlim = c(0,1), ylim = c(0,1))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", pch = 20, xlim = c(0,1), ylim = c(0,1))

par(mfrow = c(2,2)) #only showing the ones where ID =/= NA...but now no ID's = NA...this honestly looks kind of perfect?
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "M2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "C1", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "M2", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)

##################

#Plotting Bifurcation Diagrams (Adapted From 'TwoPatchElmhirst_MATLAB_outputgraphing_automatic')
#need to figure out how best to do this...I think that I just want to change the IDs of the non-stable points to like 0,-1,-2 or something and then use that to define the colours and hope that R orders things on figures in a reasonable way but that might not be sufficient
#also may as well use the trajectories computed below to deal with the basin of attraction side of things...

for(k in 1:99){
	for(j in 1:99){
recruitvalue = k/100
g_value =j/100
print(paste("recruitvalue =", recruitvalue, "g_value =", g_value))

#transparent version of grazing bifurcation plot
png(paste0("TransparentGrazingBifurc_Recruitment", recruitvalue,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], 0.2), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Recruitment = ", recruitvalue))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.2), pch = c(20,20))
dev.off()

#grazing bifurcation plot
png(paste0("GrazingBifurc_Recruitment", recruitvalue,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Recruitment = ", recruitvalue))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#recruitment bifurcation plot
png(paste0("RecruitmentBifurc_Grazing", g_value,".png"))
plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value], col = twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "recruitment (p_c)", ylab = "C cover", main = paste("g =", g_value))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent version of recruitment bifurc plot
png(paste0("TransparentRecruitmentBifurc_Grazing", g_value,".png"))
plot(x = twopatch_ext_complete$q_c[twopatch_ext_complete$g == g_value], y = twopatch_ext_complete$C1[twopatch_ext_complete$g == g_value], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$g == g_value],0.2), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "recruitment (p_c)", ylab = "C cover", main = paste("g =", g_value))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.2), pch = c(20,20))
dev.off()

#C1xM1, C2xM2 plots with equi values and trajectories
#recruitvalue = recruitvalue
g_val = g_value
times <- seq(0,100, by = 0.1)
ntrajectory <- 400
npoints <- length(times)
initM <- runif(ntrajectory,0,1)
initC <- (1 - initM)*(runif(ntrajectory,0,1)) #to get all of the points to fall within the lower triangle
initT <- 1 - initM - initC
#can't see the full range of trajectories without starting the values in the other patch from different starting points (can't just set M1=M2, C1=C2 re: starting points)
initM2 <- runif(ntrajectory,0,1)
initC2 <- (1 - initM2)*(runif(ntrajectory,0,1)) #to get all of the points to fall within the lower triangle
initT2 <- 1 - initM2 - initC2

#determining the trajectories
MumbyOpen_Elmhirst_2PatchExt <- function(t,state,parameters){
	with(as.list(c(state,parameters)),{
		dM1 <- a*M1*C1 - (g*M1)/(M1+Tu1) + gamma*(1-q_m)*M1*Tu1 + gamma*p_m*M2*Tu1
		dC1 <- r*(1-q_c)*Tu1*C1 - d*C1 - a*M1*C1 + p_c*r*C2*Tu1
		dT1 <- (g*M1)/(M1+Tu1) - gamma*(1-q_m)*M1*Tu1 + d*C1 - r*(1-q_c)*Tu1*C1 - (p_c*r*C2 + gamma*p_m*M2)*Tu1
		dM2 <- a*M2*C2 - (g*M2)/(M2+Tu2) + gamma*(1-p_m)*M2*Tu2 + gamma*q_m*M1*Tu2
		dC2 <- r*(1-p_c)*Tu2*C2 - d*C2 - a*M2*C2 + q_c*r*C1*Tu2
		dT2 <- (g*M2)/(M2+Tu2) - gamma*(1-p_m)*M2*Tu2 + d*C2 - r*(1-p_c)*Tu2*C2 - (gamma*q_m*M1 + q_c*r*C1)*Tu2
		list(c(dM1,dC1,dT1,dM2,dC2,dT2))
	})
}

mumbytrajectories <- data.frame(Run=rep(1:ntrajectory, each = npoints), M1 = NA, C1 = NA, T1 = NA, M2 = NA, C2 = NA, T2 = NA, TimeStep = rep(1:npoints))

for(i in 1:ntrajectory){
#Mumby parameter model
#parameters <- c(a <- 0.1, d <- 0.44, g <- 0.4, r <- 1, gamma <- 0.8)
#Elmhirst parameter model 
parameters <- c(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, p_c <- recruitvalue, q_c <- recruitvalue, p_m <- recruitvalue, q_m <- recruitvalue)
#giving M1 and M2, C1 and C2, T1 and T2 starting conditions
state <- c(M1 = initM[i], C1 = initC[i], Tu1 = initT[i], M2 = initM2[i], C2 = initC2[i], Tu2 = initT2[i])
out <- ode(y = state, times = times, func = MumbyOpen_Elmhirst_2PatchExt, parms = parameters)	
mumbytrajectories[mumbytrajectories$Run == i,"M1"] <- out[,2]
mumbytrajectories[mumbytrajectories$Run == i,"C1"] <- out[,3]
mumbytrajectories[mumbytrajectories$Run == i,"T1"] <- out[,4]
mumbytrajectories[mumbytrajectories$Run == i,"M2"] <- out[,5]
mumbytrajectories[mumbytrajectories$Run == i,"C2"] <- out[,6]
mumbytrajectories[mumbytrajectories$Run == i,"T2"] <- out[,7]
}
bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#bluefunc <- colorRampPalette(c("lightblue", "red")) #looked sort of misleading
bluefunc_ten <- bluefunc(10)
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(1,100))] <- bluefunc_ten[1]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(101,200))] <- bluefunc_ten[2]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(201,300))] <- bluefunc_ten[3]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(301,400))] <- bluefunc_ten[4]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(401,500))] <- bluefunc_ten[5]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(501,600))] <- bluefunc_ten[6]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(601,700))] <- bluefunc_ten[7]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(701,800))] <- bluefunc_ten[8]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(801,900))] <- bluefunc_ten[9]
mumbytrajectories$colour[is.element(mumbytrajectories$TimeStep, seq(901,1001))] <- bluefunc_ten[10]

#non-transparent, M1C1 C1C2 M2C2 M1M2 graphs
png(paste0("TrajectoryPlot_Grazing", g_value,"Recruitment",recruitvalue,".png"))
par(mfrow = c(2,2))
plot(x = mumbytrajectories$M1, y = mumbytrajectories$C1, pch = ".", col = mumbytrajectories$colour,xlab = "M1 cover", ylab = "C1 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val])

plot(x = mumbytrajectories$M1, y = mumbytrajectories$M2, pch = ".", col = mumbytrajectories$colour,xlab = "M1 cover", ylab = "M2 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val))
points(x = twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val])

plot(x = mumbytrajectories$C1, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "C1 cover", ylab = "C2 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val))
points(x = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val])

plot(x = mumbytrajectories$M2, y = mumbytrajectories$C2, pch = ".", col = mumbytrajectories$colour,xlab = "M2 cover", ylab = "C2 cover", main = paste("Recruitment = ", recruitvalue, ", grazing = ", g_val))
points(x = twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], y = twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val],  xlim = c(0,1), ylim = c(0,1), pch = twopatch_ext_complete$symbol[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val])

dev.off()

}}



#define basin of attraction for all scenarios included in dataset (adapted from 'TwoPatchElmhirstExt_BasinofAttraction_UsingID')


load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/complete_run/Equi tracking/twopatch_ext_complete_IDs.RData')
head(twopatch_ext_complete_IDs)
twopatch_ext_complete <- twopatch_ext_complete_IDs #just so i don't have to re-code everything
names(twopatch_ext_complete)

#each basin of attraction needs a colour
twopatch_ext_complete$basin_colour <- NA
numIDs <- length(levels(as.factor(twopatch_ext_complete$ID)))

cols <- sample(tim.colors(24)) 
for(i in 1:numIDs){
twopatch_ext_complete$basin_colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == i] <- cols[i] 
}

#STEP 1: create grid of starting points for trajectories in the region of the [0,1] x [0,1] space allowed by 1 = M+C+T 

#x and y coords of all of the initial starting points
x_coords <- seq(0.01,0.99, by = 0.01)
y_coords <- seq(0.01,0.99,by = 0.01)
#make dataframe 
grid <- data.frame(Point_Num=seq(1,length(x_coords)*length(y_coords),by=1), Minit = rep(x_coords,each = 99), Cinit = NA, Tinit = NA)
#populate C column - want a lower triangle situation in an MxC graph
for(i in 1:length(x_coords)){ 
val <- x_coords[i]
grid$Cinit[grid$Minit == val] <- c(seq(0.01,1-val,by=0.01),rep(NA,i)) #want C+M in each row to be =< 1, hence the NAs
}
#plot(grid$Minit,grid$Cinit,pch=20) #whoo it worked

#remove the NA rows
grid <- grid[-which(is.na(grid$Cinit)),] 

#populate T column
for(i in 1:dim(grid)[1]){
grid$Tinit[i] <- 1 - grid$Minit[i] - grid$Cinit[i] #the sum of each row should be 1
grid$Tinit[i] <- round(grid$Tinit[i],digits = 3)
}

#make one for reef patch 2 [need it to be a sampled version of the first one]
reord <- sample(c(1:dim(grid)[1]))
gridtwo <- data.frame(Point_Num=seq(1,dim(grid)[1],by=1), Minit = NA, Cinit = NA, Tinit = NA)
for(i in 1:dim(gridtwo)[1]){
gridtwo[i,] <- grid[reord[i],]
}

#can remove the Point_Num column, it was really just a placeholder column
grid$Point_Num <- NULL
gridtwo$Point_Num <- NULL

#STEP 2: draw some trajectories

MumbyOpen_Elmhirst_2PatchExt <- function(t,state,parameters){
	with(as.list(c(state,parameters)),{
		dM1 <- a*M1*C1 - (g*M1)/(M1+Tu1) + gamma*(1-q_m)*M1*Tu1 + gamma*p_m*M2*Tu1
		dC1 <- r*(1-q_c)*Tu1*C1 - d*C1 - a*M1*C1 + p_c*r*C2*Tu1
		dT1 <- (g*M1)/(M1+Tu1) - gamma*(1-q_m)*M1*Tu1 + d*C1 - r*(1-q_c)*Tu1*C1 - (p_c*r*C2 + gamma*p_m*M2)*Tu1
		dM2 <- a*M2*C2 - (g*M2)/(M2+Tu2) + gamma*(1-p_m)*M2*Tu2 + gamma*q_m*M1*Tu2
		dC2 <- r*(1-p_c)*Tu2*C2 - d*C2 - a*M2*C2 + q_c*r*C1*Tu2
		dT2 <- (g*M2)/(M2+Tu2) - gamma*(1-p_m)*M2*Tu2 + d*C2 - r*(1-p_c)*Tu2*C2 - (gamma*q_m*M1 + q_c*r*C1)*Tu2
		list(c(dM1,dC1,dT1,dM2,dC2,dT2))
	})
}

for(k in 1:99){
	for(j in 1:99){
recruitvalue = k/100
g_value =j/100
print(paste("recruitvalue =", recruitvalue, "g_value =", g_value))

g_val = g_value
times <- seq(0,500, by = 0.1) #changed from 0,100 by =0.1
ntrajectory <- 4950
npoints <- length(times)
initM <- grid$Minit
initC <- grid$Cinit
initT <- grid$Tinit

#can't see the full range of trajectories without starting the values in the other patch from different starting points (can't just set M1=M2, C1=C2 re: starting points)
initM2 <- gridtwo$Minit
initC2 <- gridtwo$Cinit
initT2 <- gridtwo$Tinit


#determining the trajectories

mumbytrajectories <- data.frame(Run=rep(1:ntrajectory, each = npoints), M1 = NA, C1 = NA, T1 = NA, M2 = NA, C2 = NA, T2 = NA, TimeStep = rep(1:npoints))
basinofattractionID <- data.frame(Run=rep(1:ntrajectory), Equilibrium = NA, Colour = NA, initM1 = initM[1:ntrajectory], initC1 = initC[1:ntrajectory], initT1 = initT[1:ntrajectory], initM2 = initM2[1:ntrajectory], initC2 = initC2[1:ntrajectory], initT2 = initT2[1:ntrajectory], recruitvalue = recruitvalue, grazing = g_val)

for(i in 1:ntrajectory){
#Mumby parameter model
#parameters <- c(a <- 0.1, d <- 0.44, g <- 0.4, r <- 1, gamma <- 0.8)
#Elmhirst parameter model 
parameters <- c(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, p_c <- recruitvalue, q_c <- recruitvalue, p_m <- recruitvalue, q_m <- recruitvalue)
#giving M1 and M2, C1 and C2, T1 and T2 starting conditions
state <- c(M1 = initM[i], C1 = initC[i], Tu1 = initT[i], M2 = initM2[i], C2 = initC2[i], Tu2 = initT2[i])
out <- ode(y = state, times = times, func = MumbyOpen_Elmhirst_2PatchExt, parms = parameters)	
mumbytrajectories[mumbytrajectories$Run == i,"M1"] <- out[,2]
mumbytrajectories[mumbytrajectories$Run == i,"C1"] <- out[,3]
mumbytrajectories[mumbytrajectories$Run == i,"T1"] <- out[,4]
mumbytrajectories[mumbytrajectories$Run == i,"M2"] <- out[,5]
mumbytrajectories[mumbytrajectories$Run == i,"C2"] <- out[,6]
mumbytrajectories[mumbytrajectories$Run == i,"T2"] <- out[,7]
}

save(mumbytrajectories, file = paste0("mumbytrajectories_recr",recruitvalue,"g",g_val,".RData"))

#STEP 3: define how close + how long a trajectory needs to spend near an equi point for it to have been considered 'attracted' there
#only going to include those which have stability 'stable_node' in this
radius <- 0.005 #needs to be within a 0.005 radius from the equi point (i.e. within the same grid point? i think?) in all components?
times <- seq(0,500, by = 0.1) #changed from 0,100 by = 0.1
finaltime <- floor(length(times)*0.1) #needs to spend last tenth of the total time within a 0.005 radius of the stable_node
#should all end up by some stable point, if ran the simulation for long enough

#STEP 4: colour code each grid point as determined by which basin of attraction it's in
#number of stable equi at that recruitment and grazing value combo
numequi <- dim(twopatch_ext_complete[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node",])[1]
#coordinates of the stable equi at that recruitment and grazing value combo
M1equi <- twopatch_ext_complete$M1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node"]
M2equi <- twopatch_ext_complete$M2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node"]
C1equi <- twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node"]
C2equi <- twopatch_ext_complete$C2[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node"]

for(n in 1:ntrajectory){
	for(m in 1:numequi){
print(paste("n = ",n,"m = ", m))
#if stay within that radius for the final 10th of the time, initial conditions + run # assigned that colour + equi number
if((((M1equi[m] - radius) < mumbytrajectories$M1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((M1equi[m] + radius) > mumbytrajectories$M1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((C1equi[m] - radius) < mumbytrajectories$C1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((C1equi[m] + radius) > mumbytrajectories$C1[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((M2equi[m] - radius) < mumbytrajectories$M2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((M2equi[m] + radius) > mumbytrajectories$M2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)])) & (((C2equi[m] - radius) < mumbytrajectories$C2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]) && ((C2equi[m] + radius) > mumbytrajectories$C2[mumbytrajectories$Run == n & mumbytrajectories$TimeStep > (length(times) - finaltime)]))){
basinofattractionID$Equilibrium[basinofattractionID$Run ==  n] <- 	twopatch_ext_complete$ID[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$M1 == M1equi[m] & twopatch_ext_complete$M2 == M2equi[m] & twopatch_ext_complete$C1 == C1equi[m] & twopatch_ext_complete$C2 == C2equi[m]]
basinofattractionID$Colour[basinofattractionID$Run ==  n] <- 	twopatch_ext_complete$basin_colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$M1 == M1equi[m] & twopatch_ext_complete$M2 == M2equi[m] & twopatch_ext_complete$C1 == C1equi[m] & twopatch_ext_complete$C2 == C2equi[m]]
}
}}
save(basinofattractionID, file = paste0("basinofattractionID_recr",recruitvalue,"g",g_val,"_5000.RData"))
#NEED TO ADD IN SOMETHING TO CALCULATE NUM OF POINTS THAT GO TO EACH BASIN AND THEN SAVE THOSE SOMEWHERE
}
}
#first check if there are any that satisfy this and if there are, colour them black and re-evaluate at longer traj lengths
basinofattractionID$Colour[which(is.na(basinofattractionID$Colour))] <- "black" 

#NEED TO ADD IN SOMETHING TO CALCULATE NUM OF POINTS THAT GO TO EACH BASIN AND THEN SAVE THOSE SOMEWHERE


