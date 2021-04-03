twopatch_ext_heterograzing_bigdisp <- read.table(file = "~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_nosmalldisp_done.txt", sep = ",")
names(twopatch_ext_heterograzing_bigdisp) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","g2","troubleshoot")
head(twopatch_ext_heterograzing_bigdisp)

twopatch_ext_heterograzing_zero <- read.table(file = "~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Heterogeneous_Grazing/twopatch_ext_heterograz_zerodisp_done.txt", sep = ",")
names(twopatch_ext_heterograzing_zero) <-c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","g2","troubleshoot")
head(twopatch_ext_heterograzing_zero)

#remove the rows that are empty
twopatch_ext_heterograzing_bigdisp_abr <- twopatch_ext_heterograzing_bigdisp[twopatch_ext_heterograzing_bigdisp$C1 <= 1,]
head(twopatch_ext_heterograzing_bigdisp_abr)
#dim(twopatch_ext_heterograzing_abr)

twopatch_ext_heterograzing_zero_abr <- twopatch_ext_heterograzing_zero[twopatch_ext_heterograzing_zero$C1 <= 1,]
head(twopatch_ext_heterograzing_zero_abr)

twopatch_ext_heterograzing <- rbind(twopatch_ext_heterograzing_zero_abr, twopatch_ext_heterograzing_bigdisp_abr)
head(twopatch_ext_heterograzing)

#the troubleshoot column isn't helpful
twopatch_ext_heterograzing$troubleshoot <- NULL


#need to add a 'stability' column
twopatch_ext_heterograzing$stability <- NA
twopatch_ext_heterograzing$stability[twopatch_ext_heterograzing$eig_1 > 0 & twopatch_ext_heterograzing$eig_2 > 0 & twopatch_ext_heterograzing$eig_3 > 0 & twopatch_ext_heterograzing$eig_4 > 0] <- 'unstable_node'
twopatch_ext_heterograzing$stability[twopatch_ext_heterograzing$eig_1 < 0 & twopatch_ext_heterograzing$eig_2 < 0 & twopatch_ext_heterograzing$eig_3 < 0 & twopatch_ext_heterograzing$eig_4 < 0] <- 'stable_node'
twopatch_ext_heterograzing$stability[twopatch_ext_heterograzing$eig_1 == 0 | twopatch_ext_heterograzing$eig_2 == 0 | twopatch_ext_heterograzing$eig_3 == 0 | twopatch_ext_heterograzing$eig_4 == 0] <- 'bifurcation_point'
twopatch_ext_heterograzing$stability[is.na(twopatch_ext_heterograzing$stability)] <- 'saddle_node'

#give each of the stability designations an associated colour
twopatch_ext_heterograzing$Colour <- NA
twopatch_ext_heterograzing$Colour[twopatch_ext_heterograzing$stability == "stable_node"] <- 'black'
twopatch_ext_heterograzing$Colour[twopatch_ext_heterograzing$stability == "unstable_node"] <- 'gold'
twopatch_ext_heterograzing$Colour[twopatch_ext_heterograzing$stability == "saddle_node"] <- 'purple'
twopatch_ext_heterograzing$Colour[twopatch_ext_heterograzing$stability == "bifurcation_point"] <- 'green'

head(twopatch_ext_heterograzing)

#ADDING IDs
library(scales)
library(deSolve)

twopatch_ext_heterograzing$ID <- NA

#column indicating when change to a new parameter combination
twopatch_ext_heterograzing$paramcombo <- NA
paramcomb <- 1
twopatch_ext_heterograzing$paramcombo[1] <- 1
for(i in 2:dim(twopatch_ext_heterograzing)[1]){
	if(twopatch_ext_heterograzing$Equilibrium[i] == 1){
	paramcomb <- paramcomb + 1
	twopatch_ext_heterograzing$paramcombo[i] <- paramcomb	
	}else{twopatch_ext_heterograzing$paramcombo[i] <- paramcomb} 
}
#paramcomb got up to 29700, which makes sense bc 99*100*3

#does every paramcombo have a stable node?
stable <- rep(NA, paramcomb)
for(i in 1:paramcomb){
stable[i] <- "no"
numequi <- tail(twopatch_ext_heterograzing$Equilibrium[twopatch_ext_heterograzing$paramcombo == i], 1)
for(j in 1:numequi){
	if(twopatch_ext_heterograzing$stability[twopatch_ext_heterograzing$paramcombo == i][j] == "stable_node"){
stable[i] <- "yes"}
}
}
length(stable[stable == "yes"]) #29699...eep
which(stable == "no") #3123
twopatch_ext_heterograzing[twopatch_ext_heterograzing$paramcombo == 3123,] #yeah, just 2 saddle nodes huh
#one is only not stable in one component and the other is stable in that component? so maybe it's fine?

#note: the nice thing about just doing stable nodes is that they can't be next to each other (in 4D space)
#so if have to define the others later, it could be good to do each separately  

#reset ID column 
twopatch_ext_heterograzing$ID <- NA
twopatch_ext_heterograzing$ID[twopatch_ext_heterograzing$stability == "unstable_node"] <- 0
twopatch_ext_heterograzing$ID[twopatch_ext_heterograzing$stability == "bifurcation_point"] <- 0
twopatch_ext_heterograzing$ID[twopatch_ext_heterograzing$stability == "saddle_node"] <- 0
#because only want to define the stable nodes anyways (at least for now)


twopatch_ext_complete <- twopatch_ext_heterograzing

#determine starting values
#twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] <- twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] <- 1 #was always 1 before, things get messed up below if this doesn't start at 1
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

#ran into some issue at i = 18 (fixed by assigning the first ID to 1), issue with 3123 bc noo stable node so need to skip
for(i in 2:3122){ #2:paramcomb
print(paste("Parameter combination ", i))
numequi <- length(twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) #want the number of stable equi
equivals <- data.frame(EquiNum=seq(1,numequi, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA, matched = 0) #changed from matched <- 0
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
			#issue here at k = 1 when i = 18, not sure why
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
} #ok so it goes through all the unmatched ones and tries to match them with a previous ID, if some are still unmatched...
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

for(i in 3124:paramcomb){ #2:paramcomb
print(paste("Parameter combination ", i))
numequi <- length(twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) #want the number of stable equi
equivals <- data.frame(EquiNum=seq(1,numequi, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA, matched = 0) #changed from matched <- 0
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
			#issue here at k = 1 when i = 18, not sure why
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
} #ok so it goes through all the unmatched ones and tries to match them with a previous ID, if some are still unmatched...
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

twopatch_ext_heterograz_ID_full <- twopatch_ext_complete

#12.9.2019: MAKE ALL OF THESE ID = 302 - NOT DONE YET (determined they were getting ID = NA bc of that one without a stable node above, but they all look to be the same equi)
for(i in 55:99){
rv <- round(i/100,4)
twopatch_ext_heterograz_ID_full$ID[twopatch_ext_heterograz_ID_full$p_c == rv & twopatch_ext_heterograz_ID_full$p_m == rv & twopatch_ext_heterograz_ID_full$q_c == rv & twopatch_ext_heterograz_ID_full$q_m == rv & twopatch_ext_heterograz_ID_full$g == 0.29 & twopatch_ext_heterograz_ID_full$g2 == 0.1 & twopatch_ext_heterograz_ID_full$stability == "stable_node"] <- 302
}


save(twopatch_ext_heterograz_ID_full, file = "twopatch_ext_heterograz_ID_full.RData")

range(twopatch_ext_heterograz_ID_full$ID, na.rm=TRUE) #301!





