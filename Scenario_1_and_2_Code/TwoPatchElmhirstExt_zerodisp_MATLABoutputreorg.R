twopatch_ext_zerodisp <- read.table(file = "~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/zero dispersal/twopatch_ext_fiveMA_zerodispersal_done.txt", sep = ",")
names(twopatch_ext_zerodisp) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","troubleshoot")
head(twopatch_ext_zerodisp)


#remove the rows that are empty
twopatch_ext_zerodisp_abr <- twopatch_ext_zerodisp[twopatch_ext_zerodisp$C1 <= 1,]

#the troubleshoot column isn't helpful
twopatch_ext_zerodisp_abr$troubleshoot <- NULL


#add a percent of coral column
twopatch_ext_zerodisp_abr$percentofcoral <- 1 #(twopatch_ext_zerodisp_abr$q_m/twopatch_ext_zerodisp_abr$q_c)*100

#need to add a 'stability' column
twopatch_ext_zerodisp_abr$stability <- NA
twopatch_ext_zerodisp_abr$stability[twopatch_ext_zerodisp_abr$eig_1 > 0 & twopatch_ext_zerodisp_abr$eig_2 > 0 & twopatch_ext_zerodisp_abr$eig_3 > 0 & twopatch_ext_zerodisp_abr$eig_4 > 0] <- 'unstable_node'
twopatch_ext_zerodisp_abr$stability[twopatch_ext_zerodisp_abr$eig_1 < 0 & twopatch_ext_zerodisp_abr$eig_2 < 0 & twopatch_ext_zerodisp_abr$eig_3 < 0 & twopatch_ext_zerodisp_abr$eig_4 < 0] <- 'stable_node'
twopatch_ext_zerodisp_abr$stability[twopatch_ext_zerodisp_abr$eig_1 == 0 | twopatch_ext_zerodisp_abr$eig_2 == 0 | twopatch_ext_zerodisp_abr$eig_3 == 0 | twopatch_ext_zerodisp_abr$eig_4 == 0] <- 'bifurcation_point'
twopatch_ext_zerodisp_abr$stability[is.na(twopatch_ext_zerodisp_abr$stability)] <- 'saddle_node'

#give each of the stability designations an associated colour
twopatch_ext_zerodisp_abr$Colour <- NA
twopatch_ext_zerodisp_abr$Colour[twopatch_ext_zerodisp_abr$stability == "stable_node"] <- 'black'
twopatch_ext_zerodisp_abr$Colour[twopatch_ext_zerodisp_abr$stability == "unstable_node"] <- 'gold'
twopatch_ext_zerodisp_abr$Colour[twopatch_ext_zerodisp_abr$stability == "saddle_node"] <- 'purple'
twopatch_ext_zerodisp_abr$Colour[twopatch_ext_zerodisp_abr$stability == "bifurcation_point"] <- 'green'
head(twopatch_ext_zerodisp_abr)

#lots of repeat rows bc didn't have to do different levels of dispersal mismatch...
twopatch_ext_zerodisp_abr[930:938,]
twopatch_ext_zerodisp_abrabr <- twopatch_ext_zerodisp_abr[1:937,]
names(twopatch_ext_zerodisp_abrabr)
twopatch_ext_zerodisp_abr <- twopatch_ext_zerodisp_abrabr
#twopatch_ext_zerodisp_abrabr[,c(1:10,16)]
#plot(x = twopatch_ext_zerodisp_abrabr$g, y = twopatch_ext_zerodisp_abrabr$C1, col = twopatch_ext_zerodisp_abrabr$Colour)

#plot(x = twopatch_ext_zerodisp_abrabr$g[twopatch_ext_zerodisp_abrabr$stability == "saddle_node"], y = twopatch_ext_zerodisp_abrabr$C1[twopatch_ext_zerodisp_abrabr$stability == "saddle_node"], col = twopatch_ext_zerodisp_abrabr$Colour[twopatch_ext_zerodisp_abrabr$stability == "saddle_node"])

#plot(x = twopatch_ext_zerodisp_abrabr$g[twopatch_ext_zerodisp_abrabr$stability == "unstable_node"], y = twopatch_ext_zerodisp_abrabr$C1[twopatch_ext_zerodisp_abrabr$stability == "unstable_node"], col = twopatch_ext_zerodisp_abrabr$Colour[twopatch_ext_zerodisp_abrabr$stability == "unstable_node"])

#plot 2x2 plots
#par(mfrow = c(2,2))
#plot(x = twopatch_ext_zerodisp_abrabr$C1[twopatch_ext_zerodisp_abrabr$g == 0.3], y = twopatch_ext_zerodisp_abrabr$M1[twopatch_ext_zerodisp_abrabr$g == 0.3], pch = 20, col = twopatch_ext_zerodisp_abrabr$Colour[twopatch_ext_zerodisp_abrabr$g == 0.3], main = "Intermediate Grazing, Zero Dispersal", xlab = "C1", ylab = "M1")
#plot(x = twopatch_ext_zerodisp_abrabr$C1[twopatch_ext_zerodisp_abrabr$g == 0.3], y = twopatch_ext_zerodisp_abrabr$C2[twopatch_ext_zerodisp_abrabr$g == 0.3], pch = 20, col = twopatch_ext_zerodisp_abrabr$Colour[twopatch_ext_zerodisp_abrabr$g == 0.3], xlab = "C1", ylab = "C2")
#plot(x = twopatch_ext_zerodisp_abrabr$C2[twopatch_ext_zerodisp_abrabr$g == 0.3], y = twopatch_ext_zerodisp_abrabr$M2[twopatch_ext_zerodisp_abrabr$g == 0.3], pch = 20, col = twopatch_ext_zerodisp_abrabr$Colour[twopatch_ext_zerodisp_abrabr$g == 0.3], xlab = "C2", ylab = "M2")
#plot(x = twopatch_ext_zerodisp_abrabr$M1[twopatch_ext_zerodisp_abrabr$g == 0.3], y = twopatch_ext_zerodisp_abrabr$M2[twopatch_ext_zerodisp_abrabr$g == 0.3], pch = 20, col = twopatch_ext_zerodisp_abrabr$Colour[twopatch_ext_zerodisp_abrabr$g == 0.3], xlab = "M1", ylab = "M2")


#ADDING IDs
library(scales)
library(deSolve)

twopatch_ext_zerodisp_abr$ID <- NA

#column indicating when change to a new parameter combination
twopatch_ext_zerodisp_abr$paramcombo <- NA
paramcomb <- 1
twopatch_ext_zerodisp_abr$paramcombo[1] <- 1
ggval <- twopatch_ext_zerodisp_abr$g[1] #have to do it this way bc there might not be an equi 1 every time
for(i in 2:dim(twopatch_ext_zerodisp_abr)[1]){
	if(ggval != twopatch_ext_zerodisp_abr$g[i]){
	paramcomb <- paramcomb + 1
	twopatch_ext_zerodisp_abr$paramcombo[i] <- paramcomb	
	ggval <- twopatch_ext_zerodisp_abr$g[i]
	}else{twopatch_ext_zerodisp_abr$paramcombo[i] <- paramcomb} 
}

#does every paramcombo have a stable node?
stable <- rep(NA, paramcomb)
for(i in 1:paramcomb){
stable[i] <- "no"
numequi <- tail(twopatch_ext_zerodisp_abr$Equilibrium[twopatch_ext_zerodisp_abr$paramcombo == i], 1)
for(j in 1:numequi){
	if(twopatch_ext_zerodisp_abr$stability[twopatch_ext_zerodisp_abr$paramcombo == i][j] == "stable_node"){
stable[i] <- "yes"}
}
}
length(stable[stable == "yes"]) #99...good
#note: the nice thing about just doing stable nodes is that they can't be next to each other (in 4D space)
#so if have to define the others later, it could be good to do each separately (tho don't think the others have to be separate) 

#reset ID column 
twopatch_ext_zerodisp_abr$ID <- NA
twopatch_ext_zerodisp_abr$ID[twopatch_ext_zerodisp_abr$stability == "unstable_node"] <- 0
twopatch_ext_zerodisp_abr$ID[twopatch_ext_zerodisp_abr$stability == "bifurcation_point"] <- 0
twopatch_ext_zerodisp_abr$ID[twopatch_ext_zerodisp_abr$stability == "saddle_node"] <- 0
#because only want to define the stable nodes anyways (at least for now)

twopatch_ext_complete <- twopatch_ext_zerodisp_abr

#determine starting values
twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] <- 1 #twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] (only 1 of these, it's a mess if this number is not 1)
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

twopatch_ext_zero_ID_full <- twopatch_ext_complete
save(twopatch_ext_zero_ID_full, file = "twopatch_ext_zero_ID_full.RData")

range(twopatch_ext_zero_ID_full$ID) #0 4

recruitvalue = 0
lvl = 1 

#testing...seems fine
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Only Stable Nodes, Dispersal = ", recruitvalue, "and level =", lvl))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))




