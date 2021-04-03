#load in the .txt files
twopatch_ext_unequal_ptone <- read.table(file = "~/Dropbox/University of Toronto/Research Related/MATLAB Outputs from other computers/Pt1 of data from coral dispersal higher than macroalgae dispersal/twopatch_ext_fourMA_inprog.txt", sep = ",")
names(twopatch_ext_unequal_ptone) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4")
head(twopatch_ext_unequal_ptone)

#need to remove the '2's
twopatch_ext_unequal_ptone_abr <- twopatch_ext_unequal_ptone[twopatch_ext_unequal_ptone$C1 <= 1,]

#next file
twopatch_ext_unequal_pttwo <- read.table(file = "~/Dropbox/University of Toronto/Research Related/MATLAB Outputs from other computers/twopatch_ext_fourMA_inprog_pt2.txt", sep = ",")
names(twopatch_ext_unequal_pttwo) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","troubleshoot")
head(twopatch_ext_unequal_pttwo)

#need to remove the '2's
twopatch_ext_unequal_pttwo_abr <- twopatch_ext_unequal_pttwo[twopatch_ext_unequal_pttwo$C1 <= 1,]

#next file
twopatch_ext_unequal_ptthree <- read.table(file = "~/Dropbox/University of Toronto/Research Related/MATLAB Outputs from other computers/twopatch_ext_fourMA_inprog_pt3.txt", sep = ",")
names(twopatch_ext_unequal_ptthree) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","troubleshoot")
head(twopatch_ext_unequal_ptthree)

#need to remove the '2's
twopatch_ext_unequal_ptthree_abr <- twopatch_ext_unequal_ptthree[twopatch_ext_unequal_ptthree$C1 <= 1,]

#next file - this one is possibly extra
twopatch_ext_unequal_ptfour <- read.table(file = "~/Dropbox/University of Toronto/Research Related/MATLAB Outputs from other computers/twopatch_ext_fourMA_done.txt", sep = ",")
names(twopatch_ext_unequal_ptfour) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","troubleshoot")
head(twopatch_ext_unequal_ptfour)

#need to remove the '2's
twopatch_ext_unequal_ptfour_abr <- twopatch_ext_unequal_ptfour[twopatch_ext_unequal_ptfour$C1 <= 1,]

#5.29.2019 - adding in the lost middle bit 
twopatch_ext_unequal_middle <- read.table(file = "~/Dropbox/University of Toronto/Research Related/MATLAB Outputs from other computers/twopatch_ext_fourMA_lostmiddlebit_done.txt", sep = ",")
names(twopatch_ext_unequal_middle) <- c("g","q_c","p_c","q_m","p_m","Equilibrium","C1","M1","C2","M2","eig_1","eig_2","eig_3","eig_4","troubleshoot")
head(twopatch_ext_unequal_middle)

#need to remove the '2's
twopatch_ext_unequal_middle_abr <- twopatch_ext_unequal_middle[twopatch_ext_unequal_middle$C1 <= 1,]


#the troubleshoot columns in the abridged dataframes aren't helpful
twopatch_ext_unequal_pttwo_abr$troubleshoot <- NULL
twopatch_ext_unequal_ptthree_abr$troubleshoot <- NULL
twopatch_ext_unequal_ptfour_abr$troubleshoot <- NULL
twopatch_ext_unequal_middle_abr$troubleshoot <- NULL

#now need to put them together
head(twopatch_ext_unequal_ptone_abr)
head(twopatch_ext_unequal_pttwo_abr) #empty...
head(twopatch_ext_unequal_ptthree_abr)
head(twopatch_ext_unequal_ptfour_abr) #seems like this one is identical to pt3 
head(twopatch_ext_unequal_middle_abr)

#checking identical-ness of ptthree and ptfour
which(twopatch_ext_unequal_ptthree_abr$q_m != twopatch_ext_unequal_ptfour_abr$q_m) #okay they're identical

#^ that  seems a bit suspicious, need to check that all of the conditions have been included
#q_c and p_c need to go from 0.01 -> 0.99, jumping by 0.01 each time
#q_m and p_m need to go through those values*0.05, *0.1, *0.25, *0.5
levels(as.factor(twopatch_ext_unequal_ptone_abr$q_c)) #okay that's not informative, this one has all of them
check_ptone <- levels(as.factor(twopatch_ext_unequal_ptone_abr$q_m))
check_ptthree <- levels(as.factor(twopatch_ext_unequal_ptthree_abr$q_m))
base <- seq(0.01,0.99, by = 0.01)
base_five <- base*0.05
base_ten <- base*0.1
base_twentyfive <- base*0.25
base_fifty <- base*0.5
length(c(base_five,base_ten,base_twentyfive,base_fifty))

#ptone_abr goes from 0.01*0.05 -> 0.67*0.1 and ptthree_abr goes from 0.44*0.1 -> 0.99*0.5 and i checked a few intermediate conditions and they showed up - but i actually am missing from g = 0.6 -> g = 0.77 from the *0.1 round (~1660 in the middle are missing)...this is in middle abridged

#now need to put them all together + add a column ID'ing how much coral dispersal > macroalgal dispersal
#middle abr has a bit of overlap on purpose, need to remove that
dim(twopatch_ext_unequal_ptone_abr) 
twopatch_ext_unequal_ptone_abr[52048,] #g = 0.6, q_c = 0.67, q_m = 0.067
twopatch_ext_unequal_ptthree_abr[1,] #g = 0.77, q_c = 0.44, q_m = 0.044
twopatch_ext_unequal_middle_abr[28,] #start here
dim(twopatch_ext_unequal_middle_abr)
twopatch_ext_unequal_middle_abr[4961,] #end here

twopatch_ext_unequal_middle_abr$percentofcoral <- (twopatch_ext_unequal_middle_abr$q_m/twopatch_ext_unequal_middle_abr$q_c)*100
twopatch_ext_unequal_ptone_abr$percentofcoral <- (twopatch_ext_unequal_ptone_abr$q_m/twopatch_ext_unequal_ptone_abr$q_c)*100
twopatch_ext_unequal_ptthree_abr$percentofcoral <- (twopatch_ext_unequal_ptthree_abr$q_m/twopatch_ext_unequal_ptthree_abr$q_c)*100
save(twopatch_ext_unequal_ptthree_abr,twopatch_ext_unequal_ptone_abr,twopatch_ext_unequal_middle_abr, file = "TwoPatchElmExt_unequal_sepdataframes.RData")

#join them together
twopatch_ext_unequal_complete <- rbind(twopatch_ext_unequal_ptone_abr,twopatch_ext_unequal_middle_abr[28:4961,],twopatch_ext_unequal_ptthree_abr)
#looked at random rows, seems to have been done properly
save(twopatch_ext_unequal_complete, file = "twopatch_ext_unequal_complete.RData")

#need to add a 'stability' column
twopatch_ext_unequal_complete$stability <- NA
twopatch_ext_unequal_complete$stability[twopatch_ext_unequal_complete$eig_1 > 0 & twopatch_ext_unequal_complete$eig_2 > 0 & twopatch_ext_unequal_complete$eig_3 > 0 & twopatch_ext_unequal_complete$eig_4 > 0] <- 'unstable_node'
twopatch_ext_unequal_complete$stability[twopatch_ext_unequal_complete$eig_1 < 0 & twopatch_ext_unequal_complete$eig_2 < 0 & twopatch_ext_unequal_complete$eig_3 < 0 & twopatch_ext_unequal_complete$eig_4 < 0] <- 'stable_node'
twopatch_ext_unequal_complete$stability[twopatch_ext_unequal_complete$eig_1 == 0 | twopatch_ext_unequal_complete$eig_2 == 0 | twopatch_ext_unequal_complete$eig_3 == 0 | twopatch_ext_unequal_complete$eig_4 == 0] <- 'bifurcation_point'
twopatch_ext_unequal_complete$stability[is.na(twopatch_ext_unequal_complete$stability)] <- 'saddle_node'

#give each of the stability designations an associated colour
twopatch_ext_unequal_complete$Colour <- NA
twopatch_ext_unequal_complete$Colour[twopatch_ext_unequal_complete$stability == "stable_node"] <- 'black'
twopatch_ext_unequal_complete$Colour[twopatch_ext_unequal_complete$stability == "unstable_node"] <- 'gold'
twopatch_ext_unequal_complete$Colour[twopatch_ext_unequal_complete$stability == "saddle_node"] <- 'purple'
twopatch_ext_unequal_complete$Colour[twopatch_ext_unequal_complete$stability == "bifurcation_point"] <- 'green'

head(twopatch_ext_unequal_complete)
save(twopatch_ext_unequal_complete, file = "twopatch_ext_unequal_complete.RData")

##################

load('~/Dropbox/University of Toronto/Research Related/R outputs/Two Patch Elmhirst extended/Unequal coral macroalgal dispersal/twopatch_ext_unequal_complete.RData')
head(twopatch_ext_unequal_complete)

#ADDING IDs
library(scales)
library(deSolve)

twopatch_ext_complete <- twopatch_ext_unequal_complete #for simplicity
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
#paramcomb got up to 39204 which makes sense because 4*99*99=39204

#determining values
#i think it should be proportional difference instead of an absolute value, because that's been getting me in trouble thus far
#pts <- seq(0.01,1,0.01)
#plot(x=1:100,y=pts,pch = 20, col = "blue")
#points(x = 1:100, y=(pts*0.05)+0.01+pts, pch = 20, col = "green") #seems like a good cutoff
#points(x=1:100,y=(pts*0.03)+pts, pch = 20, col = "red") #this is smaller than a 0.02 cutoff at the small values, larger at the larger values which seems about what i want bc 0.02 was leaving me with some 'no matches' that shouldn't have been
#nope it's too sensitive to the small values
#points(x=1:100,y=0.02+pts, pch = 20, col = "black") 
#points(x=1:100,y=(pts*0.1)+pts, pch=20, col = "green") #this seems too big

#does every paramcombo have a stable node?
stable <- rep(NA, paramcomb)
for(i in 1:paramcomb){
stable[i] <- "no"
numequi <- tail(twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == i], 1)
for(j in 1:numequi){
	if(twopatch_ext_complete$stability[twopatch_ext_complete$paramcombo == i][j] == "stable_node"){
stable[i] <- "yes"}
}
}
length(stable[stable == "yes"]) #39204...good
#note: the nice thing about just doing stable nodes is that they can't be next to each other (in 4D space)
#so if have to define the others later, it could be good to do each separately (tho don't think the others have to be separate) 

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

#95 different IDs oh boy

twopatch_ext_complete[is.na(twopatch_ext_complete$ID),] #0
#but there are NAs in the warning thing...
length(which(is.na(warning))) #77
for(i in 1:length(which(is.na(warning)))){
print(twopatch_ext_complete[twopatch_ext_complete$paramcombo == which(is.na(warning))[i],])
} #ubt all of the IDs got assigned so it should be fine? unclear why from ^ that NAs got assigned

#THINGS TO THINK ABOUT: i did this indiscriminately across all of the unequal scenarios (5,10,25,50)...maybe i should've done one per set?

library(fields)
twopatch_ext_complete$colour <- NA
#cols <- rainbow(26)
cols <- sample(tim.colors(95)) 
for(i in 1:95){
twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == i] <- cols[i] 
}

twopatch_ext_unequal_complete_IDs <- twopatch_ext_complete

save(twopatch_ext_unequal_complete_IDs, file = "twopatch_ext_unequal_complete_IDs.RData")

par(mfrow = c(2,1)) #no NAs left - these all match and look pretty decent
#coloured by ID designation
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20, xlim = c(0,1), ylim = c(0,1))
#all should show up, regardless of whether they have IDs or not
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", pch = 20, xlim = c(0,1), ylim = c(0,1))

par(mfrow = c(2,1)) #this shows the ones im missing pretty clearly [old version]
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", col = twopatch_ext_complete$ID[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", pch = 20)

par(mfrow = c(2,2)) #only showing the ones where ID =/= NA
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "M2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "C1", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "M2", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)

lvl = 50
par(mfrow = c(2,2)) #only showing the ones where ID =/= NA, for one percent coral level at a time - all seem good
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], xlab = "M1", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], pch = 20)
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], xlab = "M1", ylab = "M2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], pch = 20)
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], xlab = "C1", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], pch = 20)
plot(x = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], xlab = "M2", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$percentofcoral == lvl], pch = 20)
