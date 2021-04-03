library(scales)
library(deSolve)
library(fields)

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
#paramcomb got up to 9801 which makes sense given 99*99=9801

#i think it should be proportional difference instead of an absolute value, because that's been getting me in trouble thus far
pts <- seq(0.01,1,0.01)
plot(x=1:100,y=pts,pch = 20, col = "blue")
points(x = 1:100, y=(pts*0.05)+0.01+pts, pch = 20, col = "green") #seems like a good cutoff
points(x=1:100,y=(pts*0.03)+pts, pch = 20, col = "red") #this is smaller than a 0.02 cutoff at the small values, larger at the larger values which seems about what i want bc 0.02 was leaving me with some 'no matches' that shouldn't have been
#nope it's too sensitive to the small values
points(x=1:100,y=0.02+pts, pch = 20, col = "black") 
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
length(stable[stable == "yes"]) #9801...so yes, that makes things easier

#reset ID column 
twopatch_ext_complete$ID <- NA
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "unstable_node"] <- 0
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "bifurcation_point"] <- 0
twopatch_ext_complete$ID[twopatch_ext_complete$stability == "saddle_node"] <- 0
#because only want to define the stable nodes anyways (at least for now)

#determine starting values
twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] <- twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]
numequi_old <- length(twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"]) 
MaxID <- max(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"])
equivals_old <- data.frame(EquiNum=seq(1,numequi_old, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA)
equivals_old$C1 <- twopatch_ext_complete$C1[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 
equivals_old$M1 <- twopatch_ext_complete$M1[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 
equivals_old$C2 <- twopatch_ext_complete$C2[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 
equivals_old$M2 <- twopatch_ext_complete$M2[twopatch_ext_complete$paramcombo == 1 & twopatch_ext_complete$stability == "stable_node"] 


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
			equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
			}
			}
		}
	#if each of these are not identical, sum will be less than numequi bc summing TRUE+TRUE+TRUE...i think if there are NAs would see 're-ordered' until it gets replaced later
	ifelse(sum(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] == twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == (i-1) & twopatch_ext_complete$stability == "stable_node"]) < numequi,warning[i]<-"re-ordered", warning[i]<-":)")
	#issue: ^ after hit a 'no matches' an NA is introduced and doesn't go away until there's a bifurcation
	#same number of equi, not all matched
	if(sum(equivals$matched) < numequi){
		warning[i] <- "no matches" #not sure what to do with this yet, just want to see if it's going to happen
	} 
    if(sum(is.na(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]))>0){
    	na.catcher[i] <- "yes"} 
    if(length(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) > length(unique(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"])) & na.catcher[i] == "no"){warning[i] <- "repeat values"}		
	
	
}
if(numequi != numequi_old){ #bifurcation, try to continue assigning old IDs...assign new ones otherwise
	#twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"] <- twopatch_ext_complete$Equilibrium[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]
	warning[i] <- "bifurcation"
		for(j in 1:numequi){ 
			Cone <- equivals$C1[j]
			Mone <- equivals$M1[j]
			Ctwo <- equivals$C2[j]
			Mtwo <- equivals$M2[j]
			for(k in 1:numequi_old){
			if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C1[k]) & (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C1[k])) & ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M1[k]) & (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M1[k])) & ((Ctwo + (prop_cutoff*Ctwo + cutoff_add) > equivals_old$C2[k]) & (Ctwo - (prop_cutoff*Ctwo + cutoff_add) < equivals_old$C2[k])) & ((Mtwo + (prop_cutoff*Mtwo + cutoff_add) > equivals_old$M2[k]) & (Mtwo - (prop_cutoff*Mtwo + cutoff_add) < equivals_old$M2[k])) & equivals$matched[j] < 1){
			twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j] <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == (i-1) & twopatch_ext_complete$stability == "stable_node"][k]
			equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
			}
			}
		}
if(sum(equivals$matched) < numequi){
	warning[i] <- "new assignments"
	
	for(m in 1:numequi){
	if(equivals$matched[m] == 0){
	MaxID <- MaxID + 1
	twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][m] <- MaxID
		}	
	}

	}
	
	
    if(sum(is.na(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]))>0){
    	na.catcher[i] <- "yes"} 
    if(length(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"]) > length(unique(twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"])) & na.catcher[i] == "no"){warning[i] <- "repeat values_bifurc"}		
	 
}

equivals_old <- equivals
numequi_old <- numequi
}
length(warning[warning == "new assignments"]) #14 - seems kind of low considering it goes from 1 -> 24 but multiple in one round i guess? yeah often 2 at a time (always, in this case)
length(warning[warning == "repeat values_bifurc"]) #1 <- this seems fake bc i don't see this anywhere, just see 'new assignments', 'bifurcation' and 'no matches'
#no matches: 2873,2874,2875,2972,2973,3071,3170, etc
#26 levels
twopatch_ext_complete$colour <- NA
#cols <- rainbow(26)
cols <- sample(tim.colors(26)) 
for(i in 1:26){
twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == i] <- cols[i] 
}
for(i in 1:26){ #first two have a bunch of values, rest just have 1...i feel like i fixed this before
print(paste(length(twopatch_ext_complete$C1[is.element(twopatch_ext_complete$ID,i)]),	"were given ID =", i))	
print(paste("The range of paramcombo's is from", range(twopatch_ext_complete$paramcombo[is.element(twopatch_ext_complete$ID,i)])[1], "to", range(twopatch_ext_complete$paramcombo[is.element(twopatch_ext_complete$ID,i)])[2]))
}

nomatch <- which(warning == "no matches")


par(mfrow = c(2,2)) #this shows the ones im missing pretty clearly, 'no match' is getting all the NAs (and more bc for a whole paramcombo)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20, xlim = c(0,1), ylim = c(0,1))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", pch = 20, xlim = c(0,1), ylim = c(0,1))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node" & is.element(twopatch_ext_complete$paramcombo, nomatch)], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & is.element(twopatch_ext_complete$paramcombo, nomatch)], xlab = "g", ylab = "C1", pch = 4, xlim = c(0,1), ylim = c(0,1))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node" & is.na(twopatch_ext_complete$ID)], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & is.na(twopatch_ext_complete$ID)], xlab = "g", ylab = "C1", pch = 8, xlim = c(0,1), ylim = c(0,1))

warning[twopatch_ext_complete$paramcombo[is.na(twopatch_ext_complete$ID)]] #all = "no matches"




par(mfrow = c(2,1)) #this shows the ones im missing pretty clearly, the colours are weird...
#plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", col = twopatch_ext_complete$ID[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "g", ylab = "C1", pch = 20)


#1 and 2 are the two lines, the rest are just individual dots
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == 24], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == 24], xlab = "g", ylab = "C1", pch = 20)

#3-24 - just 1 line each (but getting a ton of NA rows too for some reason)
twopatch_ext_complete[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == 3,]
twopatch_ext_complete[twopatch_ext_complete$ID == 3 & is.na(twopatch_ext_complete$ID)==F,] #gets rid of the NA rows
twopatch_ext_complete[is.element(twopatch_ext_complete$ID, 3:24) & is.na(twopatch_ext_complete$ID)==F,] 
#^ showing them sequentially like that seems to show that the reason they keep getting re-IDd is it's switching between two points (can kind of see in some of the graphs above also)

#should have a for loop looping through the 'no matches' trying to match them with the existing IDs
#but should probably first go through the ID'd ones and try to reduce the number of IDs
#or should use a bigger cut off?

numID <- length(levels(as.factor(na.omit(twopatch_ext_complete$ID)))) - 1 #remove the 0 case
#loop through the IDs,  try and reduce the number using the same cut off as before
#I DONT THINK THIS IS THE RIGHT WAY TO DO IT
for(i  in 1:numID){
numequi <- length(twopatch_ext_complete$C1[twopatch_ext_complete$ID == i & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"])
equivals <- data.frame(EquiNum=seq(1,numequi, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA, matched <- 0)
equivals$C1 <- twopatch_ext_complete$C1[twopatch_ext_complete$ID == i & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
equivals$M1 <- twopatch_ext_complete$M1[twopatch_ext_complete$ID == i & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
equivals$C2 <- twopatch_ext_complete$C2[twopatch_ext_complete$ID == i & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
equivals$M2 <- twopatch_ext_complete$M2[twopatch_ext_complete$ID == i & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
	
for(j in 1:numID){ 
numequi_comp <- length(twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"])
equivals_comp <- data.frame(EquiNum=seq(1,numequi_comp, by=1), C1 = NA, M1 = NA, C2 = NA, M2 = NA, matched <- 0)
equivals_comp$C1 <- twopatch_ext_complete$C1[twopatch_ext_complete$ID == j & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
equivals_comp$M1 <- twopatch_ext_complete$M1[twopatch_ext_complete$ID == j & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
equivals_comp$C2 <- twopatch_ext_complete$C2[twopatch_ext_complete$ID == j & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 
equivals_comp$M2 <- twopatch_ext_complete$M2[twopatch_ext_complete$ID == j & is.na(twopatch_ext_complete$ID)==F & twopatch_ext_complete$stability == "stable_node"] 

			for(k in 1:numequi_comp){
			if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C1[k]) & (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C1[k])) & ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M1[k]) & (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M1[k])) & ((Ctwo + (prop_cutoff*Ctwo + cutoff_add) > equivals_old$C2[k]) & (Ctwo - (prop_cutoff*Ctwo + cutoff_add) < equivals_old$C2[k])) & ((Mtwo + (prop_cutoff*Mtwo + cutoff_add) > equivals_old$M2[k]) & (Mtwo - (prop_cutoff*Mtwo + cutoff_add) < equivals_old$M2[k])) & equivals$matched[j] < 1){
			twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == i & twopatch_ext_complete$stability == "stable_node"][j] <- twopatch_ext_complete$ID[twopatch_ext_complete$paramcombo == (i-1) & twopatch_ext_complete$stability == "stable_node"][k]
			equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
		}
	}
}
}



levels(as.factor(twopatch_ext_complete$ID))

#okay im only getting 'no matches' in relation with bifurcations now, and no repeated values - only 22 of them also
i = 1 #ok checked all of them, they look suspiciously clean? like most are only one value which seems wrong..but i guess the stable values are only doing funny things for a few values...so it could be ok 
par(mfrow = c(2,2))
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$ID == i], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == i], xlab = "M1", ylab = "C1")
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$ID == i], y = twopatch_ext_complete$M2[twopatch_ext_complete$ID == i], xlab = "M1", ylab = "M2")
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$ID == i], y = twopatch_ext_complete$C2[twopatch_ext_complete$ID == i], xlab = "C1", ylab = "C2")
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$ID == i], y = twopatch_ext_complete$M2[twopatch_ext_complete$ID == i], xlab = "M1", ylab = "M2")

par(mfrow = c(2,2)) #only showing the ones where ID =/= NA
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "C1", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "M2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "C1", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)
plot(x = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "M2", ylab = "C2", col = twopatch_ext_complete$colour[twopatch_ext_complete$stability == "stable_node"], pch = 20)

idval <- 1 #only idval 1 shows up in more than 1 spot...weird
par(mfrow = c(2,2))
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], xlab = "M1", ylab = "C1")
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], xlab = "M1", ylab = "M2")
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], xlab = "C1", ylab = "C2")
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$ID == idval], xlab = "M1", ylab = "M2")



par(mfrow = c(2,2))
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "C1")
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "M2")
plot(x = twopatch_ext_complete$C1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$C2[twopatch_ext_complete$stability == "stable_node"], xlab = "C1", ylab = "C2")
plot(x = twopatch_ext_complete$M1[twopatch_ext_complete$stability == "stable_node"], y = twopatch_ext_complete$M2[twopatch_ext_complete$stability == "stable_node"], xlab = "M1", ylab = "M2")


#seems good? need to try plotting other ways too though
par(mfrow = c(2,5))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 1 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 1 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 1", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 1 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 2 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 2 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 2", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 2 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 3 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 3 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 3", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 3 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 4 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 4 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 4", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 4 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 5 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 5 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 5", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 5 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 6 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 6 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 6", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 6 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 7 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 7 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 7", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 7 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 8 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 8 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 8", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 8 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 9 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 9 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 9", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 9 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$ID == 10 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], y = twopatch_ext_complete$C1[twopatch_ext_complete$ID == 10 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], main = "ID 10", col = twopatch_ext_complete$Colour[twopatch_ext_complete$ID == 10 & twopatch_ext_complete$q_c == 0.01 & twopatch_ext_complete$p_c == 0.01 & twopatch_ext_complete$q_m == 0.01 & twopatch_ext_complete$p_m == 0.01], pch = 20, xlab = "grazing", ylab = "C1 cover")


nomatch <- which(warning == "no matches")
for(m in 1:length(nomatch)){
print(m)
print(twopatch_ext_complete[twopatch_ext_complete$paramcombo == nomatch[m]-1,])
print(twopatch_ext_complete[twopatch_ext_complete$paramcombo == nomatch[m],])
}
dim(twopatch_ext_complete[is.na(twopatch_ext_complete$ID),])


#none of these are NA
bifurc <- which(warning == "bifurcation")
for(m in 1:length(bifurc)){
print(m)
print(twopatch_ext_complete[twopatch_ext_complete$paramcombo == bifurc[m],])
}



