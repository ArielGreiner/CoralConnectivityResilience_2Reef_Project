library(scales)
library(deSolve)
library(geometry)
library(fields)

#load in dataset including appropriate ID values
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
#30 chosen bc never going to see that many stable equilibria, 99 = length(seq(0.01,0.99,by=0.01))
basins <- data.frame(RecruitValue = rep(seq(0.01,0.99,by=0.01),each = 30*99), Grazing = rep(seq(0.01,0.99,by=0.01), each = 30), EquilibriumID = seq(1,30,by=1), Size = 0)

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

basins$Size[basins$RecruitValue == recruitvalue & basins$Grazing == g_val & basins$EquilibriumID == twopatch_ext_complete$ID[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$M1 == M1equi[m] & twopatch_ext_complete$M2 == M2equi[m] & twopatch_ext_complete$C1 == C1equi[m] & twopatch_ext_complete$C2 == C2equi[m]]] <- 1 + basins$Size[basins$RecruitValue == recruitvalue & basins$Grazing == g_val & basins$EquilibriumID == twopatch_ext_complete$ID[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g == g_val & twopatch_ext_complete$stability == "stable_node" & twopatch_ext_complete$M1 == M1equi[m] & twopatch_ext_complete$M2 == M2equi[m] & twopatch_ext_complete$C1 == C1equi[m] & twopatch_ext_complete$C2 == C2equi[m]]]
}

}}
save(basinofattractionID, file = paste0("basinofattractionID_recr",recruitvalue,"g",g_val,"_5000.RData"))
save(basins, file = paste0("basins_updateduntil_recr",recruitvalue,"g",g_val,".RData"))
}
}
#first check if there are any that satisfy this and if there are, colour them black and re-evaluate at longer traj lengths
basinofattractionID$Colour[which(is.na(basinofattractionID$Colour))] <- "black" 

