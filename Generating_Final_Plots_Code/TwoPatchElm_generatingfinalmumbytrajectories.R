library(scales)
library(deSolve)
library(geometry)
library(fields)

#STEP 1: create grid of starting points for trajectories in the region of the [0,1] x [0,1] space allowed by 1 = M+C+T 

#x and y coords of all of the initial starting points
interval <- 0.05
x_coords <- seq(0.01,0.99, by = interval)
y_coords <- seq(0.01,0.99,by = interval)
#make dataframe 
grid <- data.frame(Point_Num=seq(1,length(x_coords)*length(y_coords),by=1), Minit = rep(x_coords,each = length(y_coords)), Cinit = NA, Tinit = NA)
#populate C column - want a lower triangle situation in an MxC graph
for(i in 1:length(x_coords)){ 
  val <- x_coords[i]
  grid$Cinit[grid$Minit == val] <- c(seq(0.01,1-val,by=interval),rep(NA,i)) #want C+M in each row to be =< 1, hence the NAs
}
#plot(grid$Minit,grid$Cinit,pch=20) #whoo it worked

#remove the NA rows
grid <- grid[-which(is.na(grid$Cinit)),] 

#populate T column
for(i in 1:dim(grid)[1]){
  grid$Tinit[i] <- 1 - grid$Minit[i] - grid$Cinit[i] #the sum of each row should be 1
  grid$Tinit[i] <- round(grid$Tinit[i],digits = 3)
}
print(paste("The first row of grid = ", grid[1,]))

set.seed(2)
#make one for reef patch 2 [need it to be a sampled version of the first one]
reord <- sample(c(1:dim(grid)[1]))
gridtwo <- data.frame(Point_Num=seq(1,dim(grid)[1],by=1), Minit = NA, Cinit = NA, Tinit = NA)
for(i in 1:dim(gridtwo)[1]){
  gridtwo[i,] <- grid[reord[i],]
}
print(paste("The first row of grid2 = ", gridtwo[1,]))

#can remove the Point_Num column, it was really just a placeholder column
grid$Point_Num <- NULL
gridtwo$Point_Num <- NULL


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


ntrajectory <- dim(grid)[1]
times <- seq(0,2000, by = 0.1)
npoints <- length(times)
initM <- grid$Minit
initC <- grid$Cinit
initT <- grid$Tinit

#can't see the full range of trajectories without starting the values in the other patch from different starting points (can't just set M1=M2, C1=C2 re: starting points)
initM2 <- gridtwo$Minit
initC2 <- gridtwo$Cinit
initT2 <- gridtwo$Tinit

#initializing the dataframes beforehand
mumbytrajectories <- data.frame(Run=rep(1:ntrajectory, each = npoints), M1 = NA, C1 = NA, T1 = NA, M2 = NA, C2 = NA, T2 = NA, TimeStep = rep(1:npoints))

CalcTrajectories <- function(i,parameters,recruitvalue, g_val, lvl, ntrajectory,times,mumbytrajectories,initM,initC,initT,initM2,initC2,initT2,MumbyOpen_Elmhirst_2PatchExt){
  for(i in 1:ntrajectory){
    #Elmhirst parameter model 
    parameters <- c(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, p_c <- recruitvalue, q_c <- recruitvalue, p_m <- round(recruitvalue*lvl,4), q_m <- round(recruitvalue*lvl,4))
    #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
    state <- c(M1 = initM[i], C1 = initC[i], Tu1 = initT[i], M2 = initM2[i], C2 = initC2[i], Tu2 = initT2[i])
    #print("In trajectory calculation function")
    out <- lsode(y = state, times = times, func = MumbyOpen_Elmhirst_2PatchExt, parms = parameters)	
    #print(paste("first row of out = ", out[1,]))
    mumbytrajectories[mumbytrajectories$Run == i,"M1"] <- out[,2]
    #print(paste("M1 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"M1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"C1"] <- out[,3]
    #print(paste("C1 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"C1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"T1"] <- out[,4]
    #print(paste("T1 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"T1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"M2"] <- out[,5]
    #print(paste("M2 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"M2"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"C2"] <- out[,6]
    #print(paste("C2 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"C2"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"T2"] <- out[,7]
    #print(paste("T2 worked and first element is",mumbytrajectories[mumbytrajectories$Run == i,"T2"][1]))
    #print(paste("First row of mumbytrajectories post-indexing", mumbytrajectories[1,]))
  }
  return(mumbytrajectories)
}
recruitvalue = c(0.1,0.1,0.1,0.1)#0.3#c(0,0.06,0.3) #c(0.06,0.3,0.1,0.1,0.1)
g_val = c(0.27,0.27, 0.27,0.27) #0.29 #0.3
lvl = c(0.05, 0.1, 0.25, 0.5) #c(1,1,1) #c(1,1,0.05,0.25,0.5)

for(j in 1:length(lvl)){
parameters <- c(a <- 0.1, d <- 0.24, g <- g_val[j], r <- 0.55, gamma <- 0.77, p_c <- recruitvalue[j], q_c <- recruitvalue[j], p_m <- round(recruitvalue[j]*lvl[j],4), q_m <- round(recruitvalue[j]*lvl[j],4))
mumbytraj <- CalcTrajectories(i,parameters,recruitvalue[j], g_val[j], lvl[j], ntrajectory,times,mumbytrajectories,initM,initC,initT,initM2,initC2,initT2,MumbyOpen_Elmhirst_2PatchExt)
mumbytrajectories <- mumbytraj

save(mumbytrajectories, file = paste0("~/Dropbox/University of Toronto/Research Related/Chapter Drafts/Bistability Connectivity Chapter/data_for_final_phaseportraits/mumbytrajectories_recr",recruitvalue[j],"g",g_val[j],"_lvl",lvl[j]*100,"_20000.RData"))


}




