# This is the skrips I used for the simulation runs (via WinSCP and the server). R can interact with NetLogo via the R-Package RNetlogo. 

library (reshape)
library (RNetLogo)
nl.path <- "/opt/netlogo-5.1.0"
NLStart(nl.path, gui=FALSE)
model.path <-"/home/helen/bee3.nlogo"
NLLoadModel(model.path)

#Default settings (important to get back to base line)
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 10")
NLCommand ("set flower-cover 10 ")
NLCommand ("set view 6")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 30")
NLCommand ("set cluster-degree 1")
NLCommand ("set pollen-reach 1")

#Parametervalues to test:
freq.seq <- c(rep(c(c(0,1,3),seq(5,100,5)), each=10))
cover.seq <- c(5,10,20,30,50)
bees.seq <- c(10)
cluster.seq <- c(1,2,5,10,20,50,75,100)
reward.seq <- c(0.00004)
view.seq <- c(6)
flight.seq <- c(5)
angle.seq <- c(30)
pollen.seq <- c(1,2,4,6,8,16)


all<- expand.grid(freq.seq, cover.seq, bees.seq,cluster.seq, reward.seq  , view.seq , flight.seq , angle.seq, pollen.seq)
names(all) <- c("freq", "cover","bees", "cluster", "reward", "view", "flight", "angle", "pollen")

print(nrow(all)) #For estimating the simulation time

sim.freq <- function (freqtest) {
  NLCommand("set frequency", freqtest[1],
            "set flower-cover", freqtest[2],
            "set number-bees", freqtest[3], 
            "set cluster-degree", freqtest[4],
            "set reward-function", freqtest[5], 
            "set view", freqtest[6],
            "set flightsteps-until-change", freqtest[7], 
            "set stdev-angle", freqtest[8],
            "set pollen-reach", freqtest[9],
            "setup")
  
  NLDoCommandWhile("ticks < 1000", "go");
  F1 <- NLReport("number-flowers 1")
  F2 <- NLReport("number-flowers 2")
  VR1 <- NLReport("visits 1")
  VR2 <- NLReport("visits 2")
  P1 <- NLReport("pollination-success 1")
  P2 <- NLReport("pollination-success 2")
  output <- c(F1,F2,VR1,VR2,P1,P2)
  return (output)
}

NLdata <- apply(all,1,sim.freq)
NLdata <- data.frame(matrix(unlist(NLdata), nrow=nrow(all), byrow=T))
names(NLdata) <- c("F1","F2", "VR1", "VR2", "P1", "P2" )
NLdata<- cbind(all,NLdata)
NLdata$invfreq <- 100 - NLdata$freq
write.csv(NLdata,"/home/helen/XXX.csv", row.names=F)

pt1 <- proc.time() #how much time in total
pt1[3] / nrow(all) #how much time per simulation run

NLQuit()
