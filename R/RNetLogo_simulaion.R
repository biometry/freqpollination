library (reshape)
library (RNetLogo)
nl.path <- "/opt/netlogo-5.1.0"
NLStart(nl.path, gui=FALSE)
model.path <- "/home/helen/bee2.nlogo"
NLLoadModel(model.path)

#Default:
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 10")
NLCommand ("set flower-cover 10 ")
NLCommand ("set view 6")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 30")
NLCommand ("set cluster-degree 1")


freq.seq <- c(rep((seq(from=5,to=95,by=5)), each=10))
cover.seq <- c(10)
bees.seq <- c(10)
cluster.seq <- c(1)
reward.seq <- c(0.00004)
view.seq <- c(6)
flight.seq <- c(5)
angle.seq <- c(30)

all<- expand.grid(freq.seq, cover.seq, bees.seq,cluster.seq, reward.seq  , view.seq , flight.seq , angle.seq)
names(all) <- c("freq", "cover","bees", "cluster", "reward", "view", "flight", "angle")

sim.freq1000 <- function (freqtest) {
  NLCommand("set frequency", freqtest[1],
            "set flower-cover", freqtest[2],
            "set number-bees", freqtest[3], 
            "set cluster-degree", freqtest[4],
            
            "set reward-function", freqtest[5], 
            "set view", freqtest[6],
            "set flightsteps-until-change", freqtest[7], 
            "set stdev-angle", freqtest[8],
            "setup")
  
  NLDoCommandWhile("ticks < 1000", "go");
  F1 <- NLReport("number_flowers 1")
  F2 <- NLReport("number_flowers 2")
  VR1 <- NLReport("visits 1")
  VR2 <- NLReport("visits 2")
  P1 <- NLReport("pollination-success 1")
  P2 <- NLReport("pollination-success 2")
  output <- c(F1,F2,VR1,VR2,P1,P2)
  return (output)
}

NLdata <- apply(all,1,sim.freq1000)
#get the data into a dataframe and name it
NLdata<- data.frame(matrix(unlist(NLdata), nrow=nrow(all), byrow=T)) #list into dataframe
names(NLdata) <- c("F1","F2", "VR1", "VR2", "P1", "P2" ) #new names
NLdata <- cbind(all,NLdata) #both dataframes together
NLdata$invfreq <- 100 - NLdatafreq
write.csv(NLdata, "/home/helen/NLdata1000.csv", row.names=F)



sim.freq500 <- function (freqtest) {
  NLCommand("set frequency", freqtest[1],
            "set flower-cover", freqtest[2],
            "set number-bees", freqtest[3], 
            "set cluster-degree", freqtest[4],
            
            "set reward-function", freqtest[5], 
            "set view", freqtest[6],
            "set flightsteps-until-change", freqtest[7], 
            "set stdev-angle", freqtest[8],
            "setup")
  
  NLDoCommandWhile("ticks < 500", "go");
  F1 <- NLReport("number_flowers 1")
  F2 <- NLReport("number_flowers 2")
  VR1 <- NLReport("visits 1")
  VR2 <- NLReport("visits 2")
  P1 <- NLReport("pollination-success 1")
  P2 <- NLReport("pollination-success 2")
  output <- c(F1,F2,VR1,VR2,P1,P2)
  return (output)
}


NLdata <- apply(all,1,sim.freq500)
#get the data into a dataframe and name it
NLdata<- data.frame(matrix(unlist(NLdata), nrow=nrow(all), byrow=T)) #list into dataframe
names(NLdata) <- c("F1","F2", "VR1", "VR2", "P1", "P2" ) #new names
NLdata <- cbind(all,NLdata) #both dataframes together
NLdata$invfreq <- 100 - NLdatafreq

write.csv(NLdata,"/home/helen/NLdata500.csv", row.names=F)

NLQuit()