install.packages("RNetLogo")
install.packages ("reshape")
library (reshape)
library (RNetLogo)
nl.path <- "C:/Program Files (x86)/NetLogo 5.1.0"
NLStart(nl.path)
NLStart(nl.path, gui=FALSE)

#####
#fire-test
#####
model.fire <-  file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")
NLLoadModel(file.path(nl.path, model.fire))
NLCommand ("set density 77")
NLCommand("setup")
NLCommand("go")          # just one tick
NLDoCommand(10, "go")    # 10 ticks
NLCommand("print \"Hello NetLogo, I called you from R.\"")
NLReport("ticks")

burned <- NLDoReportWhile("any? turtles", "go", c("ticks", "(burned-trees / initial-trees) * 100"), as.data.frame = TRUE, df.col.names = c("tick", "percent burned"))
plot(burned, type = "s")

#####################################
###Exploratory Analysis

NLCommand("setup")
sim <- function(density) {
  NLCommand("set density ", density, "setup")
  NLDoCommandWhile("any? turtles", "go");
  ret <- NLReport("(burned-trees / initial-trees) * 100")
  return(ret)
  }

d <- seq(1,100,1)
pb <- sapply(d, function(dens) sim(dens))
plot(d, pb, xlab="density", ylab="percent burned")

d <- seq(1,100,5)
pb <- sapply(d, function(density) sim(density))
plot(d, pb, xlab="density", ylab="percent burned")


###################################
NLQuit()

model.path <- "C:/Users/hczioska/Documents/GitHub/freqpollination/NetLogo/bee2.nlogo"
NLLoadModel(model.path)
NLCommand("setup")
NLCommand ("set frequency 30")
NLDoCommand(10, "go")    # 10 ticks
NLReport("ticks")

######################
visits <- NLDoReportWhile("ticks < 1000", "go", c("ticks", 
                          "(sum [visit-count] of patches with [species = 1]) / count patches with [species = 1]",
                          "(sum [visit-count] of patches with [species = 2]) / count patches with [species = 2]"),
                          as.data.frame = TRUE, df.col.names = c("ticks", "VR1" , "VR2"))

plot(visits$VR1 ~ visits$tick,  type = "s")
points(visits$VR2 ~ visits$tick, col="red" ,  type = "s")
legend ("topleft", c("VR1 (30% freq)", "VR2 (70% freq)"), lty=1, col= c("black", "red"), cex=.75)
#######################


####
# das Ganze mit Replica:
rep.simf <- function(frequency, rep) 
  lapply(frequency, function(freq) replicate(rep, simf(freq))) #??

d <- seq(10, 99, 10); res <- rep.simf(d, 10)
boxplot(res, names = d, xlab = "frequency", ylab = "VR1")


###############################################
##mit VR1 und VR2

# world: 100x100cells = 10x10m = 10m²

LCommand("setup")

##"Default" - Settings
NLCommand ("set frequency 30")
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 5")
NLCommand ("set flower-cover 30 ")
NLCommand ("set view 5")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 65")


#####
#flower_cover
#####

sim.cover <- function(cover) {
  NLCommand("set flower-cover", cover, "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  return ( c ( VR1,VR2 ))
}

cover.seq <- c(1,1, 10,10,30,30,50,50,70,70,90,90,99)
cover.y <- sapply(cover.seq, function(cov) sim.cover(cov))

cover.data <- data.frame(cover.seq,cover.y[1,],cover.y[2,])
colnames(cover.data) <- c("Cover", "VR1","VR2")

plot(VR1~Cover,data=cover.data, ylim=c(0,2.5),  ylab="Visitation Rate in 1000sec", xlab="% cover")
points(VR2~Cover,data=cover.data, col="red")
legend ("topright", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)

cover.model1 <- lm(VR1~Cover,data=cover.data,) #0.003876 ** 
cover.model2 <- lm(VR2~Cover,data=cover.data) #0.005039 ** 

cnew <- range(cover.data$Cover)
lines(cnew, predict(cover.model1, newdata =data.frame(Cover=cnew)), col= "black")
lines(cseq, predict(cover.model2,list(Cover=cseq), type="response"), col= "red")

#####
#view
#####

sim.view <- function(view) {
  NLCommand("set view", view, "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  return ( c ( VR1,VR2 ))
}

# mit diesen frequency-werten soll die Simulation laufen:
view.seq <- c(1,1,2,2,4,4,6,6,8,8,10,10,20,20,50)
view.y <- sapply(view.seq, function(vie) sim.view(vie))
plot(view.seq,view.y[1,], ylab="Visitation Rate in 1000sec", xlab="view (in 10cm)")
points(view.seq,view.y[2,], col="red")
legend ("topright", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)

summary(glm(view.y[1,]~view.seq))
summary(glm(view.y[2,]~view.seq))


#####
#reward
#####

sim.reward <- function(reward) {
  NLCommand("set reward-function", reward, "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  return ( c ( VR1,VR2 ))
}

# mit diesen frequency-werten soll die Simulation laufen:
reward.seq <- c(0, 0.000025, 0.000025, 0.00007, 0.00007, 0.00004, 0.00004, 0.0001 ,0.0001, 0.01)
reward.y <- sapply(reward.seq, function(rew) sim.reward(rew))
plot(reward.seq,reward.y[1,], ylim=c(0.4,0.8),ylab="Visitation Rate in 1000sec", xlab="Reward gain per Tick")
points(reward.seq,reward.y[2,], col="red")
legend ("topright", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)


#ohne den letzten:
plot(reward.seq,reward.y[1,],xlim=c(0,0.0001), ylim=c(0.5,0.8), ylab="Visitation Rate in 1000sec", xlab="Reward gain per Tick")
points(reward.seq,reward.y[2,], col="red")
legend ("topright", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)


reward.model1 <- glm(reward.y[1,]~reward.seq) #0.0218 *  
reward.model2 <- glm(reward.y[2,]~reward.seq) #0.677

cseq <- seq(0,100.1)
lines(cseq, predict(reward.model1,list(reward.seq=cseq), type="response"), col= "black")
lines(cseq, predict(reward.model2,list(reward.seq=cseq), type="response"), col= "red")


#####
#number bees
#####

sim.bee <- function(bee) {
  NLCommand("set number-bees", bee, "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  return ( c ( VR1,VR2 ))
}

# mit diesen frequency-werten soll die Simulation laufen:
bee.seq <- c(1,1,3,3,5,5,7,7,10,10,15,20,25,50,50)
bee.y <- sapply(bee.seq, function(be) sim.bee(be))
plot(bee.seq,bee.y[1,]/bee.seq,  ylim=c(0,0.2), ylab="Visitation Rate PER BEE in 1000sec", xlab="Number bees")
points(bee.seq,bee.y[2,]/bee.seq, col="red")
legend ("topleft", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)

#####
#stdev
#####

sim.dev <- function(dev) {
  NLCommand("set stdev-angle", dev, "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  return ( c ( VR1,VR2 ))
}


dev.seq <- c(0,10,30,30,60,60,180,180,270,270,360)
dev.y <- sapply(dev.seq, function(steps) sim.dev(steps))
plot(dev.seq,dev.y[1,],  ylab="Visitation Rate in 1000sec", xlab="stdev-angle for CRW")
points(dev.seq,dev.y[2,], col="red")
legend ("topleft", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)



#####
#flightsteps-until-change
#####

sim.flight <- function(flight) {
  NLCommand("set flightsteps-until-change", flight, "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  return ( c ( VR1,VR2 ))
}


flight.seq <- c(1,2,2,4,4,5,6,6,8,8,10,10,20,20,50)
flight.y <- sapply(flight.seq, function(steps) sim.flight(steps))

plot(flight.seq,flight.y[1,], ylim=c(0.4,1), ylab="Visitation Rate in 1000sec", xlab="flightsteps-until-change")
points(flight.seq,flight.y[2,], col="red")
legend ("topleft", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)


#without last:
plot(flight.seq,flight.y[1,], ylim=c(0.4,1), xlim=c(0,21), ylab="Visitation Rate in 1000sec", xlab="flightsteps-until-change")
points(flight.seq,flight.y[2,], col="red")
legend ("topleft", c("VR1 (30%) ", "VR2 (70%)"), pch=15, col= c("black", "red"), cex=.75)


######
#Alle Plots zusammen:
par(mfrow=c(3, 2))
par(mfrow=c(1, 1))





###################

flight.seq <- c(1,3,5,7,10,15,20) # 5
angle.seq <- c(0,30,60,180,270,360) #65
view.seq <- c(1,3,5,7,10,15,20) # 6
reward.seq <- c(0, 0.00001, 0.00005, 0.0001 ,0.0005, 0.005) # 0.0004
cover.seq <- c(10,0,90,) # 20


all<- expand.grid(flight.seq,angle.seq,view.seq,reward.seq,cover.seq)
names(all) <- c("flight", "angle", "view","reward","cover")

sim.all <- function (alldata) {
  NLCommand("set stdev-angle", alldata[2], "set flightsteps-until-change", alldata[1], "set view", alldata[3], "setup")
  NLDoCommandWhile("ticks < 500", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  output <- c(VR1, VR2)
  return ( output)
}

output <- apply(all,1, sim.all) 
output<- data.frame(matrix(unlist(output), nrow=448, byrow=T)) #list into dataframe
names(output) <- c("VR1", "VR2") #new names
sa1 <- cbind(all,output) #both dataframes together

##############################

plot(VR1 ~ flight, data=sa1, ylab="Visitation Rate in 1000sec")
points(VR2 ~ flight, data=sa1, col="red")

plot(VR1 ~ view, data=sa1, ylab="Visitation Rate in 1000sec")
points(VR2 ~ view, data=sa1, col="red")

plot(VR1 ~ angle, data=sa1, ylab="Visitation Rate in 1000sec")
points(VR2 ~ angle, data=sa1, col="red")



subset1 <- sa1[ which(sa1$flight == 5 & sa1$angle == 60), ]

plot(VR1 ~ view, data=subset1, ylab="Visitation Rate in 1000sec")
points(VR2 ~ view, data=subset1, col="red")
