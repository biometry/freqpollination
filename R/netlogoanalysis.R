###################################
NLQuit()
###################################

install.packages("RNetLogo")
install.packages ("reshape")
install.packages("rJava")

library (reshape)
library (RNetLogo)
nl.path <- "C:/Program Files (x86)/NetLogo 5.1.0"
NLStart(nl.path)
NLStart(nl.path, gui=FALSE)



model.path <- "C:/Users/hczioska/Documents/GitHub/freqpollination/NetLogo/bee2.nlogo"
NLLoadModel(model.path)

model.path2 <- "C:/Users/Helen/Desktop/bee2.nlogo"
NLLoadModel(model.path2)


# world: 100x100cells = 10x10m = 10m²

NLCommand("setup")

##"Default" - Settings
NLCommand ("set frequency 30")
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 5")
NLCommand ("set flower-cover 30 ")
NLCommand ("set view 5")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 65")
NLCommand ("set cluster-degree 1")


####
# das Ganze mit Replica:
rep.simf <- function(frequency, rep) 
  lapply(frequency, function(freq) replicate(rep, simf(freq))) #??

d <- seq(10, 99, 10); res <- rep.simf(d, 10)
boxplot(res, names = d, xlab = "frequency", ylab = "VR1")



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

flight.seq <- c(1,5,10) # 5
angle.seq <- c(30, 60, 180) #65
view.seq <- c(1,6,15) # 6
reward.seq <- c(0, 0.0001, 0.001 ) # 0.0004
cover.seq <- c(20, 30, 50) # 20
bee.seq <- c(1,5,10,20,30) #5
cluster.seq <- c(1)
freq.seq <- c(rep(seq(from=5,to=95,by=10), rep(10)))


test <- rep(1:4, rep(4))


all<- expand.grid(flight.seq,angle.seq,view.seq,reward.seq,cover.seq, bee.seq)
names(all) <- c("flight", "angle", "view","reward","cover", "bees")

sim.all <- function (alldata) {
  NLCommand("set flightsteps-until-change", alldata[1], "set stdev-angle", alldata[2],  "set view", alldata[3], "set reward-function", alldata[4],"set flower-cover", alldata[5], "set number-bees", alldata[6], "setup")
  NLDoCommandWhile("ticks < 500", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  output <- c(VR1, VR2)
  return ( output)
}


output3 <- apply(all,1, sim.all) 
output.frame.3<- data.frame(matrix(unlist(output3), nrow=nrow(all), byrow=T)) #list into dataframe
names(output.frame.3) <- c("VR1", "VR2") #new names
sensitive.3 <- cbind(all,output.frame.3) #both dataframes together


sensitive.4 <- read.csv("C:/Users/Helen/Desktop/output3.csv", header=T, sep= ",")
str(sensitive.4)
sensitive.4$freq1 <- 30
sensitive.4$cluster <- 1
sensitive.4[1] <- NULL
length(unique(sensitive.4$bees))

sa.all <- rbind(sa.all,sensitive.4)
str(sa.all)
length(unique(sa.all$bees))

write.csv(sa.all, "C:/Users/Helen/Desktop/sa.all.csv")

sa.all$beeVR1 <- sa.all$VR1 / sa.all$bees #visits per bee (mehr bienen = mehr visits)
sa.all$beeVR2 <- sa.all$VR2 / sa.all$bees #visits per bee (mehr bienen = mehr visits)


##############################
subset.c <- sa.all[ which(sa.all$cover == 20 & sa.all$bees == 5), ]
subset.b <- sa.all[ which(sa.all$cover == 20 & sa.all$view == 6), ]

summary(glm (VR1 ~ flight+view+angle+cover+reward, data=sa.all))
#view, cover und reward sind sig

summary(glm (VR2 ~ flight+view+angle+cover+reward, data=sa.all))
#flight und cover sind sig

summary(glm (beeVR1~ flight+view+angle+reward+bees, data=subset.b))
#view, reward und bees sind sig

summary(glm (VR2 ~ flight+view+angle+reward, data=subset.c))
#flight, view  und reward sind sig



summary(lm (VR2 ~ flight, data=subset.c))
#flight, view  und reward sind sig

abline(lm (VR1 ~ flight, data=subset.c))


boxplot(VR1 ~ flight, data=subset.c, ylab="Visitation Rate in 1000sec")
boxplot(VR2 ~ flight, data=subset.c, col="red")

plot(beeVR1 ~ flight, data=sa.all, ylab="Visitation Rate in 1000sec")
points(beeVR2 ~ flight, data=sa.all, col="red")

plot(VR1 ~ flight, data=sa.all, ylab="Visitation Rate in 1000sec")
points(VR2 ~ flight, data=sa.all, col="red")



plot(VR1 ~ view, data=subset.c, ylab="Visitation Rate in 1000sec")
points(VR2 ~ view, data=subset.c, col="red")

plot(VR1 ~ view, data=sa.all, ylab="Visitation Rate in 1000sec")
points(VR2 ~ view, data=sa.all, col="red")



plot(VR1 ~ angle, data=subset.c, ylab="Visitation Rate in 1000sec")
points(VR2 ~ angle, data=subset.c, col="red")

plot(VR1 ~ reward, data=subset.c, ylab="Visitation Rate in 1000sec" , xlim=c(0,0.005))
points(VR2 ~ reward, data=subset.c, col="red")
abline(lm(VR1~reward, data=subset.c))
abline(lm(VR2~reward, data=subset.c), col="red")

plot(VR1 ~ cover, data=sa.all, ylab="Visitation Rate in 1000sec")
points(VR2 ~ cover, data=sa.all, col="red")


par(mfrow=c(2,2))
boxplot(beeVR1 ~ bees, data=subset.b, ylab="Visitation Rate in 1000sec")
boxplot(beeVR2 ~ bees, data=subset.b, col="red")


length(unique(subset.b$bees))





########################################
#Default:
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 5")
NLCommand ("set flower-cover 10 ")
NLCommand ("set view 6")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 65")
NLCommand ("set cluster-degree 1")

freq.seq <- c(rep((seq(from=5,to=95,by=10)), each=10))

sim.freq <- function (freqtest) {
  NLCommand("set frequency", freqtest, "setup")
  NLDoCommandWhile("ticks < 2000", "go");
  VR1 <- NLReport("(sum [visit-count] of patches with [species = 1])/ count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])/ count patches with [species = 2]")
  output <- c(VR1, VR2)
  return ( output)
}


output20 <- sapply(freq.seq,function(d) sim.freq(d)) 

plot(freq.seq,output10[1,],ylab="Visitation Rate in 1000sec", xlab="frequency")

output10 <- sapply(freq.seq,function(d) sim.freq(d))

