####Data####
bees <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/bees.csv")
view <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/view.csv")
flight <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/flight.csv")
reward <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/reward.csv")

write.csv(view,"C:/Users/hczioska/Documents/GitHub/Data/SA/view.csv", row.names=F)


view$freqshare <- view$freq / 100
view$VR <- view$VR1+ view$VR2

view$PFV1 <- view$VR1 / view$F1
view$PFV2 <- view$VR2 / view$F2

flight$PFV1 <- flight$VR1 / flight$F1
flight$PFV2 <- flight$VR2 / flight$F2

reward$PFV1 <- reward$VR1 / reward$F1
reward$PFV2 <- reward$VR2 / reward$F2

bees$PFV1 <- bees$VR1 / bees$F1
bees$PFV2 <- bees$VR2 / bees$F2

#################

SA.cover.seq <- c(5,20,50)
cluster.SA<- c(1, 10, 100)
view.seq <- c(1,6,20,50)
bees.seq <- c(5,10,20,50)
flight.seq <- c(1,5,20,50)
bees.seq <- c(0,0.00004, 0.001,0.1)
SA.col <- c("cornflowerblue", "blue4", "red", "orange")
cluster.plot <- c(1,2,20,100) #for plotting
cluster.SA <- c(1,10,100) #for plotting

###################

par(mfrow=c(2,2), mar = c(4, 4, 3, 2) + 0.1)

boxplot(VR~reward, data=reward, ylab="Sum of Visits", xlab="Reward regrowth rate [Joule/sec]")
boxplot(VR~view, data=view, ylab="Sum of Visits", xlab="Vision distance [patches]")
boxplot(VR~flight, data=flight, ylab="Sum of Visits", xlab="Search limit [seconds]")
boxplot(VR~bees, data=bees, ylab="Sum of Visits", xlab="Number bee-agents")

mtext("Change in Visits", side = 3, line = -2, outer = TRUE)


#################################################################################################################################

par(mfrow=c(2,3), mar = c(2, 2, 2, 2))

for (j in 1:2) {
  plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
  for (i in 1:4) {
    sub <- flight[which(flight$cover == 5 & flight$cluster == cluster.SA[j] & flight$flight == flight.seq[i]),]
    sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
    sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
    points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
    points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l") 
  } 
  abline(0,1, lty=2)  
}

plot(1, type="n" )
legend("right", pch=20, legend=unique(flight$flight), col=SA.col, cex=2, title="flight", bty="n") 

for (j in 1:3) {
plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- flight[which(flight$cover == 20 & flight$cluster == cluster.SA[j] & flight$flight == flight.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l")
} 
abline(0,1, lty=2)
}


######################################################
#######outlier#############################################

out<- read.csv("C:/Users/hczioska/Documents/GitHub/Data/outlier.csv")

out$VR <- out$VR1+ out$VR2
out$freqshare <- out$freq / 100
out$F1[out$F1 == 0] <- 0.01
out$F2[out$F2 == 0] <- 0.01
out$PFV1 <- out$VR1/out$F1
out$PFV2 <- out$VR2/out$F2
out$P12 <- out$P1+ out$P2
out$PFP1 <- out$P1/out$F1
out$PFP2 <- out$P2/out$F2


par(mfrow=c(1,1))

plot(1, ylim=c(0,4), xlim=c(0,1),ylab="Relative Fitness", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- out[which(out$cover == cover.seq[i]),]
  sub.mean <-  tapply(sub$PFV1/sub$PFV2, sub$freqshare, mean)
  sub.sd <-  tapply(sub$PFV1/sub$PFV2, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(1,0, lty=2)
