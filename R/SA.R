bees <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/bees.csv")
view <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/view.csv")
flight <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/flight.csv")
bees <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/bees.csv")


bees$freqshare <- bees$freq / 100
bees$VR <- bees$VR1+ bees$VR2


SA.cover.seq <- c(5,20,50)
SA.cluster.seq <- c(1, 10, 100)
view.seq <- c(1,6,20,50)
bees.seq <- c(5,10,20,50)
flight.seq <- c(1,5,20,50)
bees.seq <- c(0,0.00004, 0.001,0.1)
SA.col <- c("cornflowerblue", "blue4", "red", "orange")


par(mfrow=c(2,2))
par(mfrow=c(2,2), mar = c(4, 4, 3, 2) + 0.1)
boxplot(VR~reward, data=reward, ylab="Sum Visits", xlab="Reward regrowth rate (Joule/sec)")
boxplot(VR~view, data=view, ylab="Sum Visits", xlab="Vision distance (in patches)")
boxplot(VR~flight, data=flight, ylab="Sum Visits", xlab="Search limit (in seconds)")
boxplot(VR~bees, data=bees, ylab="Sum Visits", xlab="Number bee-agents")
plot(VR/bees~bees, data=bees, ylab="Sum Visits", xlab="Number bee-agents", col=cover)
     

mtext("Change in Visits", side = 3, line = -2, outer = TRUE)


par(mfrow=c(1,1))
plot(1, ylim=c(400,1700), xlim=c(0,50), ylab="Total Visits", xlab="Vision distance (in patches)", type="n" )
for (i in 1:4) {
  sub <- view[which(view$cover == SA.cover.seq[i]),]
  sub.mean <-  tapply(sub$VR, sub$view, mean)
  sub.sd <-  tapply(sub$VR, sub$view, sd)
  points(sub.mean~view.seq, pch=20, col=SA.col[i])
  points(sub.mean~view.seq, pch=20, col=SA.col[i], type="l")
  arrows(view.seq, sub.mean-sub.sd, view.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
legend("bottomright", pch=20, legend=unique(view$cover), col=covercol, cex=0.8, title="Cover", bty="n") 





#####
#view
#####

par(mfrow=c(2,3), mar = c(2, 2, 2, 2))

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 5 & bees$cluster == 1 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l") 
} 
abline(0,1, lty=2)

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 5 & bees$cluster == 10 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l")
} 
abline(0,1, lty=2)

plot(1, type="n" )
legend("right", pch=20, legend=unique(bees$bees), col=SA.col, cex=2, title="bees", bty="n") 

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 20 & bees$cluster == 1 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l")
} 
abline(0,1, lty=2)

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 20 & bees$cluster == 10 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l")
} 
abline(0,1, lty=2)


plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 20 & bees$cluster == 100 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i])
  points(sub.mean~freq.seq.share, pch=20, col=SA.col[i], type="l")
} 
abline(0,1, lty=2)

#####
#bees
#####

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 20 & bees$cluster == 100 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
} 
legend("bottomright", pch=20, legend=unique(bees$bees), col=covercol, cex=0.8, title="bees", bty="n") 
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 
abline(0,1, lty=2)


#####
#flight
#####

par(mfrow=c(1,1))
plot(VR1/F1 ~ view, data=view, col=cover)

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- flight[which(flight$cover == 20 & flight$cluster == 100 & flight$flight == flight.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
#legend("bottomright", pch=20, legend=unique(flight$flight), col=covercol, cex=0.8, title="flight", bty="n") 
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 
abline(0,1, lty=2)


#####
#bees
#####

par(mfrow=c(1,1))
boxplot(VR1/F1 ~ bees, data=bees, col=cover)

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- bees[which(bees$cover == 20 & bees$cluster == 100 & bees$bees == bees.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
#legend("bottomright", pch=20, legend=unique(bees$bees), col=covercol, cex=0.8, title="bees", bty="n") 
#legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 
abline(0,1, lty=2)




#####
#reward VR SUM
#####

par(mfrow=c(1,1))
boxplot(VR1/F1 ~ bees, data=bees, col=cover)


par(mfrow=c(2,2))

plot(1, ylim=c(800,2000), xlim=c(0,100), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- reward[which(reward$cover == 50 & reward$cluster == 1 & reward$reward == reward.seq[i]),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
legend("bottomright", pch=20, legend=unique(reward$reward), col=covercol, cex=0.8, title="reward", bty="n") 
