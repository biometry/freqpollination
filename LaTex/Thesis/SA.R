bees <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/bees.csv")
view <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/view.csv")
flight <- read.csv("C:/Users/hczioska/Documents/GitHub/Data/SA/flight.csv")

flight$freqshare <- flight$freq / 100
flight$VR <- flight$VR1+ flight$VR2


SA.cover.seq <- c(5,20,50)
SA.cluster.seq <- c(1, 10, 100)
view.seq <- c(1,6,20,50)
bees.seq <- c(5,10,20,50)
flight.seq <- c(1,5,20,50)

#####
#view
#####

plot(VR1/F1 ~ view, data=view, col=cover)

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:4) {
  sub <- view[which(view$cover == 20 & view$cluster == 100 & view$view == view.seq[i]),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
#legend("bottomright", pch=20, legend=unique(view$view), col=covercol, cex=0.8, title="View", bty="n") 
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 
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
  arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
legend("bottomright", pch=20, legend=unique(bees$bees), col=covercol, cex=0.8, title="bees", bty="n") 
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 
abline(0,1, lty=2)


plot(1, ylim=c(0,5), xlim=c(0,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:3) {
  sub <- bees[which(bees$cover == SA.cover.seq[i] & bees$cluster == 1 & bees$bees == 10),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
legend("bottomright", pch=20, legend=unique(bees$cover), col=covercol, cex=0.8, title="cover", bty="n") 
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 



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