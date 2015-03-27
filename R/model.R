###read data####

NL<- read.csv("C:/Users/hczioska/Documents/GitHub/Data/NL.csv")

NL$VR <- NL$VR1+ NL$VR2
NL$freqshare <- NL$freq / 100
NL$F1[NL$F1 == 0] <- 0.01 
NL$F2[NL$F2 == 0] <- 0.01 # Number of flowers is the demoninator when calculating the per-flower visitation rate. Can´t devide through zero
NL$PFV1 <- NL$VR1/NL$F1
NL$PFV2 <- NL$VR2/NL$F2

NL$P12 <- NL$P1+ NL$P2

NL$PFP1 <- NL$P1/NL$F1
NL$PFP2 <- NL$P2/NL$F2


# F1: Number of flowers of species A
# F2: Number of flowers of species B
# VR1: Sum of visits to all flowers of species A
# VR2: Sum of visits to all flowers of species B
# P1: Sum of successful pollinations to all flowers of species A
# P2: Sum of successful pollinations to all flowers of species B
# PFV: Per-flower visitation rate

#####basics############

par(mfrow=c(2,2))
par(mfrow=c(1,1))

cover.seq <- c( 5, 10, 20, 30, 50)
cluster.seq <- c( 1,2,5,10,20,50,75,100)
pollen.seq <- c( 1,2,4,6,8,16)
freq.seq <- c(c(c(0,1,3),seq(5,100,5)))
freq.seq.share <- c(c(c(0,1,3),seq(5,100,5))) / 100
cluster.plot <- c(1,2,20,100) #for plotting
cluster.SA <- c(1,10,100) #for plotting

covercol <- c("cornflowerblue", "blue4", "maroon", "red", "orange")
coverpol <- c("cornflowerblue", "blue4", "maroon", "red", "orange", "olivedrab4")
clustercol <- c("cornflowerblue", "blue4", "maroon", "red", "orange", "chocolate4", "green4"," green3")


###V1 + V2 ~ cover / cluster (non freq)##########

par(mfrow=c(1,2))

plot(1, ylim=c(900,1650), xlim=c(1,50), ylab="Sum of Visits", xlab="Cover (%)", main="Summed Visits per Cover",type="n" )
legend("bottomright", pch=20, legend=unique(NL$cluster), col=clustercol, cex=0.8, title="Cluster") 
for (i in 1:8) {
  sub <- NL[which(NL$bees == 10 & NL$cluster == cluster.seq[i] & NL$pollen == 1),]
  sub.mean <-  tapply(sub$VR, sub$cover, mean)
  sub.sd <-  tapply(sub$VR, sub$cover, sd)
  points(sub.mean~cover.seq, pch=20, col=clustercol[i])
  points(sub.mean~cover.seq, pch=20, col=clustercol[i], type="l")
  arrows(cover.seq, sub.mean-sub.sd, cover.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
}

plot(1, ylim=c(900,1650), xlim=c(1,100), log="x", ylab="Sum of Visits", xlab="Cluster (Log-Scale)", main="Summed Visits per Cluster",type="n", )
legend("bottomleft", pch=20, legend=unique(NL$cover), col=clustercol, cex=0.8, title="Cover (%)") 
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$pollen == 1),]
  sub.mean <-  tapply(sub$VR, sub$cluster, mean)
  sub.sd <-  tapply(sub$VR, sub$cluster, sd)
  points(sub.mean~cluster.seq, pch=20, col=covercol[i])
  points(sub.mean~cluster.seq, pch=20, col=covercol[i], type="l")
  arrows(cluster.seq, sub.mean-sub.sd, cluster.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
}


###V1 + V2 ~ Frequency (for different cluster and cover)######

par(mfrow=c(1,4),mar = c(5, 2, 3, 2))

for (j in 1:4){
plot(1, ylim=c(900,1650), xlim=c(0,1), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
  sub.mean <-  tapply(sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR, sub$freqshare, sd)
  
  arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.02, lwd=0.9,angle=90, code=3, "gray83") 
} 
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
  sub.mean <-  tapply(sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
}
}

mtext("Sum Visitation per Frequency", side = 3, line = -2, outer = TRUE)


####PFV######

par(mfrow=c(1,4),mar = c(5, 2, 3, 2))

#legend("topleft", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, title="Cover (%) ", bty="n") 

for (j in 1:4){
plot(1, ylim=c(0,4), xlim=c(0,1), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freqshare, sd)
  arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.02, lwd= 0.9, angle=90, code=3, col="gray83") 
} 
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
} 
}

mtext("Per-Flower Visitation Rate per Frequency", side = 3, line = -2, outer = TRUE)

#####relative PFV###########----

par(mfrow=c(1,4),mar = c(5, 2, 3, 2))

for (j in 1:4){
  plot(1, ylim=c(0,5), xlim=c(0,1),ylab="PFV1/PFV2", xlab="Frequency", type="n" )
  for (i in 1:5) {
    sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
    sub.mean <-  tapply(sub$PFV1/sub$PFV2, sub$freqshare, mean)
    sub.sd <-  tapply(sub$PFV1/sub$PFV2, sub$freqshare, sd)
    arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.02, angle=90, code=3, lwd=0.9, col="gray83") 
  } 
  for (i in 1:5) {
    sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
    sub.mean <-  tapply(sub$PFV1/sub$PFV2, sub$freqshare, mean)
    sub.sd <-  tapply(sub$PFV1/sub$PFV2, sub$freqshare, sd)
    points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
    points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  } 
  abline(1,0, lty=2)
}



####P1/F1~freq (different cover and cluster)####

par(mfrow=c(2,2))

for (j in 1:4){
  plot(1, ylim=c(0,4), xlim=c(0,1), ylab="per-flower pollination rate", xlab="Frequency", type="n" )
  for (i in 1:5) {
    sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == cluster.plot[j]),]
    sub.mean <-  tapply(sub$P1/sub$F1, sub$freqshare, mean)
    sub.sd <-  tapply(sub$P1/sub$F1, sub$freqshare, sd)
    points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
    points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
    arrows(freq.seq.share, sub.mean-sub.sd, freq.seq.share, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000030") 
  } 
}

mtext("Pollination Rate per Frequency (cluster 1,2,10,20)", side = 3, line = -2, outer = TRUE)


####P1:V1 Ratio####----

POC.cover <- c(5,20,50)

par(mfrow=c(3,3), mar = c(2, 2, 2, 2))
for (k in 1:3){
  for (j in 1:3){
    plot(1, ylim=c(0,1), xlim=c(1,100), type="n" )
    for (i in 1:6) {
      sub <- NL[which(NL$cover == POC.cover[k] & NL$cluster == cluster.SA[j] & NL$pollen == pollen.seq[i]),]
      sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
      sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
      points(sub.mean~freq.seq, pch=20, col=coverpol[i])
      points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
    } 
  } 
}

legend("bottomright", pch=20, legend=unique(NL$pollen), col=coverpol, cex=0.8, title="POC", bty="n") 

mtext("Visit:Pollination Ratio", side = 3, line = -2, outer = TRUE)


