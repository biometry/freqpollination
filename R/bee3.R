###read data####

NL<- read.csv("C:/Users/hczioska/Documents/GitHub/Data/NL.csv")

NL$VR <- NULL
NL$freqshare <- NULL

NL$VR <- NL$VR1+ NL$VR2
NL$freqshare <- NL$freq / 100
NL$F1[NL$F1 == 0] <- 0.1

write.csv(NL3,"C:/Users/hczioska/Documents/GitHub/Data/NL.csv", row.names=F)


#####basics############

par(mfrow=c(2,2))
par(mfrow=c(1,1))

cover.seq <- c( 5, 10, 20, 30, 50)
cluster.seq <- c( 1,2,5,10,20,50,75,100)
pollen.seq <- c( 1,2,4,6,8,16)
freq.seq <- c(c(c(0,1,3),seq(5,100,5)))
freq.seq.share <- c(c(c(0,1,3),seq(5,100,5))) / 100
covercol <- c("cornflowerblue", "blue4", "maroon", "red", "orange")
coverpol <- c("cornflowerblue", "blue4", "maroon", "red", "orange", "olivedrab4")
clustercol <- c("cornflowerblue", "blue4", "maroon", "red", "orange", "chocolate4", "green4"," green3")


###V1 + V2 ~ cover##########

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

###V1 + V2 ~ cluster##########

par(mfrow=c(1,2))
plot((VR1+VR2) ~ cluster, data=NL[which ( NL$bees == 10),], col=cover, ylab="Total Visits", xlab="Degree of clustering", log="x")

boxplot((VR1+VR2) ~ cluster, data=NL[which (NL$cover == 10),],  xlab="Degree of clustering", ylim=c(1000,1800),ylab="Total Visits per Simulation Run")


par(mfrow=c(1,1))
plot(1, ylim=c(850,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Degree of Clustering (Log-Scale)", main="Summed Visits per Cluster",log="x", type="n" )
legend("bottomleft", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, title="Cover (%)") 
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$pollen == 1),]
  sub.mean <-  tapply(sub$VR, sub$cluster, mean)
  sub.sd <-  tapply(sub$VR, sub$cluster, sd)
  points(sub.mean~cluster.seq, pch=20, col=covercol[i])
  points(sub.mean~cluster.seq, pch=20, col=covercol[i], type="l")
  arrows(cluster.seq, sub.mean-sub.sd, cluster.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
}


plot(1, ylim=c(850,1550), xlim=c(1,100), ylab="Sum of Pollination", xlab="Degree of Clustering (Log-Scale)", main="Summed Pollination per Cluster",log="x", type="n" ) 
#legend("bottomleft", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, title="Cover (%)") 
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$pollen == 1),]
  sub.mean <-  tapply(sub$P1 + sub$P2, sub$cluster, mean)
  sub.sd <-  tapply(sub$P1 + sub$P2, sub$cluster, sd)
  points(sub.mean~cluster.seq, pch=20, col=covercol[i])
  points(sub.mean~cluster.seq, pch=20, col=covercol[i], type="l")
  arrows(cluster.seq, sub.mean-sub.sd, cluster.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020")
}

###V1 + V2 ~ Frequency (for different cluster and cover)######
# Muster nur für cluster = 1 erkennbar, evtl auch noch für 2

par(mfrow=c(3,2))

plot(1, ylim=c(1100,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 1),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #1
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(1100,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 2),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #2
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(1000,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 10),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #10
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(1000,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 20),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #20
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(900,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 50),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #50
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(750,1650), xlim=c(1,100), ylab="Sum of Visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #100
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

mtext("Sum Visitation per Frequency", side = 3, line = -2, outer = TRUE)

####VR1/F1 ~ Freq (different cluster and cover)######

par(mfrow=c(1,4))

legend("topleft", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, title="Cover (%) ", bty="n") 
plot(1, ylim=c(0,3), xlim=c(1,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 1),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,3), xlim=c(1,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 2),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #2
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,4), xlim=c(1,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 20),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #10
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,4), xlim=c(1,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #20
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

##
plot(1, ylim=c(0,4), xlim=c(1,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 50),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
}
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,4), xlim=c(1,100), ylab="per-flower visitation rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} 
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

mtext("Per-Flower Visitation Rate per Frequency", side = 3, line = -2, outer = TRUE)

####VR1/VR ~ Freqshare (different cluster and cover)######

par(mfrow=c(3,2))

plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 1),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
legend("bottomright", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, title="Cover (%) ", bty="n") 
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 2),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 10),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 20),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 50),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1), ylab="Proportion of visits", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 

mtext("Proportion of Visits per Frequency", side = 3, line = -2, outer = TRUE)

####P1/F1~freq (different cover and cluster)####

par(mfrow=c(2,2))

plot(1, ylim=c(0,3), xlim=c(1,100), ylab="per-flower Pollination rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 1 & NL$pollen == 1),]
  sub.mean <-  tapply(sub$P1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$P1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} #1
legend("topleft", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, bty="n") 
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,3), xlim=c(1,100), ylab="per-flower Pollination rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 2 & NL$pollen == 1),]
  sub.mean <-  tapply(sub$P1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$P1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col=covercol[i]) 
} #2
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,4), xlim=c(1,100), ylab="per-flower Pollination rate", xlab="Frequency", type="n" ) 
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 10 & NL$pollen == 1),]
  sub.mean <-  tapply(sub$P1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$P1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col=covercol[i]) 
} #10
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,4), xlim=c(1,100), ylab="per-flower Pollination rate", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 20 & NL$pollen == 1),]
  sub.mean <-  tapply(sub$P1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$P1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col=covercol[i]) 
} #20
legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

mtext("Pollination Rate per Frequency (cluster 1,2,10,20)", side = 3, line = -2, outer = TRUE)

####P1:V1 Ratio (different cover, different PCO-rates, cluster= 1)####
par(mfrow=c(1,1))

plot(1, ylim=c(0,1), xlim=c(1,100), ylab="Visit:Pollination Ratio", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$bees == 10 & NL$cover == 20 & NL$cluster == 50 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
} #pollen= 1

legend("topleft", legend=unique(sub$cover), cex=0.8, bty="n") 

plot(1, ylim=c(0,1), xlim=c(1,100), ylab="Visit:Pollination Ratio", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$bees == 10 & NL$cover == 10 & NL$cluster == 1 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
} #pollen= 4
legend("topleft", legend=unique(sub$cover), cex=0.8, bty="n") 

plot(1, ylim=c(0,1), xlim=c(1,100), ylab="Visit:Pollination Ratio", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$bees == 10 & NL$cover == 20 & NL$cluster == 1 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
} #pollen= 8
legend("topleft", legend=unique(sub$cover), cex=0.8, bty="n") 

plot(1, ylim=c(0,1), xlim=c(1,100), ylab="Visit:Pollination Ratio", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$bees == 10 & NL$cover == 50 & NL$cluster == 1 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
} #pollen= 16
legend("topleft", legend=unique(sub$cover), cex=0.8, bty="n") 
legend("bottomright", pch=20, legend=unique(NL$pollen), col=coverpol, cex=0.8, title="POC", bty="n") 

mtext("Visit:Pollination Ratio (cluster=1)", side = 3, line = -2, outer = TRUE)

####P1:V1 Ratio (cover = 10, different PCO-rates, cluster= 1,2,20,50)#####
par(mfrow=c(1,3))

plot(1, ylim=c(0,1), xlim=c(1,100), ylab="", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$cover == 50 & NL$cluster == 1 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
}

plot(1, ylim=c(0,1), xlim=c(1,100), ylab="", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$cover == 50 & NL$cluster == 10 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pc16h=20, col=coverpol[i], type="l")
}


plot(1, ylim=c(0,1), xlim=c(1,100), ylab="", xlab="Frequency", type="n" )
for (i in 1:6) {
  sub <- NL[which(NL$cover == 50 & NL$cluster == 100 & NL$pollen == pollen.seq[i]),]
  sub.mean <-  tapply(sub$P1/sub$VR1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=coverpol[i])
  points(sub.mean~freq.seq, pch=20, col=coverpol[i], type="l")
}

legend("bottomright",legend=unique(NL$pollen), cex=0.8, pch= 16, col=coverpol)

####VR1/F1 ~ Freq (different cluster and cover)######

par(oma = c(1, 1, 3, 4), mar = c(1, 1, 1, 1))

par(mfrow=c(1,4))


plot(1, ylim=c(0,3), xlim=c(1,100), xlab="Frequency", ylab="",type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 1),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
}  

plot(1, ylim=c(0,3), xlim=c(1,100),ylab="", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 2),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #2

plot(1, ylim=c(0,4), xlim=c(1,100), ylab="",xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 20),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #10

plot(1, ylim=c(0,4), xlim=c(1,100),ylab="", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR1/sub$F1, sub$freq, mean)
  sub.sd <-  tapply(sub$VR1/sub$F1, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #20


par(fig = c(0, 1, 0, 1), new = TRUE)

plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", pch=20, legend=unique(NL$cover), col=covercol, title="Cover (%) ", bty="n", cex = 2)
mtext("Per-Flower Visitation Rate per Frequency", side = 3, line = -2, outer = TRUE)
#####################################

par(mfrow=c(1,4),mar = c(5, 2, 3, 2), oma=c(0,0,0,0))

plot(1, ylim=c(0,1), xlim=c(0,1),ylab="", xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 1),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
#legend("bottomright", pch=20, legend=unique(NL$cover), col=covercol, cex=0.8, title="Cover (%) ", bty="n") 
#legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1),ylab="",xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 2),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
#legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


plot(1, ylim=c(0,1), xlim=c(0,1),ylab="",xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 20),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
#legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(0,1), xlim=c(0,1),ylab="",xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR1/sub$VR, sub$freqshare, mean)
  sub.sd <-  tapply(sub$VR1/sub$VR, sub$freqshare, sd)
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i])
  points(sub.mean~freq.seq.share, pch=20, col=covercol[i], type="l")
  #arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, col="#00000020") 
} 
abline(0,1, lty=2)
#legend("topleft", legend=unique(sub$cluster), cex=0.8, bty="n") 


##############
###V1 + V2 ~ Frequency (for different cluster and cover)######
# Muster nur für cluster = 1 erkennbar, evtl auch noch für 2

par(mfrow=c(1,4),mar = c(5, 2, 3, 2))

plot(1, ylim=c(900,1650), xlim=c(1,100), xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 1),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #1
#legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(900,1650), xlim=c(1,100), xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 2),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #2
#legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(900,1650), xlim=c(1,100), xlab="Frequency", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 20),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #10
#legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 

plot(1, ylim=c(900,1650), xlim=c(1,100), xlab="Frequency", ylab="", type="n" )
for (i in 1:5) {
  sub <- NL[which(NL$bees == 10 & NL$cover == cover.seq[i] & NL$cluster == 100),]
  sub.mean <-  tapply(sub$VR, sub$freq, mean)
  sub.sd <-  tapply(sub$VR, sub$freq, sd)
  points(sub.mean~freq.seq, pch=20, col=covercol[i])
  points(sub.mean~freq.seq, pch=20, col=covercol[i], type="l")
  arrows(freq.seq, sub.mean-sub.sd, freq.seq, sub.mean+sub.sd, length=0.03, angle=90, code=3, "#00000020") 
} #20
#legend("topright", legend=unique(sub$cluster), cex=0.8, bty="n") 
