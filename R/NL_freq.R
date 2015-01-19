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


##########different degrees of clustering with all other on default##############################

#Default:
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 10")
NLCommand ("set flower-cover 10 ")
NLCommand ("set view 6")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 30")


cover.seq <- c(10)
bees.seq <- c(10)
freq.seq <- c(rep((seq(from=5,to=95,by=10)), each=10))
cluster.seq <- c(1,2,6)

all<- expand.grid(cover.seq, bees.seq, freq.seq, cluster.seq)
names(all) <- c("cover", "bees", "freq", "cluster")


sim.freq <- function (freqtest) {
  NLCommand("set flower-cover", freqtest[1],"set number-bees", freqtest[2], "set frequency", freqtest[3], "set cluster-degree", freqtest[4], "setup")
  NLDoCommandWhile("ticks < 1000", "go");
  VR1 <- NLReport("sum [visit-count] of patches with [species = 1]")
  F1 <- NLReport("count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])")
  F2 <- NLReport("count patches with [species = 2]")
  output <- c(VR1, F1, VR2, F2)
  return (output)
}


c10b10cluster.raw <- apply(all,1,sim.freq)


#get the data into a dataframe and combine with previous data:
c10b10cluster.dfraw<- data.frame(matrix(unlist(c10b10cluster.raw), nrow=nrow(all), byrow=T)) #list into dataframe
names(c10b10cluster.dfraw) <- c("VR1", "F1","VR2","F2" ) #new names
c10b10cluster.dfraw <- cbind(all,c10b10cluster.dfraw) #both dataframes together
c10b10cluster.dfraw$invfreq <- 100 - c10b10cluster.dfraw$freq
c10b10cluster <- rbind(c10b10cluster.dfraw, c10b10cluster)


write.csv(c10b10cluster, "C:/Users/Helen/Desktop/c10b10cluster.df.csv")



#########auswertung TOTAL visits##########################

par(mfrow=c(1,1))
plot((VR1+VR2) ~ cluster, data=c10b10cluster,  ylab="Total Visits", xlab="Degree of clustering")
boxplot((VR1+VR2) ~ cluster, data=c10b10cluster,  xlab="Degree of clustering", ylab="Total Visits per Simulation Run")
# interessant: die Zahl der Visits geht hoch mit degree of clustering!

# subset:
cluster.f15 <- c10b10cluster[ which(c10b10cluster$freq =='15'),]

# hier: nur für eine frequency: Selbes Muster egal welche frrequency
par(mfrow=c(1,3))
plot((VR1+VR2) ~ cluster, data=cluster.f15, main="15% Frequency")
plot((VR1+VR2) ~ cluster, data=cluster.f35, main="35% Frequency")
plot((VR1+VR2) ~ cluster, data=cluster.f55, main="55% Frequency")



############TOTAL Visits per Frequency#####################################

par(mfrow=c(1,1))

#subset:
cluster1 <- c10b10cluster[ which(c10b10cluster$cluster =='1'),]
cluster2 <- c10b10cluster[ which(c10b10cluster$cluster =='2'),]
cluster6 <- c10b10cluster[ which(c10b10cluster$cluster =='6'),]
cluster20 <- c10b10cluster[ which(c10b10cluster$cluster =='20'),]



par(mfrow=c(2,2))

boxplot((VR1+VR2) ~freq, data= cluster1, main= "cluster 1", ylab="Total Visits", xlab="Frequency" )
# U-Shape. Am wenigsten besuche bei ausgeglichener Frequency
boxplot((VR1+VR2) ~freq, data= cluster2, main= "cluster 2", ylab="Total Visits", xlab="Frequency" )
# auch wieder U-Shape. Am wenigsten besuche bei ausgeglichener Frequency
boxplot((VR1+VR2) ~freq, data= cluster6, main= "cluster 6", ylab="Total Visits", xlab="Frequency" )
# kein muster mehr!
boxplot((VR1+VR2) ~freq, data= cluster20, main= "cluster 20", ylab="Total Visits", xlab="Frequency" )
# kein muster mehr!

#################################################################Visitation-Rate!####################################(noch nicht mit drin, dass sich die insgesammte VR ändert als U-Shape)

par(mfrow=c(2,2))

plot((VR1/F1) ~freq, data= cluster1, main= "cluster 1", xlab="frequency", ylab="per-flower visitation rate")
points((VR2/F2) ~ invfreq, data= cluster1, col="grey15") 
#sehr interessantes Muster! Hohe variabilität am Anfang weil es wenig Blüten sind und jeder Besuch einen großen Einfluss hat auf die VR

plot((VR1/F1) ~freq, data= cluster2, main= "cluster 2", xlab="frequency", ylab="per-flower visitation rate")
points((VR2/F2) ~ invfreq, data= cluster2, col="grey20") # ähnliches Muster wie bei cluster = 1

plot((VR1/F1) ~freq, data= cluster6, main= "cluster 6", xlab="frequency", ylab="per-flower visitation rate")
points((VR2/F2) ~ invfreq, data= cluster6, col="grey20")# ähnliches Muster wie bei cluster = 1

plot((VR1/F1) ~freq, data= cluster20, main= "cluster 20", xlab="frequency", ylab="per-flower visitation rate")
points((VR2/F2) ~ invfreq, data= cluster20, col="grey20") # Muster ein bisschen anders



#################################################################Visitation-Rate als Anteil####################################

c10b10cluster$percentfreq <- c10b10cluster$freq/100
c10b10cluster$percentinvfreq <- c10b10cluster$invfreq/100

par(mfrow=c(2,2))

plot((VR1/(VR1+VR2)) ~(percentfreq), data= cluster1, main= "cluster 1", xlab="frequency",ylab="Share of Visits")
points(VR2/(VR1+VR2) ~ percentinvfreq, data= cluster1, col="grey20") 
abline(a=0, b=1)

plot((VR1/(VR1+VR2)) ~(percentfreq), data= cluster1, main= "cluster 2", xlab="frequency",ylab="Share of Visits")
points(VR2/(VR1+VR2) ~ percentinvfreq, data= cluster2, col="grey20") 
abline(a=0, b=1)

plot((VR1/(VR1+VR2)) ~(percentfreq), data= cluster1, main= "cluster 6", xlab="frequency",ylab="Share of Visits")
points(VR2/(VR1+VR2) ~ percentinvfreq, data= cluster6, col="grey20") 
abline(a=0, b=1)

plot((VR1/(VR1+VR2)) ~(percentfreq), data= cluster1, main= "cluster 20", xlab="frequency",ylab="Share of Visits")
points(VR2/(VR1+VR2) ~ percentinvfreq, data= cluster20, col="grey20") 
abline(a=0, b=1)


###########################################
#########per-flower########################


par(mfrow=c(2,2))

plot(((VR1/(VR1+VR2))/F1) ~ freq, data= cluster1, main= "cluster 1", ylim=c(0,0.003), ylab="Per-Flower VR", xlab="frequency")
points(((VR2/(VR1+VR2))/F2) ~ invfreq, data= cluster1, col="grey20") 
#ist ein bisschen enger gefasst aber gleiches Muster



plot(((VR1/(VR1+VR2))/F1) ~ freq, data= cluster1, main= "cluster 1", ylim=c(0,0.003), ylab="Per-Flower VR", xlab="frequency")
points(((VR2/(VR1+VR2))/F2) ~ invfreq, data= cluster1, col="grey20") 
#ist ein bisschen enger gefasst aber gleiches Muster






plot(((VR1/(VR1+VR2))/F1) ~freq, data= cluster2, main= "cluster 2", ylim=c(0,0.003), ylab="Per-Flower VR", xlab="frequency")
points(((VR2/(VR1+VR2))/F2) ~ invfreq, data= cluster2, col="grey20") # ähnliches Muster wie bei cluster = 1

plot(((VR1/(VR1+VR2))/F1) ~freq, data= cluster6, main= "cluster 6", ylim=c(0,0.003), ylab="Per-Flower VR", xlab="frequency")
points(((VR2/(VR1+VR2))/F2) ~ invfreq, data= cluster6, col="grey20")# ähnliches Muster wie bei cluster = 1

plot(((VR1/(VR1+VR2))/F1) ~freq, data= cluster20, main= "cluster 20", ylim=c(0,0.003), ylab="Per-Flower VR", xlab="frequency")
points(((VR2/(VR1+VR2))/F2) ~ invfreq, data= cluster20, col="grey20") # Muster ein bisschen anders




#################################################
#######number of runs###########################

#default:
NLCommand ("set reward-function 0.00004")
NLCommand ("set number-bees 10")
NLCommand ("set flower-cover 10 ")
NLCommand ("set view 6")
NLCommand ("set flightsteps-until-change 5")
NLCommand ("set stdev-angle 65")
NLCommand ("set cluster-degree 1")


sim.ticks <- function (freqtest) {
  NLCommand("set frequency", freqtest, "setup")
  NLDoCommandWhile("ticks < 10000", "go");
  VR1 <- NLReport("sum [visit-count] of patches with [species = 1]")
  F1 <- NLReport("count patches with [species = 1]")
  VR2 <- NLReport("(sum [visit-count] of patches with [species = 2])")
  F2 <- NLReport("count patches with [species = 2]")
  output <- c(VR1, VR2, F1, F2)
  return ( output)
}


freq.seq <- c(rep((seq(from=5,to=95,by=10)), each=10))
freq.seq5 <- c(rep((seq(from=5,to=95,by=10)), each=5))
freq.seq.3 <- c(rep((seq(from=5,to=95,by=10)), each=3))
freq.seq.2 <- c(rep((seq(from=5,to=95,by=10)), each=2))


c10b10t100 <- sapply(freq.seq,function(d) sim.ticks(d))

c10b10t500 <- sapply(freq.seq,function(d) sim.ticks(d))

c10b10t1000 <- sapply(freq.seq,function(d) sim.ticks(d))

c10b10t3000 <- sapply(freq.seq.small,function(d) sim.ticks(d))

c10b10t10000a <- sapply(freq.seq.small,function(d) sim.ticks(d))

c10b10t1000a <- sapply(freq.seq,function(d) sim.ticks(d))


