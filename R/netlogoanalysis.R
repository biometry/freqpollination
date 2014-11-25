####################################################################
testsize <- read.csv("C:/Users/hczioska/Documents/GitHub/freqpollination/NetLogo/test-size.csv", header=T, sep=",", dec = "," )
str(testsize)
attach(testsize)

#nur die vergleichbaren Datensätze
klein <- subset(testsize, number_patches==10201 )
klein_fixed_rew <- subset(klein, renew_reward==1.001 )


SD <- subset(testsize,renew_reward==1.001 & view ==4 & Flightsteps_until_change == 20)
RR <- subset(testsize, stdev.angle==30 & view ==4 & Flightsteps_until_change == 20)
VW <- subset(testsize, stdev.angle==30 & renew_reward== 1.001 & Flightsteps_until_change == 20)
FUC <- subset(testsize, stdev.angle==30 & renew_reward== 1.001 & view == 4 )


hist(testsize$Visits_per_bee)
plot(testsize$VR1~testsize$VR2)


##################################
####reward
plot (testsize$renew_reward~testsize$VR1, xlim=c(0,30))
points(testsize$renew_reward~testsize$VR2, col="red")

plot(testsize$renew_reward~testsize$reward.1)
points(testsize$renew_reward~testsize$reward.2, col="red")
plot(testsize$renew_reward~testsize$visits_per_step_per_bee)

summary(lm(testsize$renew_reward~testsize$visits_per_step_per_bee)) #weak
summary(lm(testsize$renew_reward~testsize$VR1)) #strong
summary(lm(testsize$renew_reward~testsize$reward.1)) #strong

plot (RR$renew_reward~RR$VR1, xlim=c(0,30))
points(RR$renew_reward~RR$VR2, col="red")
summary(lm(RR$renew_reward~RR$VR1)) #strong


##################################
#####view

plot (testsize$view~testsize$VR1, xlim=c(0,30))
points (testsize$view~testsize$VR2, col="red")
plot(testsize$view~testsize$visits_per_step_per_bee)

summary(lm(testsize$view~testsize$visits_per_step_per_bee)) #strong
summary(lm(testsize$view~testsize$VR1)) #strong NUR seltene Art
summary(lm(testsize$view~testsize$VR2)) #nein
summary(lm(testsize$view~testsize$reward.1)) #nein

plot (VW$view~VW$VR1, xlim=c(10,30))
points(VW$view~VW$VR2, col="red")
summary(lm(VW$view~VW$VR1)) #
summary(lm(VW$view~VW$VR2)) # sig?! Warum? Wohl zu kleine Datenmenge


#########################################
#####stdev

plot(klein_fixed_rew$stdev.angle~klein_fixed_rew$VR1, xlim=c(5,30))
points(klein_fixed_rew$stdev.angle~klein_fixed_rew$VR2, col="red")

plot(testsize$stdev.angle~testsize$VR1,xlim=c(3,30) )
points(testsize$stdev.angle~testsize$VR2, col="red")
# 
summary(lm(klein_fixed_rew$stdev.angle~klein_fixed_rew$VR1)) #nein
summary(lm(testsize$stdev.angle~testsize$VR2)) #schwach


plot(SD$stdev.angle~SD$VR1,xlim=c(15,27) )
points(SD$stdev.angle~SD$VR2, col="red")
summary(lm(SD$stdev.angle~SD$VR1)) #schwach

#########################################
#####flightsteps

plot(testsize$Flightsteps_until_change~testsize$VR1,xlim=c(3,31) )
points(testsize$Flightsteps_until_change~testsize$VR2, col="red")
plot(testsize$Flightsteps_until_change~testsize$visits_per_step_per_bee)

# 
summary(lm(testsize$Flightsteps_until_change~testsize$VR2)) #schwach positiv für niedrige Flightstep-Zahlen 

plot(FUC$Flightsteps_until_change~FUC$VR1,xlim=c(14,28))
points(FUC$Flightsteps_until_change~FUC$VR2, col="red")


