install.packages("lme4")
install.packages("nlme")
install.packages("MASS")
library (lme4)
library (MASS)

jena <- read.csv2("C:/Users/hczioska/Documents/Data/data1.csv", header=T, sep= ";")
names (jena)
str(jena)

###adjustments
jena$PatchID <- as.factor(jena$PatchID)
jena$Instead <- as.factor(jena$Instead)
jena$PatchID <- as.factor(jena$PatchID)

jena$Date <- as.POSIXct(paste(jena$Year, jena$Month, jena$Day, sep=" "), format="%Y %m %d") # Datum in R-Format


jena$Sum_all_visits <- jena$Bees_spec + jena$Bees_other + jena$Bombus_spec + jena$Bombus_other + jena$Hoverfly_spec +jena$Hoverfly_other + jena$Rest_spec + jena$Rest_other

jena$Sum_spec_visits <- jena$Bees_spec  + jena$Bombus_spec  + jena$Hoverfly_spec  + jena$Rest_spec

jena$Spec_visit_share <- jena$Sum_spec_visits/jena$Sum_all_visits

jena$VR_spec <- jena$Sum_spec_visits/jena$Number_Flowers

##############

str(jena)

length(unique(jena$Date)) #15 Sampling-Tage



# Normal distribution?

par(mfrow=c(2,2))
qqnorm(jena$VR_spec)
qqline(jena$VR_spec)
shapiro.test(jena$VR_spec) #p-value = 2.2e-16 , not normally distributed
hist(jena$VR_spec) #poisson

# Boxplots
par(mfrow=c(1,1))
plot(VR_spec~Freq_Plot, data=jena, main="VR~Frequency Plot", col= Spec, ylab="VR per Flower", xlab="Frequency in %", cex.axis=0.6)

plot(VR_spec~Freq_Patch, data=jena, main="VR~Frequency Patch", col= Spec, ylab="VR per Flower", xlab="Frequency in %", cex.axis=0.6)

plot(VR_spec~Cover_Plot, data=jena, main="VR~Cover", col= Spec, ylab="VR per Flower", xlab="Cover in %", cex.axis=0.6)

plot(VR_spec~SR_Plot, data=jena, main="VR~SR", col= Spec, ylab="VR per Flower", cex.axis=0.6)

plot(VR_spec~SR_no_sinlges, data=jena, main="VR~SR > 1", col= Spec, ylab="VR per Flower",  cex.axis=0.6)



##Plotting for different Species

plot(VR_spec~Freq_Plot, data=jena, main="VR~Frequency", ylab="VR per Flower", xlab="Frequency in %", type="n" )
legend("topleft",species,col=1:length(jena$Spec),pch=1, cex=0.7)

range (jena$Freq_Plot) #[1]  1 95
xfreq <- seq(0, 100, 1)

j=1
for (i in species){
  j=j+1
  points(VR_spec~Freq_Plot, data=get(i), col=j)
  model<-glm(VR_spec~Freq_Plot, data=get(i))
  print(summary(model))
  flush.console()
  yfreq <- predict(model,list(Freq_Plot=xfreq), type="response")
  lines (xfreq, yfreq, col=j)
}


#####################
##lme################

model1<-glmmPQL( VR_spec ~ Freq_Plot, random = ~1|Spec, family=poisson, data=jena)
summary(model1) #0.00041 ***



