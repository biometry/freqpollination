jena <- read.csv2("C:/Users/hczioska/Documents/Data/data1.csv", header=T, sep= ";")
names (jena)
str(jena)

###
jena$PatchID <- as.factor(jena$PatchID)
jena$Instead <- as.factor(jena$Instead)
jena$PatchID <- as.factor(jena$PatchID)

jena$date <- as.POSIXct(paste(jena$Year, jena$Month, jena$Day, sep=" "), format="%Y %m %d") # Datum in R-Format


# Normal distribution?

par(mfrow=c(2,2))
qqnorm(jena$VR_spec)
qqline(jena$VR_spec)
hist(jena$VR_spec) #poisson
shapiro.test(jena$VR_spec) #p-value = 2.2e-16



# Boxplots
par(mfrow=c(1,1))
plot(VR_spec~Freq_Plot, data=jena, main="VR~Frequency", col= Spec, ylab="VR per Flower", xlab="Frequency in %", cex.axis=0.6)
legend("topleft",species,col=1:length(jena$Spec),pch=1, cex=0.7)

I change something here


#####################
##GLM################

model1<-glm(VR_spec~Freq_Plot, data=jena)
summary(model1) #0.00041 ***

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
