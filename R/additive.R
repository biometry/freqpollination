
jena <- read.csv2("C:/Users/Helen/Desktop/R/data1.csv", header=T, sep= ";")
jena <- read.csv2("C:/Users/hczioska/Documents/GitHub/Data/data1.csv", header=T, sep= ";")


###data- adjustments
#####################

jena$PatchID <- as.factor(jena$PatchID)
jena$Instead <- as.factor(jena$Instead)
jena$PatchID <- as.factor(jena$PatchID)
jena$Date <- as.POSIXct(paste(jena$Year, jena$Month, jena$Day, sep=" "), format="%Y %m %d") # Datum in R-Format
jena$Sum_all_visits <- jena$Bees_spec + jena$Bees_other + jena$Bombus_spec + jena$Bombus_other + jena$Hoverfly_spec +jena$Hoverfly_other + jena$Rest_spec + jena$Rest_other
jena$Sum_spec_visits <- jena$Bees_spec  + jena$Bombus_spec  + jena$Hoverfly_spec  + jena$Rest_spec
jena$Spec_visit_share <- jena$Sum_spec_visits/jena$Sum_all_visits
jena$VR_spec <- jena$Sum_spec_visits/jena$Number_Flowers
jena$VR_spec_apis <- jena$Bees_spec/jena$Number_Flowers
jena$freq <- jena$Freq_Plot/100

###################


###data-subsets
##############
jena.ono <- jena[which(jena$Spec == "Ono"),]
jena.lat <- jena[which(jena$Spec == "Lat"),]
jena.lot <- jena[which(jena$Spec == "Lot"),]
jena.ger <- jena[which(jena$Spec == "Ger"),]
jena.tp <- jena[which(jena$Spec == "TP"),]

###############
str(jena)

gam1 <- gam(VR_spec ~ Spec + s(Freq_Plot)+ s(Cover_Plot, k=5)+s(SR_Plot, k=9),data=jena)

gam2 <- gam(VR_spec~Spec + s(Freq_Plot)+ s(Cover_Plot, k=5),data=jena)
gam3 <- gam(VR_spec ~ Spec + s(Freq_Plot)+s(SR_Plot, k=9),data=jena)
gam4 <- gam(VR_spec ~ Spec + s(Cover_Plot, k=5)+s(SR_Plot, k=9),data=jena)
gam5 <- gam(VR_spec ~ s(Freq_Plot)+ s(Cover_Plot, k=5)+s(SR_Plot, k=9),data=jena)

gam6 <- gam(VR_spec ~ Spec + s(Freq_Plot)+ s(Cover_Plot, k=5)+s(SR_Plot, k=9) + s(Freq_Plot,Cover_Plot),data=jena)

gam7 <- gam(VR_spec ~ Spec + s(Freq_Plot)+ s(Cover_Plot, k=5)+s(SR_Plot, k=9) + s(Freq_Plot,Cover_Plot)+ s(Freq_Plot,SR_Plot),data=jena)


AIC(gam1, gam2, gam3,gam4,gam5, gam6, gam7) # gam1 am besten (mit allen explanatory variables)

plot(resid(gam1)~fitted(gam1))
plot(resid(gam1)~jena$Freq_Plot)
plot(resid(gam1)~jena$Cover_Plot)
plot(resid(gam1)~jena$SR_Plot)
plot(resid(gam1)~jena$Spec)

par(mfrow=c(2,2))
plot(gam7,residuals=T, pch=16)


#mixed model:

gamm1 <- gamm(VR_spec ~ Spec + s(Freq_Plot)+ s(Cover_Plot, k=5)+s(SR_Plot, k=9),random=list(PlotID=~1),data=jena)

summary(gamm1$gam)
anova(gamm1$gam)
plot(gamm1$lme)
plot(gamm1)


#########additive plotting################################

par(mfrow=c(1,1))

sub <- list(jena.ger, jena.lat, jena.lot,jena.ono, jena.tp)
color <- c("#8E388E", "#C0FF3E", "#EEC900", "#EE1289", "#CD0000")
color.conf <- c("#8E388E30", "#C0FF3E30", "#EEC90030", "#EE128910", "#CD000030")

plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower visitation rate", xlab="Frequency", main="Visitation-Rate depending on Frequency")#, ylim=c(0,1))
legend("topleft", legend = unique(jena$Spec), lty=1, col=color, cex=0.75)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20)
  model <- loess(VR_spec~Freq_Plot, data=sub[[i]])
  yv<-predict(model,data.frame(Freq_Plot=xv))
  lines(xv,yv, col=color[i])
  yv<-predict(model,data.frame(Freq_Plot=xv), se=TRUE)
  lines(xv,yv$fit + 2 * yv$se.fit, lty = 2, col=color[i])
  lines(xv,yv$fit - 2 * yv$se.fit, lty = 2, col=color[i])
}

#zusätzlich für gesamt-daten
model <- loess(VR_spec~Freq_Plot, data=jena)
yv<-predict(model,data.frame(Freq_Plot=xv))
lines(xv,yv, lwd=2)
yv<-predict(model,data.frame(Freq_Plot=xv), se=TRUE)
lines(xv,yv$fit + 2 * yv$se.fit, lty = 2, col=color[i])
lines(xv,yv$fit - 2 * yv$se.fit, lty = 2, col=color[i])



####mit konfidenz-intervallen:############

plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n")# ylim=c(0,1))
legend("topleft", legend = unique(jena$Spec), lty=1, col=color, cex=0.75)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20)
  xv <- seq(min(sub[[i]]$Freq_Plot),max(sub[[i]]$Freq_Plot),1)
  model <- loess(VR_spec~Freq_Plot, data=sub[[i]])
  #test=data.frame(Freq_Plot=xv)
  yv<-predict(model,data.frame(Freq_Plot=xv))
  lines(xv,yv, col=color[i])
  yv<-predict(model,data.frame(Freq_Plot=xv), se=TRUE)
  polygon(x=c(xv, rev(xv)), y=c(yv$fit + 2 * yv$se.fit,rev(yv$fit - 2 * yv$se.fit)), col=color.conf[i], border=color.conf[i])
}

# Zusätzlich für gesamt-daten:
model <- loess(VR_spec~Freq_Plot, data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(model,data.frame(Freq_Plot=xv), se=TRUE)
lines(xv,yv$fit,lwd=2)
polygon(x=c(xv, rev(xv)), y=c(yv$fit + 2 * yv$se.fit,rev(yv$fit - 2 * yv$se.fit)), col="#CDC9C930", border="#CDC9C9")

