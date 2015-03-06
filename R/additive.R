#Zuur from p.50
# see exaple in Zuur Appendix A.4 p.550
"Fig. A.3 all suggest that imposing a linear L.AREA effect may be incorrect. If the GAM indicatesthat the smoother is a straight line, then we know that the linear regression model is correct."

jena <- read.csv2("C:/Users/hczioska/Documents/GitHub/Data/jena/data1.csv", header=T, sep= ";")

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
str(jena)

###data-subsets
##############
jena.ono <- jena[which(jena$Spec == "Ono"),]
jena.lat <- jena[which(jena$Spec == "Lat"),]
jena.lot <- jena[which(jena$Spec == "Lot"),]
jena.ger <- jena[which(jena$Spec == "Ger"),]
jena.tp <- jena[which(jena$Spec == "TP"),]


####test lm vs. gam##############

#Zuur p.553: Test zum Vergleichen was besser ist:
testlm <- lm(VR_spec~ Spec*Freq_Plot,data=jena)
testgam <- gam(VR_spec ~ Spec + s(Freq_Plot, bs="cs")+ s(Freq_Plot,by=Spec, bs="cs"),data=jena)
anova(testlm,testgam, test="F")
#Null-Hypothese, dass die beiden Models gleich sind wird rejected

summary(testgam)
anova(testgam)
plot(resid(testgam)~fitted(testgam))

####mixed model####

#####model selection#####

fFullgam <- formula (VR_spec ~ 
                    Spec + 
                    s(Freq_Plot,bs="cs") + 
                    s(Cover_Plot, k=5,bs="cs")+
                    s(Freq_Plot, by=Spec,bs="cs")) 

gamm.full <- gamm(fFullgam, random=list(PlotID=~1, PatchID=~1), data=jena, weights=varSpec) #no SR
gamm.full2 <- gamm(VR_spec ~ 
                    Spec + 
                    s(Freq_Plot,bs="cs") + 
                    s(SR_Plot, k=9,bs="cs")+
                    s(Freq_Plot, by=Spec,bs="cs"), 
                   random=list(PlotID=~1, PatchID=~1), 
                   data=jena, weights=varSpec) #no cover
gamm.full3 <- gamm(VR_spec ~ 
                     Spec + 
                     s(Freq_Plot,bs="cs") + 
                     s(SR_Plot, k=9,bs="cs")+
                     s(Cover_Plot, k=5,bs="cs"), 
                   random=list(PlotID=~1, PatchID=~1), 
                   data=jena, weights=varSpec) #interaction

AIC(gamm.full,gamm.full2,gamm.full3)

gamm2 <- gamm(VR_spec ~ 
                Spec + 
                s(Freq_Plot,bs="cs") + 
                s(Freq_Plot, by=Spec,bs="cs"), 
              random=list(PlotID=~1, PatchID=~1), 
              data=jena, weights=varSpec)
gamm3 <- gamm(VR_spec ~ 
                Spec + 
                s(Freq_Plot,bs="cs"), 
              random=list(PlotID=~1, PatchID=~1), 
              data=jena, weights=varSpec)
gamm4 <- gamm(VR_spec ~  
                s(Freq_Plot,bs="cs"), 
              random=list(PlotID=~1, PatchID=~1), 
              data=jena, weights=varSpec)
gamm5 <- gamm(VR_spec ~ 
                Spec, 
              random=list(PlotID=~1, PatchID=~1), 
              data=jena, weights=varSpec) 

AIC(gamm.full,gamm.full2,gamm2,gamm3,gamm4,gamm5) #gamm2!

#############################
MG.final<- gamm(VR_spec ~ 
                Spec + 
                s(Freq_Plot,bs="cc") + 
                s(Freq_Plot, by=Spec,bs="cc"), 
              random=list(PlotID=~1, PatchID=~1), 
              data=jena, weights=varSpec)
#cc & cr als smoother gehen nicht

MG.final.nospec<- gamm(VR_spec ~ s(Freq_Plot,bs="cs"), 
                       random=list(PlotID=~1, PatchID=~1), 
                       data=jena, weights=varSpec)

#zuur 155

summary(MG.final$gam)
anova(MG.final.nospec$gam)
#Spec sig
#freq sig (sehr viel mehr, wenn interaction nicht in der formel drin ist)
#interaction only for some species (ono,Ger,Lat)

#####model validation#####
plot(MG.final$lme, residuals=T, pages=1)
plot(MG.final$gam, residuals=T, pages=1)
plot(testgam , residuals=T, pages=1)
plot(MG.final$gam, residuals=T, pages=1)
vis.gam(testgam, type="response", plot.type="contour")
#wie bei lme: weighting bringts!

#########additive plotting################################
par(mfrow=c(1,1))
species <- c("Ger", "Lat", "Lot", "Ono", "TP")
subspec <- list(jena.ger, jena.lat, jena.lot,jena.ono, jena.tp)
color <- c("darkviolet","orange", "red", "magenta", "deepskyblue4")
#color.conf <- c("#8E388E30", "#C0FF3E30", "#EEC90030", "#EE128910", "#CD000030")


#############
plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower visitation rate", xlab="Frequency", main="Visitation-Rate depending on Frequency", log="y")
legend("bottomright", legend = unique(jena$Spec), pch=20, col=color, cex=0.75)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=subspec[[i]], col=color[i], pch=20)
  xv <- seq(min(subspec[[i]]$Freq_Plot),max(subspec[[i]]$Freq_Plot),1)
  model <- loess(VR_spec~Freq_Plot, data=subspec[[i]], span=1)
  yv<-predict(model,data.frame(Freq_Plot=xv))
  lines(xv,yv, col=color[i])
  #yv<-predict(model,data.frame(Freq_Plot=xv), se=TRUE)
  #lines(xv,yv$fit + 2 * yv$se.fit, lty = 2, col=color[i])
  #lines(xv,yv$fit - 2 * yv$se.fit, lty = 2, col=color[i])
}

#zusätzlich für gesamt-daten
model <- loess(VR_spec~Freq_Plot, data=jena, span=1)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(model,data.frame(Freq_Plot=xv))
lines(xv,yv, lwd=2)
yv<-predict(model,data.frame(Freq_Plot=xv), se=TRUE)
lines(xv,yv$fit + 2 * yv$se.fit, lty = 2, col="#404040")
lines(xv,yv$fit - 2 * yv$se.fit, lty = 2, col="#404040")



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

####mit gam als predictor######################################

plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n",log="y")
legend("bottomright", legend = unique(jena$Spec), lty=1, col=color, cex=0.75)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=subspec[[i]], col=color[i], pch=20, cex=.8)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  gam.nospec <- gam(VR_spec ~ s(Freq_Plot, bs="cs", k=4),data=subspec[[i]])
  yv <- predict(gam.nospec,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=1)
}

gam.nospec <- gam(VR_spec ~ s(Freq_Plot, bs="cs"),data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(gam.nospec,data.frame(Freq_Plot=xv), se=TRUE)
lines(xv,yv$fit,lwd=2)
polygon(x=c(xv, rev(xv)), y=c(yv$fit + 2 * yv$se.fit,rev(yv$fit - 2 * yv$se.fit)), col="#CDC9C930", border="#CDC9C9")



########gamm#########################

plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", log="y")
legend("bottomright", legend = unique(jena$Spec), lty=1, col=color, cex=0.75)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=subspec[[i]], col=color[i], pch=20, cex=.8)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  gam.nospec <- gamm(VR_spec ~ s(Freq_Plot, bs="cs", k=4),
                     data=subspec[[i]],
                     random=list(PlotID=~1, PatchID=~1), 
                     weights=varSpec)
  yv <- predict(gam.nospec,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=1)
}


gam.nospec <- gamm(VR_spec ~ s(Freq_Plot, bs="cs"),
                   data=jena,
                   random=list(PlotID=~1, PatchID=~1), 
                   weights=varSpec)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(gam.nospec,data.frame(Freq_Plot=xv), se=TRUE)
lines(xv,yv$fit,lwd=2, lty=2)
lines(xv,yv$fit + 2 * yv$se.fit, lty = 2, col="#404040")
lines(xv,yv$fit - 2 * yv$se.fit, lty = 2, col="#404040")
