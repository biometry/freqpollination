install.packages("lme4")
install.packages("nlme")
install.packages("MASS")
install.packages("mgcv")
install.packages("arm")
install.packages("car")
install.packages("lmtest")
install.packages("lattice")

library (lme4)
library (MASS)
library (mgcv)
library (arm)
library (nlme)
library (car)
library(lmtest)
library (lattice)

jena <- read.csv2("C:/Users/Helen/Desktop/R/data1.csv", header=T, sep= ";")
jena <- read.csv2("C:/Users/hczioska/Documents/GitHub/Data/data1.csv", header=T, sep= ";")

names (jena)
str(jena)

###########################
###data-adjustments#############
jena$PatchID <- as.factor(jena$PatchID)
jena$Instead <- as.factor(jena$Instead)
jena$PatchID <- as.factor(jena$PatchID)

jena$Date <- as.POSIXct(paste(jena$Year, jena$Month, jena$Day, sep=" "), format="%Y %m %d") # Datum in R-Format

jena$Sum_all_visits <- jena$Bees_spec + jena$Bees_other + jena$Bombus_spec + jena$Bombus_other + jena$Hoverfly_spec +jena$Hoverfly_other + jena$Rest_spec + jena$Rest_other

jena$Sum_spec_visits <- jena$Bees_spec  + jena$Bombus_spec  + jena$Hoverfly_spec  + jena$Rest_spec

jena$Spec_visit_share <- jena$Sum_spec_visits/jena$Sum_all_visits

jena$VR_spec <- jena$Sum_spec_visits/jena$Number_Flowers

jena$VR_spec_apis <- jena$Bees_spec/jena$Number_Flowers

str(jena)
length(unique(jena$Date)) #15 Sampling-Tage
length(unique(jena$PlotID))

boxplot(VR_spec~Freq_Plot, data=jena) # ist das bei 20% ein outlier? Muss der raus?


##############################################
####check for colinearity #############################################################################



z <- cbind(jena$Spec, jena$Freq_Patch, jena$Cover_Patch, jena$SR_Patch, jena$Freq_Plot, jena$Cover_Plot, jena$SR_Plot, jena$SR_no_sinlges, jena$VR_spec)

colnames(z) <- c("Spec", "Freq_Patch", "Cover_Patch",
                 "SR_Patch", "Freq_Plot", "Cover_Plot", "SR__Plot", "SR_no_1", "VR_spec")

z <- z[complete.cases(z),]

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


pairs(z, lower.panel = panel.smooth,
             upper.panel = panel.cor)


#check VIF:
source("C:/Users/hczioska/Documents/HighstatLibV6.R")
corvif(z)
#"All VIF values are below 3 (see Chapter 26 in Zuur et al. (2007)), indicating there is no collinearity in these variables"


#dotchart (see p.454 and 537 pdf)
dotchart(jena$VR_spec, groups = jena$PlotID)
dotchart(jena$VR_spec, groups = jena$Spec)
dotchart(jena$Freq_Plot, group=jena$spec)
dotchart(jena$VR_spec)

#####Analysis#########


#####Backward selection
#######
lmeALL <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena)

lme1a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme1b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot +I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena)

lme1c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena)

lme1d <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena)

anova(lmeALL, lme1a, lme1b, lme1c, lme1d) #lme1a wins

lme1a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme2b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot, random=~1|PlotID/PatchID, data=jena)

lme2c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme2d <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena)

anova(lmeALL, lme1a, lme2b, lme2c, lme2d) #lme2c wins

lme2c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme3a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme3b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena)

anova(lmeALL,lme2c, lme3a, lme3b) #lme3a wins

lme3a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme4a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

anova(lme3a, lme4a) #lme4a wins

lme4a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme5a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme5b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme5c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme5d <- lme(VR_spec ~ Freq_Plot+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme5e <- lme(VR_spec ~ I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

anova(lme4a, lme5a, lme5b, lme5c, lme5d, lme5e) #lme5d wins

lme5d <- lme(VR_spec ~ Freq_Plot+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme6a <- lme(VR_spec ~ Freq_Plot+Spec+Cover_Plot, random=~1|PlotID/PatchID, data=jena)

lme6b <- lme(VR_spec ~ Freq_Plot+Spec+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme6c <- lme(VR_spec ~ Freq_Plot+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme6d <- lme(VR_spec ~ Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

anova(lme5d, lme6a, lme6b, lme6c, lme6d) #lme6b

lme6b <- lme(VR_spec ~ Freq_Plot+Spec+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme7a <- lme(VR_spec ~ Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena)

lme7b <- lme(VR_spec ~ Freq_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena)

lme7c <- lme(VR_spec ~ Spec+SR_Plot, random=~1|PlotID/PatchID, data=jena)

anova(lme6b, lme7a, lme7b, lme7c) # lme7a

lme7a <- lme(VR_spec ~ Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena)

lme8a <- lme(VR_spec ~ Freq_Plot, random=~1|PlotID/PatchID, data=jena)

lme8b <- lme(VR_spec ~ Spec, random=~1|PlotID/PatchID, data=jena)

anova(lme7a, lme8a, lme8b) #lme7a (minimal adequat model)

#####
#winner of backward selection:
M.final <- lme(VR_spec ~ Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena)
summary(M.final)

#noch schauen ob random effect besser ist:
M.final <- lme(VR_spec ~ Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena)
M.gls<- gls(VR_spec ~ Freq_Plot+Spec, data=jena)
anova(M.gls,M.final) #random effect besser!


#Model          df    AIC   BIC   logLik   Test L.Ratio p-value
#M.gls       1  7 1204.900 1232.463 -595.4501                       
#M.final     2  9 1194.351 1229.789 -588.1757 1 vs 2 14.5488   7e-04
#The model with random intercept is better L= 14.55 (df=2, p<0.0001)


##################################################
##########Model Validation
#Crawley p.347
#Zuur Chapter 4 p.85


#schauen wegen residuals:
plot(M.final) 
#mist: Heterogeneity / Heteroscedasticity
#violates the homogeneity of variance assumption

#ein paar mehr plots:
par(mfrow=c(2,2))
plot(resid(M.final)~fitted(M.final)) #Lücke und Heteroscedasticity
plot(resid(M.final)~jena$Spec,xlab="Spec")
plot(resid(M.final)~jena$Freq_Plot, xlab="Freq")
hist(resid(M.final))

mcheck <-function (obj) {
  rs<-obj$resid
  fv<-obj$fitted
  par(mfrow=c(1,2))
  plot(rs~fv,xlab="Fitted values",ylab="Residuals")
  abline(h=0, lty=2)
  qqnorm(rs,xlab="Normal scores",ylab="Ordered residuals",main="")
  qqline(rs,lty=2)
  par(mfrow=c(1,1))
  invisible(NULL) }

mcheck(M.final)
#residuals increasing steeply with the fitted values
#non-normal distribution of errors (see. Crawley p. 351)

#transformation? Log geht wohl nicht weil so viele Nuller drin sind.

#plotting residuals agains not-used explanatory variables (Zuur p.547)
par(mfrow=c(2,2))
plot(resid(M.final)~jena$Cover_Plot,xlab="Cover")
plot(resid(M.final)~jena$SR_Plot,xlab="SR")
plot(resid(M.final)~I(Freq_Plot^2),xlab="Freq2", data=jena)
plot(resid(M.final)~I(Freq_Plot^2),xlab="Freq2", data=jena)


##################
#Möglichkeiten aus Zuur chapter 4 (p.72ff):
#(auch: Zuur Appendix p. 535)
#Adjestment of weights

###applying different weights
######
M.original <- gls(VR_spec ~ Freq_Plot+Spec, data=jena)

fixed <- varFixed(~Freq_Plot)
M1.fixed <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=fixed)
#AIC sogar größer als ohne weight?!
#Geht nicht für Spec weil factor

varFreq <- varIdent(form=~1|Freq_Plot)
M1.varFreq <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varFreq)

varSpec <- varIdent(form=~1|Spec)
M1.varSpec <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varSpec)
#each species is allowed to have a different variance

varP <-varPower(form=~Freq_Plot|Spec)
M1.varP <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varP)

varPf <-varPower(form=~Freq_Plot)
M1.varPf <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varPf)

varE <- varExp(form=~Freq_Plot)
M1.varE <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varE)

varCP <- varConstPower(form=~Freq_Plot)
M1.varCP <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varCP)

varCPs <- varConstPower(form=~Freq_Plot|Spec)
M1.varCPs <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varCPs)
#Constant plus power of the variance covariate (?)

varCom <- varComb(varIdent(form=~1|Spec),varExp(form=~Freq_Plot))
M1.varCom <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varCom)

varCom2 <- varComb(varFixed(~Freq_Plot), varIdent(form=~1|Spec))
M1.varCom2 <- gls(VR_spec~Freq_Plot+Spec, data=jena, weights=varCom2)

#####Ergebnis:
AIC(M.original,M1.fixed, M1.varFreq, M1.varSpec, M1.varP, M1.varPf, M1.varE, M1.varCP, M1.varCPs, M1.varCom, M1.varCom2) #Winner is varCPs, danach varSpec (varIdent)
#macht auch total sinn varIdent zu nehmen. Spec is a nominal variable, allowing different residual variation for the visitation rate per focal species

#so, zurück zu den mixed models:

M2 <- lme(VR_spec~Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena, weights = varSpec)
M3 <- lme(VR_spec~Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena, weights=varCPs) #geht nicht :(


anova(M.final,M2) 
#sehr großer unterschied! M2 viel besser! 
# L= 422.11 (df=4, p<0.0001)

###neues finale Model, jetzt mit Gewichtung:
M.final2 <- lme(VR_spec~Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena, weights = varSpec)


##############################
#Noch mal Model validation
par(mfrow=c(2,2))

plot(M.final)
plot(M.final2) #die redisuals sind vor allem vorne auseinandergezogen. Gibt aber immer noch eine Lücke

plot(resid(M.final2, type="normalized")~fitted(M.final2))
identify(fitted(M.final2), resid(M.final2))

plot(resid(M.final2)~jena$Spec,xlab="Spec")
boxplot(resid(M.final2)~jena$Freq_Plot, xlab="Freq")
hist(resid(M.final2))
mcheck(M.final2) #auch hier sieht es aus wie vorher!
coplot(resid(M.final2)~Freq_Plot|Spec, data=jena)


#ist jetzt nicht so toll, weiß aber auch nicht, was man noch tun könnte....


###Interpretation##########################

M.final2 <- lme(VR_spec~Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena, weights = varSpec)

summary(M.final2)

#Value Std.Error  DF    t-value p-value
#(Intercept)  2.9164051 0.1456404 205  20.024695  0.0000
#Freq_Plot    0.0034963 0.0008557 205   4.085745  0.0001
#SpecLat     -2.5153888 0.1537429 205 -16.361003  0.0000
#SpecLot     -2.7617626 0.1448555 205 -19.065638  0.0000
#SpecOno      0.5549522 0.4268535 205   1.300100  0.1950
#SpecTP      -2.8315112 0.1441490 205 -19.642947  0.0000

# Alles hoch signifikant außer Ono
# Ono hat aber auch eine mise Datenlage
# Sehr ähnliche Steigung außer für Ono
#Lat, Lot und TP sehr ähnlich
#Sehr geringe Steigung für Freq-Plot
#random effect with a variance of 0.18^2



###############################################
#habe die Interaction noch mal mit rein genommen. Zwar ist der AIC höher, aber es gibt signifikante Interactionen

M.final3 <- lme(VR_spec~Freq_Plot*Spec, random=~1|PlotID/PatchID, data=jena, weights = varSpec)

summary(M.final3)
anova(M.final3, M.final2)


library(effects)
plot(effect(term="Freq_Plot:Spec",mod=M.final3, default.levels=20), multiline=TRUE)
#Linien sehen (außer für Ono) ziemlich parallel aus


######Plotten#######################################

Mlm <- glm(VR_spec~Freq_Plot+Spec, data=jena)
#kann nicht predicten mit dem lme-model :(

species <- c("Ger", "Lat", "Lot","Ono", "TP")
color <- c("#8E388E", "#C0FF3E", "#EEC900", "#EE1289", "#CD0000")
color.conf <- c("#8E388E30", "#C0FF3E30", "#EEC90030", "#EE128910", "#CD000030")
sub <- list(jena.ger, jena.lat, jena.lot,jena.ono, jena.tp)

plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower visitation rate", xlab="Frequency", main="Visitation rate depending on frequency")#, ylim=c(0,1))
legend("topleft", legend = unique(jena$Spec), lty=1, col=color, cex=0.75)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  yv <- predict(Mlm,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i])
}


#################

par(mfrow=c(2,2))
xyplot(VR_spec~Freq_Plot, groups=Spec, data=jena, type=c("a","p"), auto.key=list(space="right"))

xyplot(VR_spec~Freq_Plot, groups=Spec, data=jena, type=c("a","p"), auto.key=list(space="right"), ylim=c(0,6))

xyplot(VR_spec~Freq_Plot, groups=Spec, data=jena, type=c("a","p"), auto.key=list(space="right"), ylim=c(0,1.5))
#könnte man sogar als anti-sigmoide-funktion sehen. das passt zum modell...


###########

