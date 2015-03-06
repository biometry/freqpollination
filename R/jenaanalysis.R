###packages####
install.packages("lme4")
install.packages("nlme")
install.packages("MASS")
install.packages("mgcv")
install.packages("arm")
install.packages("car")
install.packages("lmtest")
install.packages("lattice")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("lsmeans")
install.packages("MuMIn")
install.packages("papeR")
install.packages("gamm4")

library (lme4)
library (MASS)
library (mgcv)
library (arm)
library (nlme)
library (car)
library(lmtest)
library (lattice)
library(ggplot2)
library(multcomp)
library(lsmeans)
library(MuMIn)
library(papeR)
library(gamm4)

jena <- read.csv2("C:/Users/hczioska/Documents/GitHub/Data/data1.csv", header=T, sep= ";")

names (jena)
str(jena)


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

#jena$Freq_Plot <- scale(jena$Freq_Plot,center=T,scale=F)


###data-subsets#####

jena.ono <- jena[which(jena$Spec == "Ono"),]
jena.lat <- jena[which(jena$Spec == "Lat"),]
jena.lot <- jena[which(jena$Spec == "Lot"),]
jena.ger <- jena[which(jena$Spec == "Ger"),]
jena.tp <- jena[which(jena$Spec == "TP"),]

jena.ono <- jena.ono[order(jena.ono$Freq_Plot),]
jena.lat <- jena.lat[order(jena.lat$Freq_Plot),]
jena.ger <- jena.ger[order(jena.ger$Freq_Plot),]
jena.tp <- jena.tp[order(jena.tp$Freq_Plot),]
jena.lot <- jena.lot[order(jena.lot$Freq_Plot),]



#################################
###START ANALYSIS################
#################################

####check for colinearity #######
#####
 
z <- cbind(jena$VR_spec, jena$Spec, jena$Freq_Plot, jena$Cover_Plot, jena$SR_Plot, jena$Freq_Patch, jena$Cover_Patch, jena$SR_Patch)

colnames(z) <- c("Visitation Rate" , "Species",  "Frequency", "Cover", "Species Richness" , "Freqency (Sub)", "Cover (Sub)", "SR (Sub)")

z <- z[complete.cases(z),]

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}


pairs(z, lower.panel = panel.smooth,
             upper.panel = panel.cor, main="Collinearity of Variables")

#check VIF:

colnames(z) <- c("Visitation_Rate" , "Species",  "Frequency", "Cover", "Species_Richness" , "Freqency_Sub", "Cover_Sub", "SR_Sub")

source("C:/Users/hczioska/Documents/HighstatLibV6.R")
corvif(z)
#"All VIF values are below 3 (see Chapter 26 in Zuur et al. (2007)), indicating there is no collinearity in these variables"

#dotchart (see p.454 and 537 pdf)
dotchart(jena$VR_spec, groups = jena$PlotID)
dotchart(jena$VR_spec, groups = jena$Spec)
dotchart(jena$Freq_Plot, group=jena$spec)
dotchart(jena$VR_spec)


#################################

#I want to apply a mixed model because I have 8 observation per Plot.The data is therefore not independent but nested. 

#nach Zuur p.136 erst ein beyond optimal model aufstellen und den optimalen random effect finden. Dann mit der backward selection weiter machen um die optimal fixed structure rauszufinden. Models mit verschiedenen fixed effects müssen mit ML vergleichen weden. Die finale präsentation immer mir REML


####random vs no random effect####
#####
glsALL <- gls(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec+(Freq_Plot^3):Spec, data=jena, method="REML")

lmeALL1 <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec+(Freq_Plot^3):Spec, random=~1|PlotID/PatchID, data=jena, method="REML")

lmeALLweight <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec+(Freq_Plot^3):Spec, random=~1|PlotID/PatchID, data=jena, method="REML", weight=varSpec)

plot(lmeALL1)

anova(glsALL,lmeALL1)
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#glsALL      1 21 1362.438 1444.336 -660.2190                        
#lmeALL1     2 23 1357.480 1447.178 -655.7402 1 vs 2 8.957665  0.0113

# zuur mentions something with testing on the boundary and the p-value should be multiplied with 0.5 (eg p.468)

anova(lmeALL1, lmeALLweight)


#####Backward selection NR.1#####
# Zuur p.122: "To compare models with nested fixed effects (but with the same random structure), ML estimation must be used and not REML." We"  

#######
lmeALL <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena, method="ML")

lme1a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme1b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot +I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena, method="ML")

lme1c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena, method="ML")

lme1d <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot+I(Freq_Plot^2):Spec , random=~1|PlotID/PatchID, data=jena, method="ML")

anova(lmeALL, lme1a, lme1b, lme1c, lme1d) #lme1a wins

lme1a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme2b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme2c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme2d <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Spec:Freq_Plot + I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

anova(lme1a, lme2b, lme2c, lme2d) #lme2b wins

lme2b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot+ Spec:Freq_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme3a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Freq_Plot:Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme3b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Spec:Freq_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

anova(lme2b, lme3a, lme3b) # lme3b wins

lme3b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + Spec:Freq_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme4a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

anova(lme3a, lme4a)  #lme4a wins

lme4a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme5a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme5b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+SR_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme5c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme5d <- lme(VR_spec ~ Freq_Plot+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme5e <- lme(VR_spec ~ I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

anova(lme4a, lme5a, lme5b, lme5c, lme5d, lme5e) #lme5a wins

anova(lme4a, lme5a)  
#not significant, SR_Plot can be dropped
#all others are significant

lme6a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec, random=~1|PlotID/PatchID, data=jena, method="ML")

lme6b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme6c <- lme(VR_spec ~ Freq_Plot+Spec+Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme6d <- lme(VR_spec ~ I(Freq_Plot^2)+Spec+Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML")

lme7 <- lme(VR_spec ~ Freq_Plot+Spec, random=~1|PlotID/PatchID, data=jena, method="ML")

anova(lme5a,lme6a,lme6b,lme6c,lme6d,lme7) #lme5a am niedrigsten ABER:
anova(lme5a, lme6a)#nicht sig --> Cover_Plot can be dropped, oder?
summary(lme5a)
summary(lme6a)


#####
#winner of backward selection:
M.final <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot, random=~1|PlotID/PatchID, data=jena)
summary(M.final)


#damit es ab jetzt kürzer ist:
fFULL <- formula(VR_spec~
                   Freq_Plot+
                   I(Freq_Plot^2)+
                   Spec+
                   Cover_Plot)


####Model Validation Nr. 1####
#####
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
plot(resid(M.final)~jena$Cover_Plot, xlab="Freq")
plot(resid(M.final)~I(Freq_Plot^2),xlab="Freq2", data=jena)
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
#Möglichkeiten aus Zuur chapter 4 (p.72ff & (auch: Zuur Appendix p. 535) ---> Adjestment of weights

####applying different weights####
######

M.original <- gls(fFULL, data=jena)

fixed <- varFixed(~Freq_Plot)
M1.fixed <- gls(fFULL, data=jena, weights=fixed)
#AIC sogar größer als ohne weight?!
#Geht nicht für Spec weil factor

varFreq <- varIdent(form=~1|Freq_Plot)
M1.varFreq <- gls(fFULL, data=jena, weights=varFreq)

varSpec <- varIdent(form=~1|Spec)
M1.varSpec <- gls(fFULL, data=jena, weights=varSpec)
#each species is allowed to have a different variance

varCov <- varIdent(form=~1|Cover_Plot)
M1.varCov <- gls(fFULL, data=jena, weights=varCov)
#each species is allowed to have a different variance

varP <-varPower(form=~Freq_Plot|Spec)
M1.varP <- gls(fFULL, data=jena, weights=varP)

varPf <-varPower(form=~Freq_Plot)
M1.varPf <- gls(fFULL, data=jena, weights=varPf)

varE <- varExp(form=~Freq_Plot)
M1.varE <- gls(fFULL, data=jena, weights=varE)

varCP <- varConstPower(form=~Freq_Plot)
M1.varCP <- gls(fFULL, data=jena, weights=varCP)

varCPs <- varConstPower(form=~Freq_Plot|Spec)
M1.varCPs <- gls(fFULL, data=jena, weights=varCPs)
#Constant plus power of the variance covariate (?)

varCom <- varComb(varIdent(form=~1|Spec),varExp(form=~Freq_Plot))
M1.varCom <- gls(fFULL, data=jena, weights=varCom)

varCom2 <- varComb(varFixed(~Freq_Plot), varIdent(form=~1|Spec))
M1.varCom2 <- gls(fFULL, data=jena, weights=varCom2)


#####
#Ergebnis:
AIC(M.original,M1.fixed, M1.varFreq, M1.varSpec, M1.varCov, 
    M1.varP, M1.varPf, M1.varE, M1.varCP, M1.varCPs, 
    M1.varCom, M1.varCom2) 

#Winner is varCPs, danach varSpec (varIdent)
#macht auch total sinn varIdent zu nehmen:
#Spec is a nominal variable. So varIdent allowes different residual variation for the visitation rate per focal species

#In das mixed model einbauen:
M2 <- lme(fFULL, random=~1|PlotID/PatchID, data=jena, weights = varSpec)
M3 <- lme(fFULL, random=~1|PlotID/PatchID, data=jena, weights=varCPs, control=list(maxIter=1000, msMaxIter=1000)) #geht leider 

anova(M.final,M2) 

plot(M.final)
plot(M2)
hist(jena$VR_spec,breaks=20)
#sehr großer unterschied! M2 viel besser! 
# L= 418.17 (df=4, p<0.0001)
#significantly better variance structure


####zweite backward selection mit gewichtung!#####
lmeALL <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
                Freq_Plot:Cover_Plot+ 
                Spec:Freq_Plot + 
                I(Freq_Plot^2): Cover_Plot+
                I(Freq_Plot^2):Spec , 
              random=~1|PlotID/PatchID, 
              data=jena, method="ML", weights = varSpec)

lme1a <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
               Freq_Plot:Cover_Plot+ 
               Spec:Freq_Plot + 
               I(Freq_Plot^2): Cover_Plot, 
             random=~1|PlotID/PatchID, data=jena, 
             method="ML", weights = varSpec)

lme1b <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
               Freq_Plot:Cover_Plot+ 
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme1c <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
               Freq_Plot:Cover_Plot+ 
               I(Freq_Plot^2): Cover_Plot+
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme1d <- lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot + 
               I(Freq_Plot^2): Cover_Plot+
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

anova(lmeALL, lme1a, lme1b, lme1c, lme1d) #lme1b wins

lme1b <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
               Freq_Plot:Cover_Plot+ 
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme2a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
               Freq_Plot:Cover_Plot+ 
               Spec:Freq_Plot, 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme2b <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot + 
               Freq_Plot:Cover_Plot+ 
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme2c <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

anova(lme1a, lme2a, lme2b, lme2c) #lme2c wins


lme2c <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)


lme3a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme3b <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot, 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

anova(lme2c,lme3a,lme3b,lme3c,lme3d) #2c wins?!?!

anova(lme2c) #aber Cover und SR sind nicht significant

lme4a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme4b <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+SR_Plot +  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme4c <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme4d <- lme(VR_spec ~ 
               Freq_Plot+Spec+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)


lme4e <- lme(VR_spec ~ 
               I(Freq_Plot^2)+Spec+Cover_Plot+SR_Plot +  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

anova(lme2c, lme4a,lme4b,lme4c,lme4d,lme4e) #lme4a

lme4a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+Cover_Plot+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme5a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme5b <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Cover_Plot+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme5c <- lme(VR_spec ~ 
               Freq_Plot+Spec+Cover_Plot+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)


lme5d <- lme(VR_spec ~ 
               I(Freq_Plot^2)+Spec+Cover_Plot+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

anova(lme4a, lme5a,lme5b,lme5c,lme5d) #lme5a

lme5a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme6a <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+   
               Spec:Freq_Plot
               , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme6b <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+  
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

lme6c <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec, 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

anova(lme5a,lme6a,lme6b,lme6c) #lme5a am besten!

summary(lme5a) 
#die interaction I(Freq_Plot^2):Spec ist nie significant, das model wird aber schlechter, wenn man die interaction raus nimmt. Komisch!
#I(Freq_Plot^2) ist auch als einzelner Factor nicht significant. Muss aber drin bleiben wegen interaktion


####neues finale Model, jetzt ohne Cover_Plot und mit Interaktionen####
M.final2 <- lme(VR_spec ~ 
               Freq_Plot+I(Freq_Plot^2)+Spec+  
               Spec:Freq_Plot +
               I(Freq_Plot^2):Spec , 
             random=~1|PlotID/PatchID, 
             data=jena, method="ML", weights = varSpec)

####  MiMIn global Model Selection####

lmeFull <- lme(VR_spec ~ Freq_Plot+
                 I(Freq_Plot^2)+I(Freq_Plot^2)+Spec+Cover_Plot+
                SR_Plot + 
                Freq_Plot:Cover_Plot+ 
                Spec:Freq_Plot + 
                I(Freq_Plot^2): Cover_Plot+
                I(Freq_Plot^2):Spec+
                 I(Freq_Plot^3):Spec , 
              random=~1|PlotID/PatchID, 
              data=jena, weights = varSpec, method="ML")

dd2 <- dredge(lmeFull, extra= alist(AIC))

M.test <- lme(VR_spec~ Spec + Freq_Plot, 
              random=~1|PlotID/PatchID, 
              data=jena, weights = varSpec, method="ML")

anova(M.final2, M.test)
summary(M.test)

####NEUES FINAL MODEL###########

fFULL2 <- formula(VR_spec ~ 
                    Freq_Plot+
                    I(Freq_Plot^2)+
                    Spec+  
                    Spec:Freq_Plot +
                    Spec:I(Freq_Plot^2))

#REML
M.final2 <- lme(fFULL2, 
                random=~1|PlotID/PatchID, 
                data=jena, weights = varSpec)
M.final2ml <- lme(fFULL2, 
                random=~1|PlotID/PatchID, 
                data=jena, weights = varSpec, method="ML")

#####Noch mal Model validation####

par(mfrow=c(2,2))

plot(M.final2) 
#die redisuals sind vor allem vorne auseinandergezogen. Gibt aber immer noch eine Lücke, Heteroscedasticity ist viel besser aber noch vorhanden
#auch viel besser als bei dem Modell ohne interaktionen!

plot(resid(M.final2, type="normalized")~fitted(M.final2)) #das gleiche
plot(resid(M.final2)~jena$Spec,xlab="Spec")
plot(resid(M.final2)~jena$Freq_Plot, xlab="Freq")
hist(resid(M.final2))

###Interpretation#############

summary(M.final2)
anova(M.final2)
#                          Value    Std.Error  DF   t-value p-value
# (Intercept)             1.8933287 0.3775218 196  5.015150  0.0000
# Freq_Plot               0.0493225 0.0197384 196  2.498816  0.0133
# I(Freq_Plot^2)         -0.0003421 0.0002025 196 -1.689862  0.0926

# SpecLat                -2.2110327 0.4201872 196 -5.262018  0.0000
# SpecLot                -1.7853225 0.3936441 196 -4.535372  0.0000
# SpecOno                 0.2324026 0.8984138 196  0.258681  0.7962
# SpecTP                 -1.8321285 0.3820491 196 -4.795532  0.0000

# Freq_Plot:SpecLat      -0.0100732 0.0212353 196 -0.474362  0.6358
# Freq_Plot:SpecLot      -0.0401400 0.0207382 196 -1.935554  0.0544
# Freq_Plot:SpecOno       0.0108222 0.0660975 196  0.163731  0.8701
# Freq_Plot:SpecTP       -0.0426081 0.0201921 196 -2.110137  0.0361

# I(Freq_Plot^2):SpecLat  0.0000121 0.0002148 196  0.056210  0.9552
# I(Freq_Plot^2):SpecLot  0.0002623 0.0002143 196  1.223930  0.2224
# I(Freq_Plot^2):SpecOno  0.0000105 0.0007750 196  0.013578  0.9892
# I(Freq_Plot^2):SpecTP   0.0002932 0.0002090 196  1.403403  0.1621


#Ono unterscheidet sich nicht von Ger, alle anderen Arten schon, Focal species kann man in 2 Gruppen unterteilen, die sich dann nicht wesentlich voneinander unterscheiden. (das bezieht sich aber nicht auf irgendwelchen Slope sondern nur auf die attraktivität. nicht wirklich spannendes ergebnis für mich, dass es unterschiedlich attraktive arten gibt)
#Ono hat aber auch eine mise Datenlage (riesiger standard-error)
#genaue Werte siehe Posthoc

#Sehr geringe Steigung für Freq-Plot:Ger 0.049, ganz leicht positiv, auch nicht ganz so toll signifikant
#Ono hat einen höheren slope, alle anderen flacher. Lot &  TP sehr ähnlich, sehr flach, fast 0 (0.049-0.043=0.006 für TP). Sieht man auch in der Graphik. 

# Die Freq_Plot:Spec - Interaktionen ist nur TP unter 0.05, aber auch nur knapp. Sind alle vom Wert her auch ähnlich (sehr klein und positiv gegenüber Ger)

#warum ist I(Freq_Plot^2):Spec nicht signifikant?

#Die I(Freq_Plot^2):Spec - Interaktionen sind alle nicht signifikant. Heißt das sie unterscheiden sich alle nicht von Ger. Aber alle sind negativ (macht auch Sinn, nach unten geöffnete Parabel). Ger hat den größten negativen Wert = am meisten gekrümmt von allen 5 Arten


#see Zuur p463 for "what to write in a paper"

###hier der output wenn man Freq_Plot centert####

# Value Std.Error  DF    t-value p-value
# (Intercept)             3.309995 0.2005496 196  16.504620  0.0000
# Freq_Plot               0.022226 0.0059283 196   3.749164  /0.0002
# I(Freq_Plot^2)         -0.000342 0.0002025 196  -1.689862  0.0926

# SpecLat                -2.591000 0.2122931 196 -12.204824  0.0000
# SpecLot                -2.963546 0.2100675 196 -14.107587  0.0000
# SpecOno                 0.677464 0.8495555 196   0.797433  /0.4262
# SpecTP                 -3.059555 0.2067542 196 -14.798032  0.0000

#hier alle p-values SEHR viel niedriger:
# Freq_Plot:SpecLat      -0.009117 0.0065246 196  -1.397326  0.1639
# Freq_Plot:SpecLot      -0.019366 0.0062263 196  -3.110378  0.0021
# Freq_Plot:SpecOno       0.011656 0.0138835 196   0.839528  0.4022
# Freq_Plot:SpecTP       -0.019383 0.0060447 196  -3.206615  0.0016

#hier wieder gleich:
# I(Freq_Plot^2):SpecLat  0.000012 0.0002148 196   0.056210  0.9552
# I(Freq_Plot^2):SpecLot  0.000262 0.0002143 196   1.223930  0.2224
# I(Freq_Plot^2):SpecOno  0.000011 0.0007750 196   0.013578  0.9892
# I(Freq_Plot^2):SpecTP   0.000293 0.0002090 196   1.403403  0.1621

#Zahlen sind auf jeden Fall anders, aber nur für Freq_Plot, nicht für I(Freq_Plot^2). 
#macht es denn überhaupt Sinn die Daten zu centern? Klar habe ich mehr Plots mit geringer Frequency aufgenommen, weil es davon einfach mehr gab. Trotzdem bin ich da interessiert an dem Effect der genauen Frequency-WErte um sie auch mit dem MOdel zu vergleichen. 


###################################
###################################


##Plotten#####

library(effects)
plot(effect(term="Freq_Plot:Spec",mod=M.final2, default.levels=20), multiline=TRUE)

plot(effect(term="Spec",mod=M.final2, default.levels=20), multiline=TRUE)

######noch mal in schön:##############

Mlm <- glm(fFULL2, data=jena)
#kann nicht predicten mit dem lme-model :(

species <- c("Ger", "Lat", "Lot","Ono", "TP")
color <- c("#8E388E", "#C0FF3E", "#EEC900", "#EE1289", "#CD0000")
color.conf <- c("#8E388E30", "#C0FF3E30", "#EEC90030", "#EE128910", "#CD000030")
sub <- list(jena.ger, jena.lat, jena.lot,jena.ono, jena.tp)

par(mfrow=c(1,1))
plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower visitation rate", xlab="Frequency", main="Visitation rate depending on frequency")#, ylim=c(0,1))
legend("topleft", legend = unique(jena$Spec), pch=20, col=color, cex=1)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  yv <- predict(Mlm,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=2)
}

all.glm <- glm(VR_spec ~ Freq_Plot +I(Freq_Plot^2), data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(testlm,newdata=data.frame(Freq_Plot=xv))
lines(xv,yv, lwd=3, lty=2)


###################################
###################################
Apis <- lme(VR_spec_apis~ 
              Freq_Plot+
              I(Freq_Plot^2)+
              I(Freq_Plot^3)+
              Spec+
              Cover_Plot+
              SR_Plot + 
              Freq_Plot:Cover_Plot+ 
              Freq_Plot:SR_Plot+
              Freq_Plot:Spec+ 
              I(Freq_Plot^2):Spec+
              I(Freq_Plot^3):Spec+
              I(Freq_Plot^2): Cover_Plot, random=~1|PlotID/PatchID, data=jena, method="ML", weights = varSpec)
apis <- dredge(Apis)


####dritte backward selection mit ^3 #####
fALL <- formula(VR_spec ~ 
                  Freq_Plot+
                  I(Freq_Plot^2)+
                  I(Freq_Plot^3)+
                  Spec+
                  Cover_Plot+
                  SR_Plot + 
                  Freq_Plot:Cover_Plot+ 
                  Freq_Plot:SR_Plot+
                  Freq_Plot:Spec+ 
                  I(Freq_Plot^2):Spec+
                  I(Freq_Plot^3):Spec+
                  I(Freq_Plot^2): Cover_Plot)

lmeALL <- lme(fALL, 
              random=~1|PlotID/PatchID, 
              data=jena, method="ML", weights = varSpec)

M1a <- update(lmeALL, .~. -I(Freq_Plot^2): Cover_Plot) 
M1b <- update(lmeALL, .~. -I(Freq_Plot^3):Spec)
M1c <- update(lmeALL, .~. -I(Freq_Plot^2):Spec)
M1d <- update(lmeALL, .~. -Freq_Plot:Spec)
M1e <- update(lmeALL, .~. -Freq_Plot:Cover_Plot)

anova(lmeALL,M1a,M1b,M1c,M1d,M1e) #M1a, Cover:Freq^2 removed

M2a <- update(M1a, .~. -Freq_Plot:Spec)
M2b <- update(M1a, .~. -I(Freq_Plot^2):Spec)
M2c <- update(M1a, .~. -I(Freq_Plot^3):Spec)
M2d <- update(M1a, .~. -SR_Plot)
M2e <- update(M1a, .~. -Freq_Plot:Cover_Plot)

anova(M1a,M2a,M2b,M2c,M2d,M2e) #M2d no more SR

M3a <- update(M2d, .~. -Freq_Plot:Spec)
M3b <- update(M2d, .~. -I(Freq_Plot^2):Spec)
M3c <- update(M2d, .~. -I(Freq_Plot^3):Spec)
M3d <- update(M2d, .~. -Freq_Plot:Cover_Plot)

anova(M2d,M3a,M3b,M3c,M3d) #M3d Freq:Cover raus

M4a <- update(M3d, .~. -Freq_Plot:Spec)
M4b <- update(M3d, .~. -I(Freq_Plot^2):Spec)
M4c <- update(M3d, .~. -I(Freq_Plot^3):Spec)
M4d <- update(M3d, .~. -Cover_Plot)

anova(M3d,M4a,M4b,M4c,M4d) #M4d Cover auch raus

M5a <- update(M3d, .~. -Freq_Plot:Spec)
M5b <- update(M3d, .~. -I(Freq_Plot^2):Spec)
M5c <- update(M3d, .~. -I(Freq_Plot^3):Spec)

anova(M4d,M5a,M5b,M5c) # M4d bleibt das beste!

M6a <- lme(VR_spec~Freq_Plot, random=~1|PlotID/PatchID, 
           data=jena, method="ML", weights = varSpec)
M6b <- lme(VR_spec~Freq_Plot+I(Freq_Plot^2), random=~1|PlotID/PatchID, 
           data=jena, method="ML", weights = varSpec)
M6c <- lme(VR_spec~Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3), random=~1|PlotID/PatchID, data=jena, method="ML", weights = varSpec)
M6d <- lme(VR_spec~Spec+Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3), random=~1|PlotID/PatchID, data=jena, method="ML", weights = varSpec)
M6e <- lme(VR_spec~Spec+Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^2):Spec, random=~1|PlotID/PatchID, data=jena, method="ML", weights = varSpec)
M6f <- lme(VR_spec~Spec+Freq_Plot+I(Freq_Plot^2)+Freq_Plot:Spec, random=~1|PlotID/PatchID, data=jena, method="ML", weights = varSpec)

anova(M4d, M6a,M6b,M6c,M6d,M6e,M6f)  # M4d bleibt das beste!


###NEUES MODEL############
anova(M4d, M.final2) #ist auch besser als das "alte" Model
summary(M4d)

fFull3 <- formula(VR_spec~ 
                    Freq_Plot + I(Freq_Plot^2) + I(Freq_Plot^3)+ Spec +
                    Freq_Plot:Spec + I(Freq_Plot^2):Spec + I(Freq_Plot^3):Spec)

M.final3 <- lme(fFull3,random=~1|PlotID/PatchID, 
                data=jena, weights = varSpec)

M.final3ml <- lme(fFull3,random=~1|PlotID/PatchID, 
                data=jena, weights = varSpec, method="ML")

M.final3.noweight <- lme(fFull3,random=~1|PlotID/PatchID, 
                  data=jena)

plot(M.final3, main="Model Validation", ylim=c(-3.5,3.5)) #sieht ok aus, die Lücke ist auch nicht mehr so stark wie bei M.final2
plot(M.final3.noweight, main="Model Validation", ylim=c(-6,6))

mean(resid(M.final3)) #super, so gut wie Null!
mean(resid(M.final3.noweight))
summary(M.final3)
anova(M.final3)
summary(aov(M.final3))
# in eine Tabelle!
# F-Werte so riesig bei Species weil die between-group variability so hoch ist (attractive ARten vs. nicht attraktive arten.) Aber das ist ja auch gut, F darf groß sein, p klein

####plotten#####
Mlm3 <- glm (fFull3, data=jena)

par(mfrow=c(1,1))
plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower visitation rate", xlab="Frequency", main="Visitation rate depending on frequency", log="y")#, ylim=c(0,1))
legend("topleft", legend = unique(jena$Spec), pch=20, col=color, cex=1)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  yv <- predict(Mlm3,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=2)
}

all.glm3 <- glm(VR_spec ~ Freq_Plot +I(Freq_Plot^2)+ I(Freq_Plot^3), data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(all.glm3,newdata=data.frame(Freq_Plot=xv))
lines(xv,yv, lwd=3, lty=2)


####LOG_plotten#####
color <- c("darkviolet","orange", "red", "magenta", "deepskyblue4")

Mlm3 <- glm (fFull3, data=jena)

par(mfrow=c(1,1))
plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower Visitation Rate (Log-Scale)", xlab="Frequency (%)", main="Visitation Rate Depending on Frequency ", log="y")#, ylim=c(0,1))
legend("bottomright", legend = unique(jena$Spec), pch=20, col=color, cex=0.8)

for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20, cex=.8)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  yv <- predict(Mlm3,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=1)
}

all.glm3 <- glm(VR_spec ~ Freq_Plot +I(Freq_Plot^2)+ I(Freq_Plot^3), data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(all.glm3,newdata=data.frame(Freq_Plot=xv), se=TRUE)
lines(xv,yv$fit, lwd=3, lty=2)
#lines(xv,yv$fit + 2 * yv$se.fit, lty = 2, col="#404040")
#lines(xv,yv$fit - 2 * yv$se.fit, lty = 2, col="#404040")
polygon(x=c(xv, rev(xv)), y=c(yv$fit + 2 * yv$se.fit,rev(yv$fit - 2 * yv$se.fit)), col="#CDC9C950", border="#CDC9C940")

################

####APIS LOG_plotten#####


Mlm3a <- glm (VR_spec_apis~ 
               Freq_Plot + I(Freq_Plot^2) + I(Freq_Plot^3)+ 
               Spec +Freq_Plot:Spec + I(Freq_Plot^2):Spec + 
               I(Freq_Plot^3):Spec, data=jena)

par(mfrow=c(1,1))
plot(VR_spec_apis~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower visitation rate (Log-Scale)", xlab="Frequency", main="Visitation Rate Depending on Frequency ", log="y")#, ylim=c(0,1))
legend("bottomright", legend = unique(jena$Spec), pch=20, col=color, cex=0.9)

for (i in 1:5){
  points(VR_spec_apis~Freq_Plot,data=sub[[i]], col=color[i], pch=20)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  yv <- predict(Mlm3a,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=1)
}

all.glm3 <- glm(VR_spec_apis ~ Freq_Plot +I(Freq_Plot^2)+ I(Freq_Plot^3), data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(all.glm3,newdata=data.frame(Freq_Plot=xv))
lines(xv,yv, lwd=3, lty=2)

#################

par(mfrow=c(1,1))
xyplot(VR_spec ~ Freq_Plot, groups=Spec, data=jena, type=c("a","p"), auto.key=list(space="right"))

par(mfrow=c(1,1))
xyplot(Spec_visit_share ~ Freq_Plot, groups=Spec, data=jena, type=c("a","p"), auto.key=list(space="right"))

################################
#some basics

max(jena$SR_Plot) #12
min(jena$SR_Plot) #4
sd(jena$SR_Plot) #2.4
mean(jena$SR_Plot)

max(jena$SR_Patch) #8
min(jena$SR_Patch) #1
sd(jena$SR_Patch) #1.16
mean(jena$SR_Patch)

max(jena$VR_spec)
min(jena$VR_spec)
sd(jena$VR_spec)
mean(jena$VR_spec)

max(jena$Sum_spec_visits)
min(jena$Sum_spec_visits)
sd(jena$Sum_spec_visits)
mean(jena$Sum_spec_visits)

#####
lmeSpec <- lme(VR_spec~Spec,random=~1|PlotID/PatchID, data=jena, weights = varSpec)
summary(glht(lmeSpec, linfct=mcp(Spec="Tukey")))
plot(lmeSpec)
plot(VR_spec~Spec, data=jena)
######

Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3)

latdd<- dredge(lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3),random=~1|PlotID/PatchID, data=jena.lat, method="ML"))

lotdd<- dredge(lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3),random=~1|PlotID/PatchID, data=jena.lot, method="ML")) #nur Freq

gerdd<- dredge(lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3),random=~1|PlotID/PatchID, data=jena.ger, method="ML")) #bis ^3

onodd<- dredge(lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3),random=~1|PlotID/PatchID, data=jena.ono, method="ML")) #bis ^3

tpdd<- dredge(lme(VR_spec ~ Freq_Plot+I(Freq_Plot^2)+I(Freq_Plot^3),random=~1|PlotID/PatchID, data=jena.tp, method="ML")) #nur freq

plot(VR_spec ~ Freq_Plot, data=jena.tp)
plot(VR_spec ~ Freq_Plot, data=jena.lat)
plot(VR_spec ~ Freq_Plot, data=jena.lot)
plot(VR_spec ~ Freq_Plot, data=jena.ger)
summary(lm(VR_spec ~ Freq_Plot, data=jena.ono)) #Lot nicht significant, alle anderen schon

confint(M.final3, level=0.95)
prettify(anova(M.final3))
summary(aov(M.final3))
anova(M.final3)
