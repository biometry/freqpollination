#packages

install.packages("nlme")
install.packages("MASS")
install.packages("mgcv")
install.packages("lattice")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("MuMIn")

library(MASS)
library(mgcv)
library(nlme)
library(lattice)
library(ggplot2)
library(multcomp)
library(MuMIn)

#data and adjustments

jena <- read.csv2("C:/Users/hczioska/Documents/GitHub/Data/jena/data1.csv", header=T, sep= ";")
names (jena)

jena$PatchID <- as.factor(jena$PatchID)
jena$Instead <- as.factor(jena$Instead)
jena$PatchID <- as.factor(jena$PatchID)
jena$Date <- as.POSIXct(paste(jena$Year, jena$Month, jena$Day, sep=" "),
                        format="%Y %m %d") # Datum in R-Format

#possible response-variables:
jena$Sum_all_visits <- jena$Bees_spec + jena$Bees_other + 
  jena$Bombus_spec + jena$Bombus_other + jena$Hoverfly_spec +
  jena$Hoverfly_other + jena$Rest_spec + jena$Rest_other

jena$Sum_spec_visits <- jena$Bees_spec  + jena$Bombus_spec  + 
  jena$Hoverfly_spec  + jena$Rest_spec

jena$Spec_visit_share <- jena$Sum_spec_visits/jena$Sum_all_visits

jena$VR_spec <- jena$Sum_spec_visits/jena$Number_Flowers

jena$VR_spec_apis <- jena$Bees_spec/jena$Number_Flowers
jena$freqshare <- jena$Freq_Plot / 100
str(jena)

write.csv(jena,"C:/Users/hczioska/Documents/GitHub/Data/jena/jena.csv", row.names=F)

#some basics on the data

length(unique(jena$Date)) #15 Sampling-Days
length(unique(jena$PlotID)) #23 different Plots

max(jena$SR_Plot) #12
min(jena$SR_Plot) #4
sd(jena$SR_Plot) #2.4
mean(jena$SR_Plot)

max(jena$SR_Patch) #8
min(jena$SR_Patch) #1
sd(jena$SR_Patch) #1.16
mean(jena$SR_Patch)

length(jena [which(jena$VR_spec == 0),])

max(jena.lat$VR_spec)
min(jena.lat$VR_spec)
sd(jena.lat$VR_spec)
mean(jena.lat$VR_spec)
nrow(jena.lat)

pairwise.t.test(jena$VR_spec,jena$Spec)

max(jena$Sum_spec_visits)
min(jena$Sum_spec_visits)
sd(jena$Sum_spec_visits)
mean(jena$Sum_spec_visits)

#data-subsets

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

###########################
###ANALYSIS################
###########################
# following Zuur 2009

# 1. check for colinearity

z <- cbind(jena$VR_spec, jena$Spec, jena$Freq_Plot, 
           jena$Cover_Plot, jena$SR_Plot, jena$Freq_Patch, 
           jena$Cover_Patch, jena$SR_Patch)

colnames(z) <- c("Visitation Rate" , "Species",  "Frequency", 
                 "Cover", "Species Richness" , "Freqency (Sub)", 
                 "Cover (Sub)", "SR (Sub)")

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
colnames(z) <- c("Visitation_Rate" , "Species",  "Frequency", 
                 "Cover", "Species_Richness" , "Freqency_Sub", 
                 "Cover_Sub", "SR_Sub")

source("C:/Users/hczioska/Documents/HighstatLibV6.R") #corvif aufrufen
corvif(z)

#"All VIF values are below 3 (see Chapter 26 in Zuur et al. (2007)), indicating there is no collinearity in these variables"

# 2. The beyond optimal model

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
                  I(Freq_Plot^2):Cover_Plot)

#I am using a mixed model because I have 8 observation per Plot.The data is therefore not independent but nested. 

lmeALL <- lme(fALL,random=~1|PlotID/PatchID, data=jena)

# 3. Model Validation Nr. 1

plot(lmeALL)
#Problem: Strong Heterogeneity / Heteroscedasticity!!
#violates the homogeneity of variance assumption
#MFollowing Zuur 2009 chapter 4 (p.72ff & Appendix p. 535) ---> Adjestment of weights

# 4a. Applying different weights

M.original <- gls(fALL, data=jena)

fixed <- varFixed(~Freq_Plot)
M1.fixed <- gls(fALL, data=jena, weights=fixed)
#Not possible for Spec because it is a factor

varFreq <- varIdent(form=~1|Freq_Plot)
M1.varFreq <- gls(fALL, data=jena, weights=varFreq)

varSpec <- varIdent(form=~1|Spec)
M1.varSpec <- gls(fALL, data=jena, weights=varSpec)
#each species is allowed to have a different variance

varCov <- varIdent(form=~1|Cover_Plot)
M1.varCov <- gls(fALL, data=jena, weights=varCov)
#each species is allowed to have a different variance

varP <-varPower(form=~Freq_Plot|Spec)
M1.varP <- gls(fALL, data=jena, weights=varP)

varPf <-varPower(form=~Freq_Plot)
M1.varPf <- gls(fALL, data=jena, weights=varPf)

varE <- varExp(form=~Freq_Plot)
M1.varE <- gls(fALL, data=jena, weights=varE)

varCP <- varConstPower(form=~Freq_Plot)
M1.varCP <- gls(fALL, data=jena, weights=varCP)

varCPs <- varConstPower(form=~Freq_Plot|Spec)
M1.varCPs <- gls(fALL, data=jena, weights=varCPs)
#Constant plus power of the variance covariate

varCom <- varComb(varIdent(form=~1|Spec),varExp(form=~Freq_Plot))
M1.varCom <- gls(fALL, data=jena, weights=varCom)

varCom2 <- varComb(varFixed(~Freq_Plot), varIdent(form=~1|Spec))
M1.varCom2 <- gls(fALL, data=jena, weights=varCom2)

# 4b. Results weights 

AIC(M.original,M1.fixed, M1.varFreq, M1.varSpec, M1.varCov, 
    M1.varP, M1.varPf, M1.varE, M1.varCP, M1.varCPs, 
    M1.varCom, M1.varCom2) 

#best is varCPs, second varSpec (varIdent)
#Ecologically it makes sense to choose varIdent! Spec is a nominal variable with very different values for each species. VarIdent allowes different residual variation for the visitation rate per focal species

#Include it in the mixed model:
M1 <- lme(fALL, random=~1|PlotID/PatchID, 
          data=jena, weights = varSpec)
M2 <- lme(fALL, random=~1|PlotID/PatchID, 
          data=jena, weights=varCPs) #does not work, too few data

anova(lmeALL,M1) 

plot(M1)
plot(lmeALL)
#great difference, weightingmakes a significantly better variance structure!
# L= 383.7 (df=4, p<0.0001)
#No more Heteroscedasticity

# 5. Model Selection

lmeALL <- lme(fALL, 
              random=~1|PlotID/PatchID, 
              data=jena, method="ML", weights = varSpec)

#Important to choose the method "Maximum Likelihood" here because we are comparing models with different fixed effect structure. REML (default) is not possible.

# 5a. Global Model Selection by MiMIn

global.selection <- dredge(lmeALL , extra= alist(AIC))

global.selection[1:10] #only show the first 10 results

# 5b. Backward Selection

M1a <- update(lmeALL, .~. -I(Freq_Plot^2): Cover_Plot) 
M1b <- update(lmeALL, .~. -I(Freq_Plot^3):Spec)
M1c <- update(lmeALL, .~. -Freq_Plot:SR_Plot)

anova(lmeALL,M1a,M1b,M1c) #M1a, Cover:Freq^2 removed

M2a <- update(M1a, .~. -I(Freq_Plot^3):Spec)
M2b <- update(M1a, .~. -Freq_Plot:SR_Plot)
M2c <- update(M1a, .~. -Freq_Plot:Cover_Plot)

anova(M1a,M2a,M2b,M2c) #M2c Freq_Plot:Cover_Plot removed

M3a <- update(M2c, .~. -Freq_Plot:Spec)
M3b <- update(M2c, .~. -I(Freq_Plot^2):Spec)
M3c <- update(M2c, .~. -I(Freq_Plot^3):Spec)
M3d <- update(M2c, .~. -Freq_Plot:SR_Plot)
M3e <- update(M2c, .~. -Cover_Plot)

anova(M2c,M3a,M3b,M3c,M3d,M3e) #M3e Cover_Plot removed

M4a <- update(M3e, .~. -Freq_Plot:Spec)
M4b <- update(M3e, .~. -I(Freq_Plot^2):Spec)
M4c <- update(M3e, .~. -I(Freq_Plot^3):Spec)
M4d <- update(M3e, .~. -Freq_Plot:SR_Plot)

anova(M3d,M4a,M4b,M4c,M4d) #M4d Freq_Plot:SR_Plot removed

M5a <- update(M4d, .~. -Freq_Plot:Spec)
M5b <- update(M4d, .~. -I(Freq_Plot^2):Spec)
M5c <- update(M4d, .~. -I(Freq_Plot^3):Spec)
M5d <- update(M4d, .~. -SR_Plot)

anova(M4d,M5a,M5b,M5c,M5d) # M5d SR_Plot removed

M6a <- update(M5d, .~. -Freq_Plot:Spec)
M6b <- update(M5d, .~. -I(Freq_Plot^2):Spec)
M6c <- update(M5d, .~. -I(Freq_Plot^3):Spec)

anova(M5d,M6a,M6b,M6c) # M5d stays the best

#Same result as global selection!

#6. THE FINAL MODEL

fFinal <- formula(VR_spec~ Spec+ 
                    Freq_Plot+
                    I(Freq_Plot^2)+
                    I(Freq_Plot^3)+ 
                    Freq_Plot:Spec+
                    I(Freq_Plot^2):Spec+
                    I(Freq_Plot^3):Spec)

M.final <- lme(fFinal,random=~1|PlotID/PatchID, 
                data=jena, weights = varSpec)

M.final.ml <- lme(fFinal,random=~1|PlotID/PatchID, 
                  data=jena, weights = varSpec, method="ML")

M.final.noweight <- lme(fFinal,random=~1|PlotID/PatchID, 
                         data=jena)

M.final.onerandom <- lme(fFinal,~1|PlotID, 
                         data=jena, weights = varSpec)

M.final.NOrandom <- gls(fFinal,data=jena, weights = varSpec)

anova(M.final,M.final.noweight)

#validating random effect structure:
AIC(M.final.NOrandom,M.final.onerandom,M.final)
#                   df      AIC
#M.final.NOrandom  25 987.6298
#M.final.onerandom 26 989.6279
#M.final           27 983.6561

#7. Model Validation Nr. 2

plot(M.final, main="Model Validation", ylim=c(-3.5,3.5)) 
#looks ok

plot(M.final.noweight, main="Model Validation", ylim=c(-6,6))
# weighting makes big difference

mean(resid(M.final)) #should be zero --> good

# 8. Summary & Interpretation

summary(M.final)
anova(M.final)

# numDF denDF   F-value p-value
# (Intercept)             1   191 213.68824  <.0001
# Spec                    4   191 141.13918  <.0001
# Freq_Plot               1   191  18.29208  <.0001
# I(Freq_Plot^2)          1   191   9.29896  0.0026
# I(Freq_Plot^3)          1   191   0.05521  0.8145
# Spec:Freq_Plot          4   191   5.20801  0.0005
# Spec:I(Freq_Plot^2)     4   191   3.43666  0.0097
# Spec:I(Freq_Plot^3)     4   191   3.41020  0.0101

#F-Values are high because the between-group variability in the species is high (attractive species vs. non-attractive species), high F-values are good

r.squaredGLMM(M.final.onerandom)
#R2m (marginal) : 0.5329026 
#--> represents the variance explained by fixed factors
#R2c (conditional): 0.5341637 
#--> both fixed and random factors (i.e. the entire model)

# 9. Plotting

Mlm <- glm(fFinal, data=jena)
sub <- list(jena.ger, jena.lat, jena.lot,jena.ono, jena.tp)
color <- c("cornflowerblue", "blue4", "maroon", "red", "orange")

par(mfrow=c(1,1))
plot(VR_spec~Freq_Plot, data=jena, xlim=c(0,100), type="n", ylab="Per-Flower Visitation Rate [Log-Scale]", xlab="Frequency [%]", main="Visitation Rate Depending on Frequency ", log="y")
legend("bottomright", legend = unique(jena$Spec), pch=20, col=color, cex=0.9, bg="white")
for (i in 1:5){
  points(VR_spec~Freq_Plot,data=sub[[i]], col=color[i], pch=20, cex=.8)
  xv <- data.frame(Freq_Plot = jena$Freq_Plot, Spec=species[i])
  xv <- xv[order(xv$Freq_Plot),]
  yv <- predict(Mlm,newdata=xv)
  lines(xv$Freq_Plot,yv, col=color[i], lwd=1)
}

all.glm <- glm(VR_spec ~ Freq_Plot +I(Freq_Plot^2)+ I(Freq_Plot^3), data=jena)
xv <- seq(min(jena$Freq_Plot),max(jena$Freq_Plot),1)
yv<-predict(all.glm,newdata=data.frame(Freq_Plot=xv), se=TRUE)
polygon(x=c(xv, rev(xv)), y=c(yv$fit + 2 * yv$se.fit,rev(yv$fit - 2 * yv$se.fit)), col="#CDC9C960", border="#CDC9C940")
lines(xv,yv$fit, lwd=3, lty=2)
