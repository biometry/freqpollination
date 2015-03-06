jenaNA <- jena[!is.na(jena$Spec_visit_share),]

tf <- formula(Spec_visit_share~freqshare+I(freqshare)^2+I(freqshare)^3+ Spec+freqshare:Spec+I(freqshare)^2:Spec+I(freqshare)^3:Spec)

glmALL <- glm(tf,data=jenaNA)
testdd <- dredge(glmALL)
testdd [1:10] #freq and spec, no interactions
options(na.action="na.omit")

glm.final <- glm(Spec_visit_share~freqshare+Spec,data=jenaNA)
summary(glm.final)

# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  0.41302    0.03694  11.181  < 2e-16 ***
#   freqshare    0.58871    0.05801  10.149  < 2e-16 ***
#   SpecLat     -0.11626    0.04550  -2.555    0.011 *  
#   SpecLot     -0.34997    0.04570  -7.657 1.74e-13 ***
#   SpecOno      0.02693    0.05759   0.468    0.640    
#   SpecTP      -0.33760    0.04655  -7.252 2.49e-12 ***


test2 <- glm (Spec_visit_share~freqshare, data=jena)
color <- c("darkviolet","orange", "red", "magenta", "deepskyblue4")
sub <- list(jena.ger, jena.lat, jena.lot,jena.ono, jena.tp)
par(mfrow=c(1,1))
plot(Spec_visit_share~freqshare, data=jena, xlim=c(0,1), type="n")
legend("topleft", legend = unique(jena$Spec), pch=20, col=color, cex=0.7)

for (i in 1:5){
  points(Spec_visit_share~freqshare,data=sub[[i]], col=color[i], pch=20)
  xv <- data.frame(freqshare = jena$freqshare, Spec=species[i], cex=0.5)
  xv <- xv[order(xv$freqshare),]
  test2 <- glm(Spec_visit_share~freqshare, data=sub[[i]])
  yv <- predict(test2,newdata=xv)
  lines(xv$freqshare,yv, col=color[i], lwd=1)
}
abline(0,1, lty=2)
