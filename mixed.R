# Selected Data Analysis
# Create group-centered and standardized variables
# Run a series of models
# Evaluate models graphically using:
# plots of residuals (or mean of residuals) against 1) fitted values 2) predictor variables

library(plyr)
library(arm)
library(Hmisc)

                 
tempDFzms <- ddply(tempDF, c("male", "sschlcde2"), transform, acent10.zms = scale(acent10))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, noutnom.zms = scale(NOUTNOM))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, age.zms = scale(AGE))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, grade.zms = scale(GRADE))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, sportpop.zms = scale(SPORTPOP))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, smoked.zms = scale(SMOKED))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, alcohold.zms = scale(ALCOHOLD))
tempDFzms <- ddply(tempDFzms, c("male", "sschlcde2"), transform, drunkd.zms = scale(DRUNKD))


m6d <- merge(m6d, tempDFzms[c("aid2", "acent10.zms" )], by="aid2", all=TRUE)
m6d <- merge(m6d, tempDFzms[c("aid2", "acent10m90l.zms", "acent10f90l.zms" )], by="aid2", all=TRUE)
names(m6d)


m6m <- m6d[which(!is.na(m6d$acent10) & m6d$male==1),]
m6m <- m6m[sample(1:nrow(m6s), 25000, replace=FALSE),]
                   

grep([gpa], names(m6s))
grep([effort], names(m6s))
grep("^effort$", names(m6s))
names(m6s[grep("^GPA$", names(m6s))])
                   

                   
#m5d <- merge(m5d, tempDFzms[c("aid2", "gpahav.zms")], by="aid2", all=TRUE)
#m5d$gpahav.zms <- tempDFzms$gpahav.zms[match(m5d$aid2, tempDFzms$aid2)]
#save.image("J:/R/m6d.RData")

lm.ac <- lmer(m6d$acent10 ~ 1 + (1 | fsschlcde2), data=m6d)
display(lm.ac)
lm.ac <- lmer(m6d$acent10.zms ~ 1 + (1 | fsschlcde2), data=m6d, na.action="na.exclude")
display(lm.ac)
lm.ac1 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + gpahav.zms, data=m6s, na.action="na.exclude")
display(lm.ac1)
lm.ac2 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + gpahav.zms + gpahav.zms2, data=m6s, na.action="na.exclude")
display(lm.ac2)
lm.ac3 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + gpahav.zms + gpahav.zms2 + gpahav.zms3, data=m6s, na.action="na.exclude")
display(lm.ac3)
                   
lm.ac1b <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + GPAHAV, data=m6s, na.action="na.exclude")
display(lm.ac1b)
lm.ac2b <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + GPAHAV + gpahav2, data=m6s, na.action="na.exclude")
display(lm.ac2b)
lm.ac3b <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + GPAHAV + gpahav2 + gpahav3, data=m6s, na.action="na.exclude")
display(lm.ac3b)    

m6s$f.gpahav <- factor(m6s$GPAHAV)
                   
lm.ac1 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + gpahav.zms, data=m6s, na.action="na.exclude")
display(lm.ac1)
lm.ac2 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + gpahav.zms + gpahav.zms2, data=m6s, na.action="na.exclude")
display(lm.ac2)
lm.ac3 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + gpahav.zms + gpahav.zms2 + gpahav.zms3, data=m6s, na.action="na.exclude")
display(lm.ac3)
                   
lm.ac1b <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + GPAHAV, data=m6s, na.action="na.exclude")
display(lm.ac1b)
lm.ac2b <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + GPAHAV + gpahav2, data=m6s, na.action="na.exclude")
display(lm.ac2b)
lm.ac3b <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + GPAHAV + gpahav2 + gpahav3, data=m6s, na.action="na.exclude")
display(lm.ac3b)    



lm.ac4 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + GPAHAV, data=m6s, na.action="na.exclude" )
lm.ac5 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + INHOME + GPAHAV, data=m6s, na.action="na.exclude" )
lm.ac6 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + INHOME + male + GPAHAV, data=m6s, na.action="na.exclude" )
lm.ac7 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + INHOME + male*GPAHAV, data=m6s, na.action="na.exclude" )
lm.ac8 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + INHOME*GPAHAV + male + GPAHAV, data=m6s, na.action="na.exclude" )
lm.ac9 <- lmer(m6s$acent10.zms ~ 1 + (1 | fsschlcde2) + INHOME*male*GPAHAV, data=m6s, na.action="na.exclude" )

lm.ac10 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + INHOME*male*gpahav.zms, data=m6s, na.action="na.exclude" )
                   
display(lm.ac4)
display(lm.ac5)
display(lm.ac6)

qplot(factor(m6s$GPAHAV), residuals(lm.ac4), geom = "boxplot")
    

boxplot(residuals(lm.ac4) ~ factor(m6s$GPAHAV))

par2 <- par(cex=1, cex.main=1.2, cex.axis=1, cex.lab=1.1, lwd=2, font=2, font.lab=2, font.axis=2, las=1)
par3 <- par(cex=1, cex.main=1.2, cex.axis=1, cex.lab=1.1, lwd=1, font=2, font.lab=2, font.axis=2, las=.3, pch=1)
par3 <- par(cex=1, cex.main=1.2, cex.axis=1, cex.lab=1.1, lwd=1, font=2, font.lab=2, font.axis=2, las=.5)
png("J:/R/Graphs/mean_IDGX2_Race_GPA_uncond.png")
plotMeans(resid(lm.ac4), m6s$f.gpahav, error.bars="none")
plotMeans(resid(lm.ac4), m6s$f.gpahav, error.bars="se")
plotMeans(resid(lm.ac5), m6s$f.gpahav, error.bars="se")
plotMeans(resid(lm.ac6), m6s$f.gpahav, error.bars="se")
plotMeans(resid(lm.ac6), m6s$f.gpahav, error.bars="none")
dev.off()
plotMeans(resid(lm.ac4), m6s$f.gpahav, factor(m6s$INHOME), par(par2), error.bars="none")
plotMeans(resid(lm.ac4), m6s$f.gpahav, par(par3), error.bars="none")
plotMeans(resid(lm.ac4), m6s$f.gpahav, factor(m6s$INHOME), par(par4), error.bars="none")
plotMeans(resid(lm.ac4), m6s$f.gpahav, error.bars="none")
                   
#aggregate(resid(lm.ac4), by=list(m6s$f.gpahav), FUN=mean)
names(tempDFzms)
describe(as.numeric(tempDFzms$gpahav.zms))
describe(tempDFzms$gpa.n.zms)
lm.lac1 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + race5f + INHOME + gpahav.zms, data=m6s, na.action="na.exclude" )
lm.lac1 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + race5f*gpahav.zms + INHOME, data=m6s, na.action="na.exclude" )
lm.lac1 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + (race5f | fsschlcde2) + gpahav.zms + INHOME, data=m6s, na.action="na.exclude" )
display(lm.lac1)
                   
lm.lac2 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + race5f + (race5f | fsschlcde2) + gpahav.zms + INHOME, data=m6s, na.action="na.exclude" )
display(lm.lac2)
                   
lm.lac3 <- lmer(m6s$acent10l.zms ~ 1 + (1 | fsschlcde2) + race5f*gpahav.zms + (race5f*gpahav.zms | fsschlcde2) + gpahav.zms + INHOME, data=m6s, na.action="na.exclude" )
display(lm.lac3)

                 
                   
glm4 <- glm(IDGX2 ~ (1|SCODE) + AGE + GRADE + YRINSC + OSCHTP + NOUTNOM +
 OSCHNXP + NOUTNOMM  + HISPAN3M + HISPAN3W + BLACKOD + ASIANOD + RGROUP4D + 
 CLBACADD + CLBMUSD + SPORTPD + CLUBMIS + S44 + LWOMOM + FORMOM + MNOWRK + LWODAD + FORDAD + DNOWRK + MOMED.N + MOMEDMIS + SICK + SKIN + TTEACH0 + TTEACH2 + TTEACH3 + TTEACH4 + TATTENT0 + TATTENT2 + TATTENT3 + TATTENT4 + THMWK0 + THMWK2 + THMWK3 +  THMWK4 + TV0 + TV2 + TV3 + TV4 + GPAHCAT1 + GPAHCAT2 + GPAHCAT3 + GPAHCAT4 +  GPAHCAT6 + GPAHCAT7 + GPAHCAT8 + SMOKED + ALCOHOLD + DRUNKD  + EFFORTD1 + EFFORTD2 + EFFORTD4, family=quasipoisson, subset = MALE==1)

                   
                   
tapply(resid(lm.ac4), m6s$f.gpahav, function(x) mean(x))
r.comp <- data.frame(gpahav = levels(m6s$f.gpahav), rlm.ac4 <- tapply(resid(lm.ac4), m6s$f.gpahav, mean))

r.comp <- rbind(data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac4), m6s$f.gpahav, mean), spec= rep("lm.ac4", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac9), m6s$f.gpahav, mean), spec= rep("lm.ac9", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac10), m6s$f.gpahav, mean), spec= rep("lm.ac10", times=length(levels(m6s$f.gpahav)))))

r.comp2 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac1), m6s$f.gpahav, mean), spec= rep("lm.ac1", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac2), m6s$f.gpahav, mean), spec= rep("lm.ac2", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac3), m6s$f.gpahav, mean), spec= rep("lm.ac3", times=length(levels(m6s$f.gpahav)))))

r.comp3 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav)), na.rm=TRUE), 
                     resid = tapply(resid(lm.ac3), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), spec= rep("lm.ac3", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac3b), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), spec= rep("lm.ac3b", times=length(levels(m6s$f.gpahav)))))

                   
r.comp3 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(factor(m6s$gpahav.zms)))), 
                     resid = tapply(resid(lm.ac3), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), spec= rep("lm.ac3", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac3b), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), spec= rep("lm.ac3b", times=length(levels(m6s$f.gpahav)))))



r.comp3 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(factor(m6s$gpahav.zms)))), 
                     resid = tapply(resid(lm.ac3), factor(m6s$gpahav.zms), function(x) mean(x, na.rm=TRUE)), 
                            spec= rep("lm.ac3", times=length(levels(factor(m6s$gpahav.zms))))),
                data.frame(gpahav = as.numeric(as.character(levels(factor(m6s$gpahav.zms)))), 
                     resid = tapply(resid(lm.ac3b), factor(m6s$gpahav.zms), function(x) mean(x, na.rm=TRUE)), 
                           spec= rep("lm.ac3b", times=length(levels(factor(m6s$gpahav.zms))))))
                   

r.comp3 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(factor(m6s$gpahav.zms)))), 
                     fitted = tapply(fitted(lm.ac3), factor(m6s$gpahav.zms), function(x) mean(x, na.rm=TRUE)), 
                            spec= rep("lm.ac3", times=length(levels(factor(m6s$gpahav.zms))))),
                data.frame(gpahav = as.numeric(as.character(levels(factor(m6s$gpahav.zms)))), 
                     fitted = tapply(fitted(lm.ac3b), factor(m6s$gpahav.zms), function(x) mean(x, na.rm=TRUE)), 
                           spec= rep("lm.ac3b", times=length(levels(factor(m6s$gpahav.zms))))))

                   
r.comp3 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac3), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), 
                            spec= rep("lm.ac3", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     resid = tapply(resid(lm.ac3b), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), 
                           spec= rep("lm.ac3b", times=length(levels(m6s$f.gpahav)))))

                   
r.comp3 <- rbind(data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     fitted = tapply(fitted(lm.ac3), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), 
                            spec= rep("lm.ac3", times=length(levels(m6s$f.gpahav)))),
                data.frame(gpahav = as.numeric(as.character(levels(m6s$f.gpahav))), 
                     fitted = tapply(fitted(lm.ac3b), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE)), 
                           spec= rep("lm.ac3b", times=length(levels(m6s$f.gpahav)))))
m6s$f.gpahav <- factor(m6s$GPAHAV)
                   
qplot()
   (m6s$f.gpahav)
tapply(resid(lm.ac3), m6s$f.gpahav, function(x) mean(x, na.rm=TRUE))                   

qplot(resid(lm.ac6))
qplot(density(resid(lm.ac6)))

qplot(gpahav, resid, data=r.comp, color= spec)
qplot(gpahav, resid, data=r.comp, color= spec, geom=c("point"))
qplot(gpahav, resid, data=r.comp, color= spec, geom=c("point", "smooth"))
qplot(gpahav, resid, data=r.comp2, color= spec, geom=c("point", "smooth"))
qplot(gpahav, resid, data=r.comp3, color= spec, geom=c("point", "smooth"))
qplot(gpahav, fitted, data=r.comp3, color= spec, geom=c("point", "smooth"))
qplot(gpahav, resid, data=r.comp4, color= spec, geom=c("point", "smooth"))

qplot(gpahav.zms, data=m6s, geom=c("point", "smooth"))
qplot(gpahav.zms, data=m6s, geom=c("point"))
                   
describe(r.comp$spec)
                   
qplot(r.comp$gpahav, r.comp$rlm.ac4)
                   
qplot(r.comp$f.gpahav, r.comp$rlm.ac4)
tapply(resid(lm.ac4), m6s$f.gpahav, function(x) mean(x))

qplot(levels(m6s$f.gpahav), tapply(resid(lm.ac4), m6s$f.gpahav, mean))
p <- ggplot(,aes(levels(m6s$f.gpahav), tapply(resid(lm.ac4), m6s$f.gpahav, mean), geom="point"))

p + layer(ggplot(,aes(levels(m6s$f.gpahav), tapply(resid(lm.ac6), m6s$f.gpahav, mean), geom="point")))
                   
qplot(levels(m6s$f.gpahav), tapply(resid(lm.ac4), m6s$f.gpahav, mean), geom=c("point", "smooth"))

qplot(levels(m6s$f.gpahav), tapply(resid(lm.ac4), m6s$f.gpahav, mean), tapply(resid(lm.ac6), m6s$f.gpahav, mean), geom=c("point", "smooth"))

qplot(levels(m6s$f.gpahav), tapply(resid(lm.ac4), m6s$f.gpahav, mean), tapply(resid(lm.ac6), m6s$f.gpahav, mean), geom=c("point"), aes(group=1))
tapply(resid(lm.ac4), m6s$f.gpahav, mean)
                   
