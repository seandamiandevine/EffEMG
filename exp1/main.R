
library(lme4)
se  = function(x) sd(x,na.rm=T)/sqrt(length(x))

# Load data ---------------------------------------------------------------

files = list.files('data/', pattern = ".csv")
dat   = do.call(rbind, lapply(paste0('data/', files), read.csv, header = T, stringsAsFactors = F))
dat   = dat[dat$block!='Practice', ]
dat$trial    = c(sapply(unique(dat$id), function(x) 1:nrow(dat[dat$id==x,])))
dat$cued_c   = ifelse(dat$cued==1, 1, -1)
dat$efflev_c = dat$efflev - mean(unique(dat$efflev))

# Descriptives ------------------------------------------------------------

N     = length(unique(dat$id))
pFem  = mean(sapply(unique(dat$id), function(x) grepl('f', tolower(dat[dat$id==x, 'sex'][1]))))
mAge  = mean(unique(dat$age))
sdAge = sd(unique(dat$age))

capture.output(list(N=N, pFem=pFem, mAge=mAge, sdAge=sdAge), file='out/demo.txt')

# Sanity checks -----------------------------------------------------------

# Accuracy hist
macc = tapply(dat$acc, dat$id, mean, na.rm=T)
hist(macc, xlim=c(0,1), xlab='Mean Accuracy', main='')
abline(v=.33,lty=2)
legend('topleft',bty='n',lty=2,legend='Chance')


# Performance and effort level --------------------------------------------

# RT
png('plots/RT.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

bcor = dat[dat$acc==1, ]

mRT    = tapply(bcor$RT, list(bcor$cued, bcor$efflev), mean)
seRT   = tapply(bcor$RT, list(bcor$cued, bcor$efflev), se)
yrange = range(pretty(c(mRT-seRT, mRT+seRT+.4)))
  
plot(colnames(mRT), mRT[1,], type='b', xlab = 'Effort Level', ylab = 'Avg. Correct RT (in s.)', xaxt='n', 
     ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
lines(colnames(mRT), mRT[2,], type='b', lty=2)
axis(1, at=colnames(mRT), labels=colnames(mRT), cex.axis=1.5)
arrows(1:4, mRT[1,]-seRT[1,], 1:4, mRT[1,]+seRT[1,], length=0)
arrows(1:4, mRT[2,]-seRT[2,], 1:4, mRT[2,]+seRT[1,], length=0)
legend('topleft', bty='n', pch=c(16,1), lty=1:2, legend=c('Uncued', 'Cued'), title='Trial Type')
legend('topright', bty='n', legend='A', cex=2)

dev.off()

RTMLM0 = lmer(RT~1 + (1|id), data=bcor)
RTMLM1 = lmer(RT~efflev + (1|id), data=bcor)
RTMLM2 = lmer(RT~efflev + cued_c + (1|id), data=bcor)
RTMLM3 = lmer(RT~efflev + cued_c + (efflev|id), data=bcor)
RTMLM4 = lmer(RT~efflev * cued_c + (efflev|id), data=bcor)

comp = anova(RTMLM0, RTMLM1, RTMLM2, RTMLM3, RTMLM4)
sum  = summary(RTMLM4)
capture.output(list(aov=comp, summary=sum), file='out/RTMLM.txt')
sjPlot::tab_model(RTMLM4, file = 'out/RTMLM.html')

# acc
png('plots/acc.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

macc  = tapply(dat$acc, list(dat$cued, dat$efflev), mean)
seacc = tapply(dat$acc, list(dat$cued, dat$efflev), se)
range(pretty(c(macc-seacc, macc+seacc)))

plot(colnames(macc), macc[1,], type='b', xlab = 'Effort Level', ylab = 'p(Correct)', xaxt='n', 
     ylim=c(0,1), pch=16, cex.axis=1.5, cex.lab=1.5)
lines(colnames(macc), macc[2,], type='b', lty=2)
axis(1, at=colnames(macc), labels=colnames(macc), cex.axis=1.5)
arrows(1:4, macc[1,]-seacc[1,], 1:4, macc[1,]+seacc[1,], length=0)
arrows(1:4, macc[2,]-seacc[2,], 1:4, macc[2,]+seacc[1,], length=0)
legend('topright', bty='n', legend='B', cex=2)
dev.off()

accMLM0 = glmer(acc~1 + (1|id), data=dat, family='binomial')
accMLM1 = glmer(acc~efflev + (1|id), data=dat, family='binomial')
accMLM2 = glmer(acc~efflev+cued_c + (1|id), data=dat, family='binomial')
accMLM3 = glmer(acc~efflev*cued_c + (1|id), data=dat, family='binomial')
accMLM4 = glmer(acc~efflev+cued_c + (efflev_c|id), data=dat, family='binomial') # singular

comp = anova(accMLM0, accMLM1, accMLM2, accMLM3, accMLM4)
sum  = summary(accMLM3)
capture.output(list(aov=comp, summary=sum), file='out/accMLM.txt')
sjPlot::tab_model(accMLM3, transform = NULL,file = 'out/accMLM.html')

# Cor -----------------------------------------------------------------

# Problem
png('plots/probcor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probcor_z = c(sapply(unique(dat$id), function(x) scale(dat$probcor[dat$id==x]) ))

mSig  = tapply(dat$probcor_z, list(dat$cued,dat$efflev), mean, na.rm=T)
seSig = tapply(dat$probcor_z,  list(dat$cued,dat$efflev), se)
yrange = range(pretty(c(mSig+seSig+.1, mSig-seSig)))

plot(mSig[1,], type = 'b', ylim=yrange,xaxt='n',xlab='Effort Level', ylab='Corrugator (Z-Score)', main='During Problem', 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
lines(mSig[2,],type='b',lty=2)
axis(1,at=1:4,labels=1:4, cex.axis=1.5)
arrows(1:4,mSig[1,]-seSig[1,],1:4,mSig[1,]+seSig[1,],length=0)
arrows(1:4,mSig[2,]-seSig[2,],1:4,mSig[2,]+seSig[2,],length=0)
#legend('bottomright',bty='n', lty=1:2, pch=1, legend=c('Uncued', 'Cued'))
legend('topright', bty='n', legend='C', cex=2)
dev.off()

probcor_lm = lm(probcor_z ~ efflev_c * cued_c, data=dat)
probcor_m0 = lmer(probcor_z ~ 1 + (1|id),data=dat)
probcor_m1 = lmer(probcor_z ~ efflev + (1|id),data=dat)
probcor_m2 = lmer(probcor_z ~ efflev_c + cued_c + (1|id),data=dat)
probcor_m3 = lmer(probcor_z ~ efflev_c * cued_c + (1|id),data=dat)

probcoraov = anova(probcor_m0, probcor_m1, probcor_m2, probcor_m3)
probcorsum = summary(probcor_m1)
sjPlot::tab_model(probcor_m3, file = 'out/probcorMLM.html')

# Cue
png('plots/cuecor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuecor_z = c(sapply(unique(dat$id), function(x) scale(dat$cuecor[dat$id==x]) ))

mSig  = tapply(dat$cuecor_z, dat$efflev, mean, na.rm=T)
seSig = tapply(dat$cuecor_z, dat$efflev, se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

plot(mSig, type = 'b', ylim=yrange,xaxt='n',xlab='Effort Level', ylab='Corrugator (Z-Scored)', main='During Cue', 
    cex.lab=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=2)
axis(1,at=1:4,labels=1:4, cex.axis=1.5)
arrows(1:4,mSig-seSig,1:4,mSig+seSig,length=0)
legend('topright', bty='n', legend='D', cex=2)
dev.off()

cuecor_lm = lm(cuecor_z ~ efflev_c, data=dat)
cuecor_m0 = lmer(cuecor_z ~ 1 + (1|id), data=dat)
cuecor_m1 = lmer(cuecor_z ~ efflev_c + (1|id), data=dat)
cuecor_m2 = lmer(cuecor_z ~ efflev_c + (efflev_c|id), data=dat)

cuecoraov = anova(cuecor_m0, cuecor_m1, cuecor_m2)
cuecorsum = summary(cuecor_m1)
sjPlot::tab_model(cuecor_m2, file = 'out/cuecorMLM.html')


# Feedback
dat$fbcor_z = c(sapply(unique(dat$id), function(x) scale(dat$fbcor[dat$id==x]) ))

png('plots/fbcor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mSig  = tapply(dat$fbcor_z, list(dat$acc, dat$cued), mean, na.rm=T)
seSig = tapply(dat$fbcor_z, list(dat$acc, dat$cued), se)

colnames(mSig) = c('Uncued', 'Cued')
rownames(mSig) = c('Incorrect', 'Correct')

yrange = range(pretty(c(mSig-seSig, mSig+seSig)))
tb = barplot(mSig, beside=T,
             ylab='Corrugator (Z)',
             ylim=yrange,
             legend.text = T, args.legend = list(x='bottomright',bty='n', cex=1.5, title='Feedback'), 
             main='During Feedback', 
            cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.names = 1.5)
arrows(tb,mSig-seSig, tb,mSig+seSig, length = 0)
dev.off()

dat$acc_c = ifelse(dat$acc==1, 1, -1)
fbcor_lm = lm(fbcor_z ~ acc_c * cued_c, data=dat)
fbcor_m0 = lmer(fbcor_z ~ 1 + (1|id), data=dat)
fbcor_m1 = lmer(fbcor_z ~ acc_c + (1|id), data=dat)
fbcor_m2 = lmer(fbcor_z ~ acc_c+cued_c + (1|id), data=dat)
fbcor_m3 = lmer(fbcor_z ~ acc_c*cued_c + (1|id), data=dat)

fbcoraov = anova(fbcor_m0,fbcor_m1, fbcor_m2, fbcor_m3)
fbcorsum = summary(fbcor_m2)
summary(fbcor_lm)
sjPlot::tab_model(fbcor_m3, file = 'out/fbcorMLM.html')

# Save
capture.output(list(
  'Mod comparison (cue)'  = cuecoraov, 
  'Winning model (cue)'   = cuecorsum,
  'One-level model (cue)' = summary(cuecor_lm),
  'Mod comparison (prob)' = probcoraov,
  'Winning model (prob)'  = probcorsum, 
  'One-level model (prob)'= summary(probcor_lm), 
  'Mod comparison (fb)'   = fbcoraov,
  'Winning model (fb)'    = fbcorsum, 
  'One-level model (fb)'  = summary(fbcor_lm)
), 
file='out/COR_MLM.txt'
)


# Zyg ---------------------------------------------------------------------

# Problem
png('plots/probzyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probzyg_z = c(sapply(unique(dat$id), function(x) scale(dat$probzyg[dat$id==x]) ))

mSig  = tapply(dat$probzyg_z, list(dat$cued,dat$efflev), mean, na.rm=T)
seSig = tapply(dat$probzyg_z,  list(dat$cued,dat$efflev), se)
yrange = range(pretty(c(mSig+seSig+.1, mSig-seSig)))

plot(mSig[1,], type = 'b', ylim=yrange,xaxt='n',xlab='Effort Level', ylab='Zygomaticus (Z-Scored)', main='During Problem', 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
lines(mSig[2,],type='b',lty=2)
axis(1,at=1:4,labels=1:4, cex.axis=1.5)
arrows(1:4,mSig[1,]-seSig[1,],1:4,mSig[1,]+seSig[1,],length=0)
arrows(1:4,mSig[2,]-seSig[2,],1:4,mSig[2,]+seSig[2,],length=0)
#legend('bottomright',bty='n', lty=1:2, legend=c('Uncued', 'Cued'))
legend('topright', bty='n', legend='E', cex=2)
dev.off()

probzyg_lm = lm(probzyg_z ~ efflev_c * cued_c, data=dat)
probzyg_m0 = lmer(probzyg_z ~ 1 + (1|id),data=dat)
probzyg_m1 = lmer(probzyg_z ~ efflev_c + (1|id),data=dat)
probzyg_m2 = lmer(probzyg_z ~ efflev_c + cued_c + (1|id),data=dat)
probzyg_m3 = lmer(probzyg_z ~ efflev_c * cued_c + (1|id),data=dat)

probzygaov = anova(probzyg_m0, probzyg_m1, probzyg_m2, probzyg_m3)
# anova(probzyg_m0, probzyg_m2)
probzygsum = summary(probzyg_m1)
sjPlot::tab_model(probzyg_m3, file = 'out/probzygMLM.html')

# Cue
png('plots/cuezyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuezyg_z = c(sapply(unique(dat$id), function(x) scale(dat$cuezyg[dat$id==x]) ))

mSig  = tapply(dat$cuezyg_z, dat$efflev, mean, na.rm=T)
seSig = tapply(dat$cuezyg_z, dat$efflev, se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

plot(mSig, type = 'b', ylim=yrange,xaxt='n',xlab='Effort Level', ylab='Zygomaticus (Z-Scored)', main='During Cue', 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
axis(1,at=1:4,labels=1:4, cex.axis=1.5)
arrows(1:4,mSig-seSig,1:4,mSig+seSig,length=0)
legend('topright', bty='n', legend='F', cex=2)
dev.off()

cuezyg_lm = lm(cuezyg_z ~ efflev_c, data=dat)
cuezyg_m0 = lmer(cuezyg_z ~ 1 + (1|id), data=dat)
cuezyg_m1 = lmer(cuezyg_z ~ efflev_c + (1|id), data=dat)
cuezyg_m2 = lmer(cuezyg_z ~ efflev_c + (efflev|id), data=dat)

cuezygaov = anova(cuezyg_m0, cuezyg_m1, cuezyg_m2)
cuezygsum = summary(cuezyg_m2)
sjPlot::tab_model(cuezyg_m2, file = 'out/cuezygMLM.html')

# Feedback
dat$fbzyg_z = c(sapply(unique(dat$id), function(x) scale(dat$fbzyg[dat$id==x]) ))

png('plots/fbzyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mSig  = tapply(dat$fbzyg_z, list(dat$acc, dat$cued), mean, na.rm=T)
seSig = tapply(dat$fbzyg_z, list(dat$acc, dat$cued), se)

colnames(mSig) = c('Uncued', 'Cued')
rownames(mSig) = c('Incorrect', 'Correct')

yrange = range(pretty(c(mSig-seSig, mSig+seSig)))
tb = barplot(mSig, beside=T,
             ylab='Zygomaticus (Z)',
             ylim=yrange,
             #legend.text = T, args.legend = list(x='bottomright',bty='n', cex=1.5, title='Feedback'), 
             main='During Feedback', 
             cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.names = 1.5)
arrows(tb,mSig-seSig, tb,mSig+seSig, length = 0)
dev.off()

dat$acc_c = ifelse(dat$acc==1, 1, -1)
fbzyg_lm = lm(fbzyg_z ~ acc_c * cued_c, data=dat)
fbzyg_m0 = lmer(fbzyg_z ~ 1 + (1|id), data=dat)
fbzyg_m1 = lmer(fbzyg_z ~ acc_c + (1|id), data=dat)
fbzyg_m2 = lmer(fbzyg_z ~ acc_c+cued_c + (1|id), data=dat)
fbzyg_m3 = lmer(fbzyg_z ~ acc_c*cued_c + (1|id), data=dat)

fbzygaov = anova(fbzyg_m0,fbzyg_m1, fbzyg_m2, fbzyg_m3)
fbzygsum = summary(fbzyg_m2)
summary(fbzyg_lm)
sjPlot::tab_model(fbzyg_m3, file = 'out/fbzygMLM.html')

# Save
capture.output(list(
  'Mod comparison (cue)'  = cuecoraov, 
  'Winning model (cue)'   = cuecorsum,
  'One-level model (cue)' = summary(cuecor_lm),
  'Mod comparison (prob)' = probcoraov,
  'Winning model (prob)'  = probcorsum, 
  'One-level model (prob)'= summary(probcor_lm), 
  'Mod comparison (fb)'   = fbcoraov,
  'Winning model (fb)'    = fbcorsum, 
  'One-level model (fb)'  = summary(fbcor_lm)
), 
file='out/ZYG_MLM.txt'
)


# SCR ---------------------------------------------------------------------

# Problem
png('plots/probscr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probscr_z = c(sapply(unique(dat$id), function(x) scale(dat$probscr[dat$id==x]) ))

mSig  = tapply(dat$probscr_z, list(dat$cued,dat$efflev), mean, na.rm=T)
seSig = tapply(dat$probscr_z,  list(dat$cued,dat$efflev), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

plot(mSig[1,], type = 'b', ylim=yrange,xaxt='n',xlab='Effort Level', ylab='SCR (Z-Scored)', main='During Problem', 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
lines(mSig[2,],type='b',lty=2)
axis(1,at=1:4,labels=1:4,  cex.axis=1.5)
arrows(1:4,mSig[1,]-seSig[1,],1:4,mSig[1,]+seSig[1,],length=0)
arrows(1:4,mSig[2,]-seSig[2,],1:4,mSig[2,]+seSig[2,],length=0)
legend('topright', bty='n', legend='G', cex=2)
dev.off()

probscr_lm = lm(probscr_z ~ efflev_c * cued_c, data=dat)
probscr_m0 = lmer(probscr_z ~ 1 + (1|id),data=dat)
probscr_m1 = lmer(probscr_z ~ efflev_c + (1|id),data=dat)
probscr_m2 = lmer(probscr_z ~ efflev_c + cued_c + (1|id),data=dat)
probscr_m3 = lmer(probscr_z ~ efflev_c * cued_c + (1|id),data=dat)

probscraov = anova(probscr_m0, probscr_m1, probscr_m2, probscr_m3)
anova(probscr_m0, probscr_m2, probscr_m3)
probscrsum = summary(probscr_m3)
sjPlot::tab_model(probscr_m3, file = 'out/probscrMLM.html')

# Cue
png('plots/cuecr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuescr_z = c(sapply(unique(dat$id), function(x) scale(dat$cuescr[dat$id==x]) ))

mSig  = tapply(dat$cuescr_z, dat$efflev, mean, na.rm=T)
seSig = tapply(dat$cuescr_z, dat$efflev, se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

plot(mSig, type = 'b', ylim=yrange,xaxt='n',xlab='Effort Level', ylab='SCR (Z-Scored)', main='During Cue', 
     cex.axis=1.5, cex.lab=1.5, cex.main=2)
axis(1,at=1:4,labels=1:4,  cex.axis=1.5)
arrows(1:4,mSig-seSig,1:4,mSig+seSig,length=0)
legend('topright', bty='n', legend='H', cex=2)
dev.off()

cuescr_lm = lm(cuescr_z ~ efflev_c, data=dat)
cuescr_m0 = lmer(cuescr_z ~ 1 + (1|id), data=dat)
cuescr_m1 = lmer(cuescr_z ~ efflev_c + (1|id), data=dat)
cuescr_m2 = lmer(cuescr_z ~ efflev_c + (efflev|id), data=dat)

cuescraov = anova(cuescr_m0, cuescr_m1, cuescr_m2)
cuescrsum = summary(cuescr_m1)
sjPlot::tab_model(cuescr_m2, file = 'out/cuescrMLM.html')

# Feedback 
dat$fbscr_z = c(sapply(unique(dat$id), function(x) scale(dat$fbscr[dat$id==x]) ))

png('plots/fbscr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mSig  = tapply(dat$fbscr_z, list(dat$acc, dat$cued), mean, na.rm=T)
seSig = tapply(dat$fbscr_z, list(dat$acc, dat$cued), se)

colnames(mSig) = c('Uncued', 'Cued')
rownames(mSig) = c('Incorrect', 'Correct')

yrange = range(pretty(c(mSig-seSig, mSig+seSig)))
tb = barplot(mSig, beside=T,
             ylab='SCR (Z)',
             ylim=yrange,
             #legend.text = T, args.legend = list(x='bottomright',bty='n', cex=1.5, title='Feedback'), 
             main='During Feedback', 
             cex.axis=1.5, cex.lab=1.5, cex.main=2, cex.names = 1.5)
arrows(tb,mSig-seSig, tb,mSig+seSig, length = 0)
dev.off()

dat$acc_c = ifelse(dat$acc==1, 1, -1)
fbscr_lm = lm(fbscr_z ~ acc_c * cued_c, data=dat)
fbscr_m0 = lmer(fbscr_z ~ 1 + (1|id), data=dat)
fbscr_m1 = lmer(fbscr_z ~ acc_c + (1|id), data=dat)
fbscr_m2 = lmer(fbscr_z ~ acc_c+cued_c + (1|id), data=dat)
fbscr_m3 = lmer(fbscr_z ~ acc_c*cued_c + (1|id), data=dat)

fbscraov = anova(fbscr_m0,fbscr_m1, fbscr_m2, fbscr_m3)
fbscrsum = summary(fbscr_m2)
summary(fbscr_lm)
sjPlot::tab_model(fbscr_m3, file = 'out/fbscrMLM.html')


# Save
capture.output(list(
  'Mod comparison (cue)'  = cuecoraov, 
  'Winning model (cue)'   = cuecorsum,
  'One-level model (cue)' = summary(cuecor_lm),
  'Mod comparison (prob)' = probcoraov,
  'Winning model (prob)'  = probcorsum, 
  'One-level model (prob)'= summary(probcor_lm), 
  'Mod comparison (fb)'   = fbscraov,
  'Winning model (fb)'    = fbscrsum, 
  'One-level model (fb)'  = summary(fbscr_lm)
), 
file='out/SCR_MLM.txt'
)


# Individual differences -- RT vs acc -----------------------------
png('plots/rtXacc.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mrt  = tapply(dat$RT[dat$acc==1],  dat$id[dat$acc==1], mean, na.rm=T)
macc = tapply(dat$acc, dat$id, mean, na.rm=T)

plot(mrt, macc,  xlab = 'Mean Correct RT (in sec.)', ylab='p(Correct)',
     ylim=c(0,1), cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(macc~mrt), col='red', lwd=2)
r = cor.test(mrt,macc)
p = round(r$p.value, 2)
legend('bottomright', bty='n', legend=paste0('r = ', round(r$estimate, 2), '\n',
                                             'p = ', p))

dev.off()

# ... x eff
png('plots/rtXaccxeff.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mrt  = tapply(dat$RT[dat$acc==1],  list(dat$id[dat$acc==1], dat$efflev[dat$acc==1]), mean, na.rm=T)
macc = tapply(dat$acc, list(dat$id, dat$efflev), mean, na.rm=T)

plot(NULL, xlim=c(0,3), ylim=c(0,1), xlab = 'Mean Correct RT (in s.)', ylab='p(Correct)', cex.axis=1.5, cex.lab=1.5, cex = 1.5)
cols = rainbow(4)

for(i in 1:4) {
  points(mrt[,i], macc[,i], col=scales::alpha(cols[i], 0.5), pch=16)
  abline(lm(macc[,i]~mrt[,i]), col=cols[i], lwd=2)
  
}

legend('bottomleft', bty='n', lty=1, pch=16, col=cols, legend=1:4, title='Effort Level')

dev.off()


# Individual differences -- random slopes -----------------------------
png('plots/corXrt_slopes.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

rert  = coef(RTMLM3)$id[,'efflev']
rert  = rert[c(1:27, 29:length(rert))]
recue = coef(cuecor_m2)$id[,'efflev_c']

plot(rert, recue,  xlab = 'RT Effort Slope', ylab = 'Corrugator Effort Slope', 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(recue~rert), col='red', lwd=2)
r = cor.test(recue,rert)
p = round(r$p.value, 2)
legend('topleft', bty='n', legend=paste0('r = ', round(r$estimate, 2), '\n',
                                         'p = ', p))
dev.off()

png('plots/corXacc_slopes.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

reacc  = coef(accMLM4)$id[,'efflev_c']
reacc  = reacc[c(1:27, 29:length(reacc))]
recue = coef(cuecor_m2)$id[,'efflev_c']

plot(reacc, recue,  xlab = 'ACC Effort Slope', ylab = 'Corrugator Effort Slope', 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(recue~reacc), col='red', lwd=2)
r = cor.test(recue,reacc)
p = round(r$p.value, 2)
legend('topleft', bty='n', legend=paste0('r = ', round(r$estimate, 2), '\n',
                                         'p = ', p))
dev.off()

# Individual differences -- difference scores -----------------------------

png('plots/corXrt_diff.png', width=4, height=4, units = 'in', res=300)

mrt     = tapply(dat$RT, list(dat$id, dat$efflev), mean,na.rm=T)
rtdiff  = mrt[,4] - mrt[,1]
mcor    = tapply(dat$cuecor, list(dat$id, dat$efflev), mean,na.rm=T)
cordiff = mcor[,4] - mcor[,1]

par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)
plot(rtdiff, cordiff,  xlab = expression(Delta~'RT High/Low Effort'), ylab = expression(Delta~'COR on Cue High/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~rtdiff), col='red', lwd=2)
r = cor.test(cordiff,rtdiff)
p = round(r$p.value, 2)
legend('topleft', bty='n', legend=paste0('r = ', round(r$estimate, 2), '\n',
                                         'p = ', p))
dev.off()

png('plots/corXacc_diff.png', width=4, height=4, units = 'in', res=300)

macc     = tapply(dat$acc, list(dat$id, dat$efflev), mean,na.rm=T)
accdiff  = macc[,4] - macc[,1]
mcor    = tapply(dat$probcor, list(dat$id, dat$efflev), mean,na.rm=T)
cordiff = mcor[,4] - mcor[,1]

par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)
plot(accdiff, cordiff,  xlab = expression(Delta~'Accuracy High/Low Effort'), ylab = expression(Delta~'COR on Cue High/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~accdiff), col='red', lwd=2)
r = cor.test(cordiff,accdiff)
p = round(r$p.value, 2)
legend('topleft', bty='n', legend=paste0('r = ', round(r$estimate, 2), '\n',
                                         'p = ', p))

dev.off()
