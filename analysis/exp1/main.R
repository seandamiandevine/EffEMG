
library(lme4)
library(brms); library(bayestestR)
se  = function(x) sd(x,na.rm=T)/sqrt(length(x))

# Load data ---------------------------------------------------------------

files        = list.files('data/processed/', pattern = ".csv")
dat          = do.call(rbind, lapply(paste0('data/processed/', files), read.csv, header = T, stringsAsFactors = F))
dat          = dat[dat$block!='Practice', ]
dat$trial    = c(sapply(unique(dat$id), function(x) 1:nrow(dat[dat$id==x,])))
dat$cued_c   = ifelse(dat$cued==1, 0.5, -0.5)
dat$efflev_c = dat$efflev - mean(unique(dat$efflev))

# Descriptives ------------------------------------------------------------

N     = length(unique(dat$id))
pFem  = mean(sapply(unique(dat$id), function(x) grepl('f', tolower(dat[dat$id==x, 'sex'][1]))))
mAge  = mean(unique(dat$age))
sdAge = sd(unique(dat$age))
mtime = mean(tapply(dat$ITI3Time, dat$id, max)/60)

capture.output(list(N=N, pFem=pFem, mAge=mAge, sdAge=sdAge), file='out/demo.txt')

# Sanity checks -----------------------------------------------------------

# Accuracy hist
png('plots/global_acc.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

macc = tapply(dat$acc, dat$id, mean, na.rm=T)
hist(macc, xlim=c(0,1), xlab='Mean Accuracy', main='', cex.axis=1.5, cex.lab=1.5)
abline(v=.33,lty=2)

dev.off()

# Performance and effort level --------------------------------------------

## RT --------------------------------------------------------------------

png('plots/RT.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

bcor = dat[dat$acc==1, ]

mRT    = tapply(bcor$RT, list(bcor$efflev, bcor$cued), mean)
seRT   = tapply(bcor$RT, list(bcor$efflev, bcor$cued), se)
yrange = range(pretty(c(mRT-seRT, mRT+seRT+.4)))
  
matplot(rownames(mRT), mRT, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'Avg. Correct RT (s.)', xaxt='n',
        pch=16, cex.axis=1.5, cex.lab=1.5, ylim=yrange)
axis(1, at=rownames(mRT), labels=rownames(mRT), cex.axis=1.5)
arrows(1:4, mRT-seRT, 1:4, mRT+seRT, length=0)
legend('topleft', bty='n', pch=c(16,1), lty=1:2, legend=c('Uncued', 'Cued'), title='Trial Type')
# legend('topright', bty='n', legend='A', cex=2)

dev.off()

RTMLM_b = brm(RT ~ efflev_c * cued_c + (1|id), 
              data = bcor, 
              seed = 2022, 
              prior = set_prior("normal(0,10)", class = "b"),
              chains = 3, iter = 5000,
              sample_prior = T, file='tmp')

sum       = describe_posterior(RTMLM_b)
h         = hypothesis(RTMLM_b, c('Intercept=0','efflev_c=0', 'cued_c=0','efflev_c:cued_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/RTMLM_sum.csv')
saveRDS(RTMLM_b, file='out/bayes/RTMLM.rds')
file.remove('tmp.rds')

# RTMLM0 = lmer(RT~1 + (1|id), data=bcor)
# RTMLM1 = lmer(RT~efflev_c + (1|id), data=bcor)
# RTMLM2 = lmer(RT~efflev_c + cued_c + (1|id), data=bcor)
# RTMLM3 = lmer(RT~efflev_c + cued_c + (efflev|id), data=bcor)
# RTMLM4 = lmer(RT~efflev_c * cued_c + (efflev|id), data=bcor)
# 
# comp = anova(RTMLM0, RTMLM1, RTMLM2, RTMLM3, RTMLM4)
# sum  = summary(RTMLM4)
# capture.output(list(aov=comp, summary=sum), file='out/RTMLM.txt')
# sjPlot::tab_model(RTMLM4, file = 'out/RTMLM.html')

## acc --------------------------------------------------------------------
png('plots/acc.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

macc  = tapply(dat$acc, list(dat$efflev, dat$cued), mean)
seacc = tapply(dat$acc, list(dat$efflev, dat$cued), se)
range(pretty(c(macc-seacc, macc+seacc)))

matplot(rownames(macc), macc, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'p(Correct)', xaxt='n', 
     ylim=c(0,1), pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(macc), labels=rownames(macc), cex.axis=1.5)
arrows(1:4, macc-seacc, 1:4, macc+seacc, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

accMLM_b = brm(acc ~ efflev_c * cued_c + (1|id), 
              data = dat, 
              family=bernoulli(link = "logit"),
              seed = 2022, 
              prior = set_prior("normal(0,10)", class = "b"),
              chains = 3, iter = 5000,
              sample_prior = T, file='tmp')

sum       = describe_posterior(accMLM_b)
h         = hypothesis(accMLM_b, c('Intercept=0','efflev_c=0', 'cued_c=0','efflev_c:cued_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/accMLM_sum.csv')
saveRDS(accMLM_b, file='out/bayes/accMLM.rds')
file.remove('tmp.rds')

# accMLM0 = glmer(acc~1 + (1|id), data=dat, family='binomial')
# accMLM1 = glmer(acc~efflev_c + (1|id), data=dat, family='binomial')
# accMLM2 = glmer(acc~efflev_c+cued_c + (1|id), data=dat, family='binomial')
# accMLM3 = glmer(acc~efflev_c*cued_c + (1|id), data=dat, family='binomial')
# accMLM4 = glmer(acc~efflev_c+cued_c + (efflev_c|id), data=dat, family='binomial') # singular
# 
# comp = anova(accMLM0, accMLM1, accMLM2, accMLM3, accMLM4)
# sum  = summary(accMLM3)
# capture.output(list(aov=comp, summary=sum), file='out/accMLM.txt')
# sjPlot::tab_model(accMLM3, transform = NULL,file = 'out/accMLM.html')


# Cor -----------------------------------------------------------------

## Problem -----------------------------------------------------------------
png('plots/probcor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probcor_z = c(sapply(unique(dat$id), function(x) scale(dat$probcor[dat$id==x]) ))

mSig   = tapply(dat$probcor_z, list(dat$efflev, dat$cued), mean, na.rm=T)
seSig  = tapply(dat$probcor_z,  list(dat$efflev, dat$cued), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'COR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

probcorMLM_b = brm(probcor_z ~ efflev_c * cued_c + (1|id),
                   data = dat, 
                   seed = 2022, 
                   prior = set_prior("normal(0,10)", class = "b"),
                   chains = 3, iter = 5000,
                   sample_prior = T, file='tmp')

sum       = describe_posterior(probcorMLM_b)
h         = hypothesis(probcorMLM_b, c('Intercept=0','efflev_c=0', 'cued_c=0','efflev_c:cued_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/probcorMLM_sum.csv')
saveRDS(probcorMLM_b, file='out/bayes/probcorMLM.rds')
file.remove('tmp.rds')

# dat$efflev_c_2 = dat$efflev_c^2
# probcor_lm = lm(probcor_z ~ efflev_c * cued_c, data=dat)
# probcor_m0 = lmer(probcor_z ~ 1 + (1|id),data=dat)
# probcor_m1 = lmer(probcor_z ~ efflev_c + (1|id),data=dat)
# probcor_m2 = lmer(probcor_z ~ efflev_c + cued_c + (1|id),data=dat)
# probcor_m3 = lmer(probcor_z ~ efflev_c * cued_c + (1|id),data=dat)
# 
# probcor_m1q = lmer(probcor_z ~ efflev_c + efflev_c_2 + (1|id),data=dat)
# 
# probcoraov = anova(probcor_m0, probcor_m1, probcor_m2, probcor_m3)
# probcorsum = summary(probcor_m2)
# sjPlot::tab_model(probcor_m2, file = 'out/probcorMLM.html')

## Cue -----------------------------------------------------------------
png('plots/cuecor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuecor_z = c(sapply(unique(dat$id), function(x) scale(dat$cuecor[dat$id==x]) ))

mSig  = tapply(dat$cuecor_z, dat$efflev, mean, na.rm=T)
seSig = tapply(dat$cuecor_z, dat$efflev, se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'COR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

cuecorMLM_b = brm(cuecor_z ~ efflev_c + (1|id),
                   data = dat, 
                   seed = 2022, 
                   prior = set_prior("normal(0,10)", class = "b"),
                   chains = 3, iter = 5000,
                   sample_prior = T, file='tmp')

sum       = describe_posterior(cuecorMLM_b)
h         = hypothesis(cuecorMLM_b, c('Intercept=0','efflev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/cuecorMLM_sum.csv')
saveRDS(cuecorMLM_b, file='out/bayes/cuecorMLM.rds')
file.remove('tmp.rds')

# cuecor_lm = lm(cuecor_z ~ efflev_c, data=dat)
# cuecor_m0 = lmer(cuecor_z ~ 1 + (1|id), data=dat)
# cuecor_m1 = lmer(cuecor_z ~ efflev_c + (1|id), data=dat)
# cuecor_m2 = lmer(cuecor_z ~ efflev_c + (efflev_c|id), data=dat)
# 
# cuecoraov = anova(cuecor_m0, cuecor_m1, cuecor_m2)
# cuecorsum = summary(cuecor_m1)
# sjPlot::tab_model(cuecor_m1, file = 'out/cuecorMLM.html')

# Zyg ---------------------------------------------------------------------

## Problem -----------------------------------------------------------------
png('plots/probzyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probzyg_z = c(sapply(unique(dat$id), function(x) scale(dat$probzyg[dat$id==x]) ))

mSig  = tapply(dat$probzyg_z, list(dat$efflev, dat$cued), mean, na.rm=T)
seSig = tapply(dat$probzyg_z,  list(dat$efflev, dat$cued), se)
yrange = range(pretty(c(mSig+seSig+.1, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'ZYG (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()


probzygMLM_b = brm(probzyg_z ~ efflev_c * cued_c + (1|id),
                  data = dat, 
                  seed = 2022, 
                  prior = set_prior("normal(0,10)", class = "b"),
                  chains = 3, iter = 5000,
                  sample_prior = T, file='tmp')

sum       = describe_posterior(probzygMLM_b)
h         = hypothesis(probzygMLM_b, c('Intercept=0','efflev_c=0', 'cued_c=0','efflev_c:cued_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/probzygMLM_sum.csv')
saveRDS(probzygMLM_b, file='out/bayes/probzygMLM.rds')
file.remove('tmp.rds')

# probzyg_lm = lm(probzyg_z ~ efflev_c * cued_c, data=dat)
# probzyg_m0 = lmer(probzyg_z ~ 1 + (1|id),data=dat)
# probzyg_m1 = lmer(probzyg_z ~ efflev_c + (1|id),data=dat)
# probzyg_m2 = lmer(probzyg_z ~ efflev_c + cued_c + (1|id),data=dat)
# probzyg_m3 = lmer(probzyg_z ~ efflev_c * cued_c + (1|id),data=dat)
# 
# probzygaov = anova(probzyg_m0, probzyg_m1, probzyg_m2, probzyg_m3)
# # anova(probzyg_m0, probzyg_m2)
# probzygsum = summary(probzyg_m2)
# sjPlot::tab_model(probzyg_m3, file = 'out/probzygMLM.html')

## Cue -----------------------------------------------------------------
png('plots/cuezyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuezyg_z = c(sapply(unique(dat$id), function(x) scale(dat$cuezyg[dat$id==x]) ))

mSig  = tapply(dat$cuezyg_z, dat$efflev, mean, na.rm=T)
seSig = tapply(dat$cuezyg_z, dat$efflev, se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'ZYG (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

cuezygMLM_b = brm(cuezyg_z ~ efflev_c + (1|id),
                  data = dat, 
                  seed = 2022, 
                  prior = set_prior("normal(0,10)", class = "b"),
                  chains = 3, iter = 5000,
                  sample_prior = T, file='tmp')

sum       = describe_posterior(cuezygMLM_b)
h         = hypothesis(cuezygMLM_b, c('Intercept=0','efflev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/cuezygMLM_sum.csv')
saveRDS(cuezygMLM_b, file='out/bayes/cuezygMLM.rds')
file.remove('tmp.rds')

# cuezyg_lm = lm(cuezyg_z ~ efflev_c, data=dat)
# cuezyg_m0 = lmer(cuezyg_z ~ 1 + (1|id), data=dat)
# cuezyg_m1 = lmer(cuezyg_z ~ efflev_c + (1|id), data=dat)
# cuezyg_m2 = lmer(cuezyg_z ~ efflev_c + (efflev|id), data=dat)
# 
# cuezygaov = anova(cuezyg_m0, cuezyg_m1, cuezyg_m2)
# cuezygsum = summary(cuezyg_m1)
# sjPlot::tab_model(cuezyg_m2, file = 'out/cuezygMLM.html')


# SCR ---------------------------------------------------------------------

## Problem -----------------------------------------------------------------
png('plots/probscr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probscr_z = c(sapply(unique(dat$id), function(x) scale(dat$probscr[dat$id==x]) ))
dat$probscr_z[abs(dat$probscr_z) > 3] = NA

mSig  = tapply(dat$probscr_z, list(dat$efflev, dat$cued), mean, na.rm=T)
seSig = tapply(dat$probscr_z, list(dat$efflev, dat$cued), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'SCR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

probscrMLM_b = brm(probscr_z ~ efflev_c * cued_c + (1|id),
                   data = dat, 
                   seed = 2022, 
                   prior = set_prior("normal(0,10)", class = "b"),
                   chains = 3, iter = 5000,
                   sample_prior = T, file='tmp')

sum       = describe_posterior(probscrMLM_b)
h         = hypothesis(probscrMLM_b, c('Intercept=0','efflev_c=0', 'cued_c=0','efflev_c:cued_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/probscrMLM_sum.csv')
saveRDS(probscrMLM_b, file='out/bayes/probscrMLM.rds')
file.remove('tmp.rds')

# probscr_lm = lm(probscr_z ~ efflev_c * cued_c, data=dat)
# probscr_m0 = lmer(probscr_z ~ 1 + (1|id),data=dat)
# probscr_m1 = lmer(probscr_z ~ efflev_c + (1|id),data=dat)
# probscr_m2 = lmer(probscr_z ~ efflev_c + cued_c + (1|id),data=dat)
# probscr_m3 = lmer(probscr_z ~ efflev_c * cued_c + (1|id),data=dat)
# 
# probscraov = anova(probscr_m0, probscr_m1, probscr_m2, probscr_m3)
# anova(probscr_m0, probscr_m2, probscr_m3)
# probscrsum = summary(probscr_m2)
# sjPlot::tab_model(probscr_m3, file = 'out/probscrMLM.html')

## Cue -----------------------------------------------------------------
png('plots/cuecr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuescr_z = c(sapply(unique(dat$id), function(x) scale(dat$cuescr[dat$id==x]) ))
dat$cuescr_z[abs(dat$cuescr_z) > 3] = NA

mSig  = tapply(dat$cuescr_z, dat$efflev, mean, na.rm=T)
seSig = tapply(dat$cuescr_z, dat$efflev, se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'SCR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

cuescrMLM_b = brm(cuescr_z ~ efflev_c + (1|id),
                  data = dat, 
                  seed = 2022, 
                  prior = set_prior("normal(0,10)", class = "b"),
                  chains = 3, iter = 5000,
                  sample_prior = T, file='tmp')

sum       = describe_posterior(cuescrMLM_b)
h         = hypothesis(cuescrMLM_b, c('Intercept=0','efflev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/cuescrMLM_sum.csv')
saveRDS(cuescrMLM_b, file='out/bayes/cuescrMLM.rds')
file.remove('tmp.rds')

# cuescr_lm = lm(cuescr_z ~ efflev_c, data=dat)
# cuescr_m0 = lmer(cuescr_z ~ 1 + (1|id), data=dat)
# cuescr_m1 = lmer(cuescr_z ~ efflev_c + (1|id), data=dat)
# cuescr_m2 = lmer(cuescr_z ~ efflev_c + (efflev|id), data=dat)
# 
# cuescraov = anova(cuescr_m0, cuescr_m1, cuescr_m2)
# cuescrsum = summary(cuescr_m1)
# sjPlot::tab_model(cuescr_m1, file = 'out/cuescrMLM.html')


# Individual differences -----------------------------

## RT vs acc -----------------------------
png('plots/rtXacc.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mRT  = tapply(dat$RT[dat$acc==1],  dat$id[dat$acc==1], mean, na.rm=T)
macc = tapply(dat$acc, dat$id, mean, na.rm=T)

plot(mRT, macc,  xlab = 'Mean Correct RT (in sec.)', ylab='p(Correct)',
     ylim=c(0,1), cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(macc~mRT), col='red', lwd=2)
rs  = BayesFactor::correlationBF(macc, mRT, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'HDI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

## difference scores -----------------------------

### RT -----------------------------
png('plots/corXrt_diff.png', width=4, height=4, units = 'in', res=300)

mrt     = tapply(dat$RT, list(dat$id, dat$efflev), mean,na.rm=T)
rtdiff  = mrt[,4] - mrt[,1]
mcor    = tapply(dat$cuecor, list(dat$id, dat$efflev), mean,na.rm=T)
cordiff = mcor[,4] - mcor[,1]

par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)
plot(rtdiff, cordiff,  xlab = expression(Delta~'RT High/Low Effort'), ylab = expression(Delta~'COR on Cue High/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~rtdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(cordiff, rtdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('topright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'HDI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))
dev.off()

### acc -----------------------------

png('plots/corXacc_diff.png', width=4, height=4, units = 'in', res=300)

macc     = tapply(dat$acc, list(dat$id, dat$efflev), mean,na.rm=T)
accdiff  = macc[,4] - macc[,1]
mcor    = tapply(dat$probcor, list(dat$id, dat$efflev), mean,na.rm=T)
cordiff = mcor[,4] - mcor[,1]

par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)
plot(accdiff, cordiff,  xlab = expression(Delta~'Accuracy High/Low Effort'), ylab = expression(Delta~'COR on Cue High/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~accdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(cordiff, accdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('topright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'HDI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()



# Misc --------------------------------------------------------------------

# #bonus performance model -------------------------------------------------


dat$logrt      = log(dat$RT)
dat$trial_c    = dat$trial - median(dat$trial)
dat$trial_c0   = dat$trial_c/max(dat$trial_c)
dat$last_err   = dplyr::lag(dat$acc)
dat$last_err[dat$trialinblock==1] = NA
dat$last_err_c = dat$last_err-.5
dat$cuecor_z_c = dat$cuecor_z - mean(dat$cuecor_z, na.rm=T)
dat$cuezyg_z_c = dat$cuezyg_z - mean(dat$cuezyg_z, na.rm=T)
cor            = dat[dat$acc==1,]


mod_rt  = lmer(logrt ~ efflev_c*cuecor_z_c + trial_c + last_err_c + (1|id),
               data=cor)

summary(mod_rt)


X = expand.grid(efflev_c=unique(mod_rt@frame$efflev_c),
                cuecor_z_c = quantile(mod_rt@frame$cuecor_z_c, probs = c(0, .5, 1)), 
                trial_c = 0, 
                last_err_c=0)

X$pred = predict(mod_rt, newdata=X, re.form=NA)

mpred = tapply(X$pred, list(X$efflev_c+2.5, X$cuecor_z_c), mean, na.rm=T)

cols = grey(seq(0, 1, length.out=nrow(mpred)))
matplot(mpred, type='l', pch=16, lty=1, lwd=4, col=cols,
        xaxt='n', xlab='Effort Level', ylab='', main='Predicted Ln Correct RT\nDuring Problem')
axis(1,at=1:4, labels=1:4)
legend('topleft', bty='n', lty=1, pch=16,col=cols, 
       legend=c('Low', 'Medium', 'High'), title='COR on Cue')



# Save image --------------------------------------------------------------

save.image('exp1.RData')

