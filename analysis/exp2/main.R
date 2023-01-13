
library(lme4); library(lmerTest)
library(brms); library(bayestestR)
se           = function(x) sd(x,na.rm=T)/sqrt(length(x))

# Load data ---------------------------------------------------------------

files        = list.files('data/processed/', pattern = ".csv")
dat          = do.call(rbind, lapply(paste0('data/processed/', files), read.csv, header = T, stringsAsFactors = F))
dat          = dat[dat$block!='Practice', ]
dat          = dat[dat$id!=62,] # remove glitch subject
dat$trial    = c(sapply(unique(dat$id), function(x) 1:nrow(dat[dat$id==x,])))
dat$efflev_c = dat$efflev - mean(unique(dat$efflev))
dat$rewlev_c = ifelse(dat$rewlev=='low', -0.5, 0.5)

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

## RT ----------------------------------------------------------------------
png('plots/RT.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

bcor = dat[dat$acc==1, ]

mRT    = tapply(bcor$RT, list(bcor$efflev, bcor$rewlev), mean)
seRT   = tapply(bcor$RT, list(bcor$efflev, bcor$rewlev), se)
yrange = range(pretty(c(mRT-seRT, mRT+seRT+.4)))
  
matplot(rownames(mRT), mRT, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'Avg. Correct RT (s.)', xaxt='n',
        pch=16, cex.axis=1.5, cex.lab=1.5, ylim=yrange)
axis(1, at=rownames(mRT), labels=rownames(mRT), cex.axis=1.5)
arrows(1:4, mRT-seRT, 1:4, mRT+seRT, length=0)
legend('topleft', bty='n', pch=c(16,1), lty=1:2, legend=c('High', 'Low'), title='Reward')
# # legend('topright', bty='n', legend='A', cex=2)

dev.off()

RTMLM_b = brm(RT ~ efflev_c * rewlev_c + (1|id), 
              data = bcor, 
              seed = 2022, 
              prior = set_prior("normal(0,10)", class = "b"),
              chains = 3, iter = 5000,
              sample_prior = T, file='tmp')

sum       = describe_posterior(RTMLM_b)
h         = hypothesis(RTMLM_b, c('Intercept=0','efflev_c=0', 'rewlev_c=0','efflev_c:rewlev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/RTMLM_sum.csv')
saveRDS(RTMLM_b, file='out/bayes/RTMLM.rds')
file.remove('tmp.rds')

## Accuracy --------
png('plots/acc.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

macc  = tapply(dat$acc, list(dat$efflev, dat$rewlev), mean)
seacc = tapply(dat$acc, list(dat$efflev, dat$rewlev), se)

matplot(rownames(macc), macc, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'p(Correct)', xaxt='n', 
        ylim=c(0,1), pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(macc), labels=rownames(macc), cex.axis=1.5)
arrows(1:4, macc-seacc, 1:4, macc+seacc, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

accMLM_b = brm(acc ~ efflev_c * rewlev_c + (1|id), 
               data = dat, 
               family=bernoulli(link = "logit"),
               seed = 2022, 
               prior = set_prior("normal(0,10)", class = "b"),
               chains = 3, iter = 5000,
               sample_prior = T, file='tmp')

sum       = describe_posterior(accMLM_b)
h         = hypothesis(accMLM_b, c('Intercept=0','efflev_c=0', 'rewlev_c=0','efflev_c:rewlev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/accMLM_sum.csv')
saveRDS(accMLM_b, file='out/bayes/accMLM.rds')
file.remove('tmp.rds')


# Cor -----------------------------------------------------------------

## Problem -----------------------------------------------------------------
png('plots/probcor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probcor_z = c(sapply(unique(dat$id), function(x) scale(dat$probcor[dat$id==x]) ))

mSig   = tapply(dat$probcor_z, list(dat$efflev, dat$rewlev), mean, na.rm=T)
seSig  = tapply(dat$probcor_z,  list(dat$efflev, dat$rewlev), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'COR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

probcorMLM_b = brm(probcor_z ~ efflev_c * rewlev_c + (1|id),
                   data = dat, 
                   seed = 2022, 
                   prior = set_prior("normal(0,10)", class = "b"),
                   chains = 3, iter = 5000,
                   sample_prior = T, file='tmp')

sum       = describe_posterior(probcorMLM_b)
h         = hypothesis(probcorMLM_b, c('Intercept=0','efflev_c=0', 'rewlev_c=0','efflev_c:rewlev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/probcorMLM_sum.csv')
saveRDS(probcorMLM_b, file='out/bayes/probcorMLM.rds')
file.remove('tmp.rds')

## Cue -----------------------------------------------------------------
png('plots/cuecor.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuecor_z = c(sapply(unique(dat$id), function(x) scale(dat$cuecor[dat$id==x]) ))

mSig   = tapply(dat$cuecor_z, list(dat$efflev, dat$rewlev), mean, na.rm=T)
seSig  = tapply(dat$cuecor_z,  list(dat$efflev, dat$rewlev), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'COR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

cuecorMLM_b = brm(cuecor_z ~ efflev_c * rewlev_c + (1|id),
                  data = dat, 
                  seed = 2022, 
                  prior = set_prior("normal(0,10)", class = "b"),
                  chains = 3, iter = 5000,
                  sample_prior = T, file='tmp')

sum       = describe_posterior(cuecorMLM_b)
h         = hypothesis(cuecorMLM_b, c('efflev_c=0', 'rewlev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/cuecorMLM_sum.csv')
saveRDS(cuecorMLM_b, file='out/bayes/cuecorMLM.rds')
file.remove('tmp.rds')

# Zyg ---------------------------------------------------------------------

## Problem -----------------------------------------------------------------
png('plots/probzyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probzyg_z = c(sapply(unique(dat$id), function(x) scale(dat$probzyg[dat$id==x]) ))

mSig  = tapply(dat$probzyg_z, list(dat$efflev, dat$rewlev), mean, na.rm=T)
seSig = tapply(dat$probzyg_z,  list(dat$efflev, dat$rewlev), se)
yrange = range(pretty(c(mSig+seSig+.1, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'ZYG (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()


probzygMLM_b = brm(probzyg_z ~ efflev_c * rewlev_c + (1|id),
                   data = dat, 
                   seed = 2022, 
                   prior = set_prior("normal(0,10)", class = "b"),
                   chains = 3, iter = 5000,
                   sample_prior = T, file='tmp')

sum       = describe_posterior(probzygMLM_b)
h         = hypothesis(probzygMLM_b, c('Intercept=0','efflev_c=0', 'rewlev_c=0','efflev_c:rewlev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/probzygMLM_sum.csv')
saveRDS(probzygMLM_b, file='out/bayes/probzygMLM.rds')
file.remove('tmp.rds')


## Cue -----------------------------------------------------------------
png('plots/cuezyg.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuezyg_z = c(sapply(unique(dat$id), function(x) scale(dat$cuezyg[dat$id==x]) ))

mSig   = tapply(dat$cuezyg_z, list(dat$efflev, dat$rewlev), mean, na.rm=T)
seSig  = tapply(dat$cuezyg_z,  list(dat$efflev, dat$rewlev), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'ZYG (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
dev.off()

cuezygMLM_b = brm(cuezyg_z ~ efflev_c * rewlev_c + (1|id),
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



# SCR ---------------------------------------------------------------------

## Problem -----------------------------------------------------------------
png('plots/probscr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$probscr_z = c(sapply(unique(dat$id), function(x) scale(dat$probscr[dat$id==x]) ))
dat$probscr_z[abs(dat$probscr_z) > 3] = NA

mSig  = tapply(dat$probscr_z, list(dat$efflev, dat$rewlev), mean, na.rm=T)
seSig = tapply(dat$probscr_z, list(dat$efflev, dat$rewlev), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'SCR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
# legend('topright', bty='n', legend='B', cex=2)
dev.off()

probscrMLM_b = brm(probscr_z ~ efflev_c * rewlev_c + (1|id),
                   data = dat, 
                   seed = 2022, 
                   prior = set_prior("normal(0,10)", class = "b"),
                   chains = 3, iter = 5000,
                   sample_prior = T, file='tmp')

sum       = describe_posterior(probscrMLM_b)
h         = hypothesis(probscrMLM_b, c('Intercept=0','efflev_c=0', 'rewlev_c=0','efflev_c:rewlev_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/probscrMLM_sum.csv')
saveRDS(probscrMLM_b, file='out/bayes/probscrMLM.rds')
file.remove('tmp.rds')


## Cue -----------------------------------------------------------------
png('plots/cuecr.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

dat$cuescr_z = c(sapply(unique(dat$id), function(x) scale(dat$cuescr[dat$id==x]) ))
dat$cuescr_z[abs(dat$cuescr_z) > 3] = NA

mSig   = tapply(dat$cuescr_z, list(dat$efflev, dat$rewlev), mean, na.rm=T)
seSig  = tapply(dat$cuescr_z,  list(dat$efflev, dat$rewlev), se)
yrange = range(pretty(c(mSig+seSig, mSig-seSig)))

matplot(rownames(mSig), mSig, type='b', lty=1:2, col='black', xlab = 'Effort Level', ylab = 'SCR (Z-Score)', xaxt='n', 
        ylim=yrange, pch=16, cex.axis=1.5, cex.lab=1.5)
axis(1, at=rownames(mSig), labels=rownames(mSig), cex.axis=1.5)
arrows(1:4, mSig-seSig, 1:4, mSig+seSig, length=0)
dev.off()

cuescrMLM_b = brm(cuescr_z ~ efflev_c * rewlev_c + (1|id),
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


# Individual differences  -----------------------------

## rt x acc ----------------------------------------------------------------

### RT vs acc -----------------------------
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


png('plots/rtXaccXeff.png', width=4, height=4, units = 'in', res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)

mRT  = tapply(dat$RT[dat$acc==1],  list(dat$id[dat$acc==1], dat$efflev[dat$acc==1]), mean, na.rm=T)
macc = tapply(dat$acc, list(dat$id, dat$efflev), mean, na.rm=T)

plot(NULL, xlim=range(mRT), ylim=c(0,1),
     xlab = 'Mean Correct RT (in sec.)', ylab='p(Correct)', 
     cex.axis=1.5, cex.lab=1.5)
cols = rainbow(4)
for(i in 1:4 ) {
  points(mRT[,i], macc[,i], pch=21,cex=1.5, bg=scales::alpha(cols[i], 0.5) )
  abline(lm(macc[,i]~mRT[,i]), col=cols[i], lwd=3)
}

legend('bottomleft', bty='n', fill=cols, legend=1:4, title='Effort Level', ncol=2)

dev.off()

## difference scores - cor_eff -----------------------------

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

## difference scores - cor_rew -----------------------------

### RT -----------------------------
png('plots/cor_rewXrt_diff.png', width=4, height=4, units = 'in', res=300)

mrt     = tapply(dat$RT, list(dat$id, dat$efflev), mean,na.rm=T)
rtdiff  = mrt[,4] - mrt[,1]
mcor    = tapply(dat$cuecor, list(dat$id, dat$rewlev), mean,na.rm=T)
cordiff = mcor[,'high'] - mcor[,'low']

par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)
plot(rtdiff, cordiff,  xlab = expression(Delta~'RT High/Low Effort'), ylab = expression(Delta~'COR on Cue High/Low Reward'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~rtdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(cordiff, rtdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('topright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                          'HDI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))
dev.off()

### acc -----------------------------

png('plots/cor_rewXacc_diff.png', width=4, height=4, units = 'in', res=300)

macc     = tapply(dat$acc, list(dat$id, dat$efflev), mean,na.rm=T)
accdiff  = macc[,4] - macc[,1]
mcor    = tapply(dat$probcor, list(dat$id, dat$rewlev), mean,na.rm=T)
cordiff = mcor[,'high'] - mcor[,'low']

par(mar=c(5.1, 5.1, 4.1, 2.1), bg=NA)
plot(accdiff, cordiff,  xlab = expression(Delta~'Accuracy High/Low Effort'), ylab = expression(Delta~'COR on Cue High/Low Reward'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~accdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(cordiff, accdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('topleft', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                          'HDI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

# TLX ---------------------------------------------------------------------

tlx_files    = list.files('data/behav/tlx/', pattern = '.csv')
TLX          = do.call(rbind, lapply(paste0('data/behav/tlx/', tlx_files), read.csv, header = T, stringsAsFactors = F))
TLX          = TLX[TLX$id %in% unique(dat$id), ]
TLX$effort_c = TLX$EffortLevel - mean(unique(TLX$EffortLevel))
TLX$reward_c = ifelse(TLX$RewardLevel=='low', -0.5, 0.5)

## basic TLX effects -------------------------------------------------------

### demand ------------------------------------------------------------------

png('plots/tlx_demand.png', width=6, height=4, units = 'in', res=300)

m_demand  = tapply(TLX$mental_demand, list(TLX$RewardLevel, TLX$EffortLevel), mean)
se_demand = tapply(TLX$mental_demand, list(TLX$RewardLevel, TLX$EffortLevel), se)

b = barplot(m_demand, beside=T, xlab='Effort Level', ylab='Rating', main='How mentally demanding was the task?',
            legend.text = T, args.legend = list(bty='n',x='topleft', title='Reward'),
            ylim=c(0, 7), cex.names =1.5, cex.lab=1.5, cex.axis=1.5)
arrows(b, m_demand-se_demand, b, m_demand+se_demand,length=0)

dev.off()


tlx_demand_mod = brm(mental_demand ~ effort_c * reward_c,
                     data = TLX, 
                     seed = 2022, 
                     prior = set_prior("normal(0,10)", class = "b"),
                     chains = 3, iter = 5000,
                     sample_prior = T, file='tmp')

sum       = describe_posterior(tlx_demand_mod)
h         = hypothesis(tlx_demand_mod, c('Intercept=0','effort_c=0', 'reward_c=0','effort_c:reward_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/tlx_demand_sum.csv')
saveRDS(tlx_demand_mod, file='out/bayes/tlx_demand_mod.rds')
file.remove('tmp.rds')


### effort ------------------------------------------------------------------

png('plots/tlx_effort.png', width=6, height=4, units = 'in', res=300)

m_effort  = tapply(TLX$effort, list(TLX$RewardLevel, TLX$EffortLevel), mean)
se_effort = tapply(TLX$effort, list(TLX$RewardLevel, TLX$EffortLevel), se)

b = barplot(m_effort, beside=T, xlab='Effort Level', ylab='Rating', main='How hard did you have to work to accomplish\nyour level of performance?',
            legend.text = T, args.legend = list(bty='n',x='topleft', title='Reward'),
            ylim=c(0, 7), cex.names =1.5, cex.lab=1.5, cex.axis=1.5)
arrows(b, m_effort-se_effort, b, m_effort+se_effort,length=0)

dev.off()


tlx_effort_mod = brm(effort ~ effort_c * reward_c,
                     data = TLX, 
                     seed = 2022, 
                     prior = set_prior("normal(0,10)", class = "b"),
                     chains = 3, iter = 5000,
                     sample_prior = T, file='tmp')

sum       = describe_posterior(tlx_effort_mod)
h         = hypothesis(tlx_effort_mod, c('Intercept=0','effort_c=0', 'reward_c=0','effort_c:reward_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/tlx_effort_sum.csv')
saveRDS(tlx_effort_mod, file='out/bayes/tlx_effort_mod.rds')
file.remove('tmp.rds')



## individual differences --------------------------------------------------

### rt X effort --------------------------------------------------

png('plots/rtXtlx_effort.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$effort, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mrt      = tapply(dat$RT, list(dat$id, dat$efflev), mean,na.rm=T)
rtdiff   = mrt[,4] - mrt[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, rtdiff,  xlab = expression(Delta~'TLX Effort\nHigh/Low Effort'), ylab = expression(Delta~'Mean RT\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(rtdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, rtdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('topleft', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()


### rt X demand --------------------------------------------------

png('plots/rtXtlx_demand.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$mental_demand, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mrt      = tapply(dat$RT, list(dat$id, dat$efflev), mean,na.rm=T)
rtdiff   = mrt[,4] - mrt[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, rtdiff,  xlab = expression(Delta~'TLX Mental Demand\nHigh/Low Effort'), ylab = expression(Delta~'Mean RT\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(rtdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, rtdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('topleft', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                         'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

### acc X effort --------------------------------------------------

png('plots/accXtlx_effort.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$effort, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
macc     = tapply(dat$acc, list(dat$id, dat$efflev), mean,na.rm=T)
accdiff  = macc[,4] - macc[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, accdiff,  xlab = expression(Delta~'TLX Effort\nHigh/Low Effort'), ylab = expression(Delta~'P(Correct)\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(accdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, accdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomleft', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                         'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()


### acc X demand --------------------------------------------------

png('plots/accXtlx_demand.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$mental_demand, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
macc     = tapply(dat$acc, list(dat$id, dat$efflev), mean,na.rm=T)
accdiff  = macc[,4] - macc[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, accdiff,  xlab = expression(Delta~'TLX Mental Demand\nHigh/Low Effort'), ylab = expression(Delta~'P(Correct)\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(accdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, accdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomleft', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                         'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()


### cor X effort --------------------------------------------------

png('plots/corXtlx_effort.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$effort, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mcor     = tapply(dat$cuecor, list(dat$id, dat$efflev), mean,na.rm=T)
cordiff  = mcor[,4] - mcor[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, cordiff,  xlab = expression(Delta~'TLX Effort\nHigh/Low Effort'), ylab = expression(Delta~'COR on Cue\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, cordiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

### cor X demand --------------------------------------------------

png('plots/corXtlx_demand.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$mental_demand, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mcor     = tapply(dat$cuecor, list(dat$id, dat$efflev), mean,na.rm=T)
cordiff  = mcor[,4] - mcor[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, cordiff,  xlab = expression(Delta~'TLX Mental Demand\nHigh/Low Effort'), ylab = expression(Delta~'COR on Cue\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(cordiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, cordiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()


### zyg X effort --------------------------------------------------

png('plots/zygXtlx_effort.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$effort, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mzyg     = tapply(dat$cuezyg, list(dat$id, dat$efflev), mean,na.rm=T)
zygdiff  = mzyg[,4] - mzyg[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, zygdiff,  xlab = expression(Delta~'TLX Effort\nHigh/Low Effort'), ylab = expression(Delta~'ZYG on Cue\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(zygdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, zygdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

### zyg X demand --------------------------------------------------

png('plots/zygXtlx_demand.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$mental_demand, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mzyg     = tapply(dat$cuezyg, list(dat$id, dat$efflev), mean,na.rm=T)
zygdiff  = mzyg[,4] - mzyg[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, zygdiff,  xlab = expression(Delta~'TLX Mental Demand\nHigh/Low Effort'), ylab = expression(Delta~'ZYG on Cue\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(zygdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, zygdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()



### scr X effort --------------------------------------------------

png('plots/scrXtlx_effort.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$effort, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mscr     = tapply(dat$cuescr, list(dat$id, dat$efflev), mean,na.rm=T)
scrdiff  = mscr[,4] - mscr[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, scrdiff,  xlab = expression(Delta~'TLX Effort\nHigh/Low Effort'), ylab = expression(Delta~'SCR on Cue\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(scrdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, scrdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

### scr X demand --------------------------------------------------

png('plots/scrXtlx_demand.png', width=6, height=5, units = 'in', res=300)

mtlx     = tapply(TLX$mental_demand, list(TLX$id, TLX$EffortLevel), mean,na.rm=T)
tlxdiff  = mtlx[,4] - mtlx[,1]
mscr     = tapply(dat$cuescr, list(dat$id, dat$efflev), mean,na.rm=T)
scrdiff  = mscr[,4] - mscr[,1]

par(mar=c(7.1, 7.1, 2.1, 3.1), mgp=c(4,1,0), bg=NA)
plot(tlxdiff, scrdiff,  xlab = expression(Delta~'TLX Mental Demand\nHigh/Low Effort'), ylab = expression(Delta~'SCR on Cue\nHigh/Low Effort'), 
     cex.axis=1.5, cex.lab=1.5, cex = 1.5, pch=16, col='grey')
abline(lm(scrdiff~tlxdiff), col='red', lwd=2)
rs  = BayesFactor::correlationBF(tlxdiff, scrdiff, posterior = T, iter=5000)[,'rho']
r   = median(rs)
hpd = hdi(rs)
legend('bottomright', bty='n', legend=paste0('r = ', round(r, 2), '\n',
                                             'CI = [', round(hpd$CI_low,2), ',', round(hpd$CI_high,2), ']' ))

dev.off()

# Corrugator/Performance (trial-by-trial) ------------------------------------------------

dat$logrt      = log(dat$RT)
dat$trial_c    = dat$trial - median(dat$trial)
dat$trial_c0   = dat$trial_c/max(dat$trial_c)
dat$last_err   = dplyr::lag(dat$acc)
dat$last_err[dat$trialinblock==1] = NA
dat$last_err_c = dat$last_err-.5
dat$cuecor_z_c = dat$cuecor_z - mean(dat$cuecor_z, na.rm=T)
dat$cuezyg_z_c = dat$cuezyg_z - mean(dat$cuezyg_z, na.rm=T)
dat$cuescr_z_c = dat$cuescr_z - mean(dat$cuescr_z, na.rm=T)
cor = dat[dat$acc==1, ]

rt_cor_MLM_b = brm(logrt ~ efflev_c * rewlev_c * cuecor_z_c + trial_c0 + last_err_c + (1|id),
                  data = cor, 
                  seed = 2022, 
                  prior = set_prior("normal(0,10)", class = "b"),
                  chains = 3, iter = 5000,
                  sample_prior = T, file='tmp')

sum       = describe_posterior(rt_cor_MLM_b)
h         = hypothesis(rt_cor_MLM_b, c('cuecor_z_c=0'))
sum$bf    = abs(h$hypothesis$Evid.Ratio)
sum$logbf = log(sum$bf)
write.csv(sum, file='out/bayes/rt_cor_MLM_sum.csv')
saveRDS(rt_cor_MLM_b, file='out/bayes/rt_cor_MLM.rds')
file.remove('tmp.rds')


## visualize ----
cor$cor_split = cut(cor$cuecor_z, 2, labels=F)
mrt  = tapply(cor$RT, list(cor$efflev, cor$cor_split), mean)
sert = tapply(cor$RT, list(cor$efflev, cor$cor_split), se)

png('plots/cuecor_rt_plot.png', width=6, height=5, units = 'in', res=300)

cols = c('grey80','black')
matplot(mrt, type='b', lty=1,pch=16,col=cols, 
        xaxt='n',xlab='Effort Level', ylab='Avg.Correct RT (s.)')
axis(1, at=1:4, labels = 1:4)
arrows(1:4, mrt[,1]-sert[,1], 1:4, mrt[,1]+sert[,1], length=0, col=cols[1])
arrows(1:4, mrt[,2]-sert[,2], 1:4, mrt[,2]+sert[,2], length=0, col=cols[2])
legend('topleft',bty='n', lty=1,pch=16,col=cols, title='COR on Cue', legend=c('Low','High'))

dev.off()



# Corrugator/Performance (random effects) ------------------------------------------------

m1 = lmer(cuecor_z ~ efflev_c * rewlev_c + (efflev_c|id), data=dat)
r  = ranef(m1)$id
r$id = rownames(r)
colnames(r) = c('intercept_ranef', 'efflev_c_ranef', 'id')

dat = merge(dat, r, by = 'id')
cor = dat[dat$acc==1, ]

rt_cor_MLM_r = lmer(logrt ~ efflev_c * rewlev_c * efflev_c_ranef + trial_c0 + last_err_c + (1|id),
                    data=cor)
summary(rt_cor_MLM_r)

# Save image --------------------------------------------------------------

save.image('exp2.RData')

