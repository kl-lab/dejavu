tsi = 578
y = tsset[[tsi]]$x
yout = as.vector(tsset[[tsi]]$xx[1:horizon])

# fotios 
yresults = DoTheJob(y, fh=horizon, nts=k, Preprocessing=1, Dist_type=3, clustering=cluster, LoadData=FALSE, FullOutput=TRUE)

fcs = as.vector(yresults$fcs)
fotios_MASE1 = mean(abs(yout - fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))

PIs = apply(yresults$similarfcs, 2, quantile, probs=c(0.025, 0.975))
PIL = PIs[1,]
PIU = PIs[2,]
fotios_MSIS1 = mean((PIU - PIL +
               (2 / gamma) * (PIL - yout) * (PIL > yout) +
               (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))
print(paste0('fotios MSIS1:', fotios_MSIS1))
# dev.new(); par(mfrow = c(2,2))
plot(ts(yout), ylim = c(min(PIL, yout),max(PIU, yout)), main = paste0('fotios MSIS1:', round(fotios_MSIS1, 2)))
lines(ts(PIL), col =2)
lines(ts(PIU), col =3)
lines(fcs, col = 4)

corrs = cor(data.frame(t(yresults$similarfcs)))
diag(corrs) <- NA
rho = mean(corrs, na.rm = TRUE)
intervals = qt(1-gamma/2, k) * sqrt(((k-1)/k)*((1+rho)/(1-rho)+1/k)) * apply(yresults$similarfcs, 2, sd)
PIL = fcs - intervals
PIU = fcs + intervals
fotios_MSIS2 = mean((PIU - PIL +
               (2 / gamma) * (PIL - yout) * (PIL > yout) +
               (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))

plot(ts(yout), ylim = c(min(PIL, yout),max(PIU, yout)), main = paste0('fotios MSIS:', round(fotios_MSIS2, 2)))
lines(ts(PIL), col =2)
lines(ts(PIU), col =3)
lines(fcs, col = 4)
fit = ets(y)
fcs = as.vector(forecast(fit, h=horizon)$mean)
PIL = as.vector(forecast(fit, h=horizon, level=100*(1-gamma))$lower)
PIU = as.vector(forecast(fit, h=horizon, level=100*(1-gamma))$upper)
MASE_ets = mean(abs(yout - fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
MSIS_ets = mean((PIU - PIL +
                   (2 / gamma) * (PIL - yout) * (PIL > yout) +
                   (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))


plot(ts(yout), ylim = c(min(PIL, yout),max(PIU, yout)), main = paste0('ets MSIS:', round(MSIS_ets, 2)))
lines(ts(PIL), col =2)
lines(ts(PIU), col =3)
lines(fcs, col = 4)

# yanfei
k = 100
bs_fc_y = bs_fc(yresults$similarfcs, k)
bs_similarfcs = bs_fc_y$bs_similarfcs
bs_fc_sd = bs_fc_y$fcs_sd
yanfei_MASE = mean(abs(yout - bs_fc_y$fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
# corrs = cor(data.frame(t(bs_similarfcs)))
# diag(corrs) <- NA
# rho = mean(corrs, na.rm = TRUE)
# intervals = qt(1-gamma/2, k) * sqrt(((k-1)/k)*((1+rho)/(1-rho)+1/k)) * apply(bs_similarfcs, 2, sd)
# PIL = bs_fc_y$fcs - intervals
# PIU = bs_fc_y$fcs + intervals
PIs = apply(bs_similarfcs, 2, quantile, probs=c(0.025, 0.975))
PIL = PIs[1,] * (1 - 0.23)
PIU = PIs[2,] * (1 + 0.23)
yanfei_MSIS1 = mean((PIU - PIL +
               (2 / gamma) * (PIL - yout) * (PIL > yout) +
               (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))
plot(ts(yout), ylim = c(min(PIL, yout),max(PIU, yout)), main = paste0('yanfei MSIS1:', round(yanfei_MSIS1, 2)))
lines(ts(PIL), col =2)
lines(ts(PIU), col =3)
lines(bs_fc_y$fcs, col = 4)
# bs sd
PIL = bs_fc_y$fcs - 1.96 * bs_fc_sd 
PIU = bs_fc_y$fcs + 1.96 * bs_fc_sd 
yanfei_MSIS2 = mean((PIU - PIL +
                      (2 / gamma) * (PIL - yout) * (PIL > yout) +
                      (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))
plot(ts(yout), ylim = c(min(PIL, yout),max(PIU, yout)), main = paste0(tsi,' yanfei MSIS2:', round(yanfei_MSIS2, 2)))
lines(ts(PIL), col =2)
lines(ts(PIU), col =3)
lines(bs_fc_y$fcs, col = 4)
CIs = bs_fc_CI(yresults$similarfcs)
PIU = CIs[2, ]
PIL = CIs[1, ]
MSIS = mean((PIU - PIL +
               (2 / gamma) * (PIL - yout) * (PIL > yout) +
               (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))
MSIS

plot(ts(yout, frequency = frequency(y)))
for (j in 1:nrow(yresults$similarfcs)){
  fc <- ts(yresults$similarfcs[j, ], frequency = frequency(y))
  lines(fc, col = 'gray')
}
lines(ts(yout, frequency = frequency(y)))

plot(ts(yout, frequency = frequency(y)))
for (j in 1:nrow(bs_similarfcs)){
  fc <- ts(bs_similarfcs[j, ], frequency = frequency(y))
  lines(fc, col = 'gray')
}
lines(ts(yout, frequency = frequency(y)))

accuracy_comp <- data.frame(fotios1 = c(fotios_MSIS1, fotios_MASE1),
                            fotios2 = c(fotios_MSIS2, fotios_MASE1),
                            yanfei1 = c(yanfei_MSIS1, yanfei_MASE),
                            yanfei2 = c(yanfei_MSIS2, yanfei_MASE),
                            ets = c(MSIS_ets, MASE_ets))
rownames(accuracy_comp) <- c('MSIS', 'MASE')
accuracy_comp
entropy(y)

load("~/Documents/Code/dejavu/data/M3_freq_1_h_6_k_500_Dist1.RData")
M3yearly.mean = as.data.frame(matrix(colMeans(M3_MSIS[, -25]), 4, 6))
M3yearly.mean[, 3:4] = M3yearly.mean[,3:4]*100
load("~/Documents/Code/dejavu/data/M3_freq_4_h_8_k_500_Dist1.RData")
M3quarterly.mean = as.data.frame(matrix(colMeans(M3_MSIS[,-25]), 4, 6))
M3quarterly.mean[, 3:4] = M3quarterly.mean[,3:4]*100
load("~/Documents/Code/dejavu/data/M3_freq_12_h_18_k_500_Dist1.RData")
M3monthly.mean = as.data.frame(matrix(colMeans(M3_MSIS[,-25]), 4, 6))
M3monthly.mean[, 3:4] = M3monthly.mean[,3:4]*100
colnames(M3yearly.mean) = colnames(M3quarterly.mean) = colnames(M3monthly.mean) = c('MSIS_option1', 'MSIS_option3', 'Coverage(%)', 'Uconverage(%)', 'Spread_option1', 'Spread_option3')
rownames(M3yearly.mean) = rownames(M3quarterly.mean) = rownames(M3monthly.mean) = c('ETS', 'SHD', 'Similarity', 'ETS-Similarity')
xtable::xtable(rbind(M3yearly.mean, M3quarterly.mean, M3monthly.mean), digits = 3)
xtable(M3yearly.mean, digits = 3)
xtable(M3quarterly.mean, digits = 3)
xtable(M3monthly.mean, digits = 3)

# delta & entropy
tsset = subset(M3, 1)
M3yearly.entropy = sapply(tsset, function(y){tsfeatures::entropy(y$x)})
tsset = subset(M3, 4)
M3quarterly.entropy = sapply(tsset, function(y){tsfeatures::entropy(y$x)})
tsset = subset(M3, 12)
M3monthly.entropy = sapply(tsset, function(y){tsfeatures::entropy(y$x)})

load("~/Documents/Code/dejavu/data/M3_freq_1_h_6_k_500_Dist1.RData")
M3yearly.entropy.delta = data.frame(Forecastability = 1 - M3yearly.entropy, Delta = M3_MSIS$opt.delta)
plot_center = ggplot(M3yearly.entropy.delta, aes(x=Forecastability, y = Delta)) + geom_point() + geom_smooth(method = 'loess', size = 1.5) + labs(y = expression(paste(delta^"*")), x = '') + theme(text = element_text(size = 12)) + ggtitle('Yearly')
p1 <- ggExtra::ggMarginal(plot_center, type = 'histogram') 
load("~/Documents/Code/dejavu/data/M3_freq_4_h_8_k_500_Dist1.RData")
M3quarterly.entropy.delta = data.frame(Forecastability = 1 - M3quarterly.entropy, Delta = M3_MSIS$opt.delta)
plot_center = ggplot(M3quarterly.entropy.delta, aes(x=Forecastability, y = Delta)) + geom_point() + geom_smooth(method = 'loess', size = 1.5) + labs(y ='', x = '')+ theme(text = element_text(size = 12)) + ggtitle('Quarterly')
p2 <- ggExtra::ggMarginal(plot_center, type = 'histogram')
load("~/Documents/Code/dejavu/data/M3_freq_12_h_18_k_500_Dist1.RData")
M3monthly.entropy.delta = data.frame(Forecastability = 1 - M3monthly.entropy, Delta = M3_MSIS$opt.delta)
plot_center = ggplot(M3monthly.entropy.delta, aes(x=Forecastability, y = Delta)) + geom_point() + geom_smooth(method = 'loess', size = 1.5) + labs(y = expression(paste(delta^"*"))) + theme(text = element_text(size = 12))+ ggtitle('Monthly') + xlim(c(-0.01,0.59))
p3 <- ggExtra::ggMarginal(plot_center, type = 'histogram')
# overall
M3.entropy.delta = rbind(M3yearly.entropy.delta, M3quarterly.entropy.delta, M3monthly.entropy.delta)
plot_center = ggplot(M3.entropy.delta, aes(x=Forecastability, y = Delta)) + geom_point() + geom_smooth(method = 'loess', size = 1.5) + labs(y = '')+ theme(text = element_text(size = 12))+ ggtitle('Overall') + xlim(c(-0.01,0.59))
p4 <- ggExtra::ggMarginal(plot_center, type = 'histogram')

gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

