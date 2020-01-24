library(Mcomp)
frequency = 4
horizon = 8
tsset = subset(M3, frequency)
tsn = length(tsset)

source("forecasting_engine.R")
load(paste0("reference/ref", frequency, ".RData"))
load(paste0("reference/ref", frequency, "_",1,".RData"))

clusters = 0
k = 100
gamma = 0.05

library(foreach)
library(doSNOW)
cl = registerDoSNOW(makeCluster(3, type = "SOCK"))

tsi = 495
cluster = 0

for (cluster in clusters){
  parloop <- foreach (tsi=1:tsn, .combine='rbind', .packages=c('forecast','robustbase','dtw')) %dopar% {
    
    write(paste(cluster, tsi), file = paste0("progress_m.txt"), append=FALSE)
    
    y = tsset[[tsi]]$x
    yout = as.vector(tsset[[tsi]]$xx)
    
    yresults = DoTheJob(y, fh=horizon, nts=k, Preprocessing=1, Dist_type=3, clustering=cluster, LoadData=FALSE, FullOutput=TRUE)
    
    fcs = as.vector(yresults$fcs)
    MASE = mean(abs(yout - fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
    #PIs = apply(yresults$similarfcs, 2, quantile, probs=c(0.025, 0.975))
    
    corrs = cor(data.frame(t(yresults$similarfcs)))
    diag(corrs) <- NA
    rho = mean(corrs, na.rm = TRUE)
    intervals = qt(1-gamma/2, k) * sqrt(((k-1)/k)*((1+rho)/(1-rho)+1/k)) * apply(yresults$similarfcs, 2, sd)
    PIL = fcs - intervals
    PIU = fcs + intervals
    MSIS = mean((PIU - PIL +
                       (2 / gamma) * (PIL - yout) * (PIL > yout) +
                       (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))
    
    fit = ets(y)
    fcs = as.vector(forecast(fit, h=horizon)$mean)
    PIL = as.vector(forecast(fit, h=horizon, level=100*(1-gamma))$lower)
    PIU = as.vector(forecast(fit, h=horizon, level=100*(1-gamma))$upper)
    MASE_ets = mean(abs(yout - fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
    MSIS_ets = mean((PIU - PIL +
                       (2 / gamma) * (PIL - yout) * (PIL > yout) +
                       (2 / gamma) * (yout - PIU) * (PIU < yout))) / mean(abs(diff(as.vector(y), lag=frequency(y))))
    
    yresults = list(fcs = yresults$fcs, similarfcs = yresults$similarfcs, 
                    similarDistances=yresults$similarDistances, k=yresults$k, MASE=MASE, MSIS=MSIS,
                    fcs_ets = fcs, MASE_ets=MASE_ets, MSIS_ets=MSIS_ets)
    
    save(yresults, file=paste0("M3/M3_M_", k, "_", cluster, "_", tsi, ".RData"))
    
  }
}

MASE = array(NA, c(2, tsn))
MSIS = array(NA, c(2, tsn))
AvgDistance = array(NA, tsn)
for (tsi in 1:tsn){
  print(tsi)
  load(file=paste0("M3/M3_M_100_0_", tsi, ".RData"))

  MASE[1,tsi] = yresults$MASE_ets
  MASE[2,tsi] = yresults$MASE
  MSIS[1,tsi] = yresults$MSIS_ets
  MSIS[2,tsi] = yresults$MSIS
  AvgDistance[tsi] = mean(yresults$similarDistances)
}
rowMeans(MASE)
rowMeans(MSIS)