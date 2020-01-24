#! /usr/bin/env Rscript
cmdargs <- commandArgs(TRUE)

library(Mcomp)
frequency = as.numeric(cmdargs[1])
fh = as.numeric(cmdargs[2])
nts = as.numeric(cmdargs[3])
Dist_type = as.numeric(cmdargs[4])
tsset = subset(M3, frequency)
tsn = length(tsset)

source("../R/forecasting_engine.R")
source('../R/calculate_MSIS.R')
load(paste0("../reference/ref", frequency, ".RData"))
load(paste0("../reference/ref", frequency, "_",1,".RData"))

library(doMC)
registerDoMC(cores = detectCores())
system.time(
M3_MSIS <- 
  foreach(tsi = seq_along(tsset), .combine = rbind) %dopar% {
  calculate_MSIS(tsi, 0.05, fh = fh, nts = nts, Dist_type = Dist_type)
  }
)

M3_summary <- rbind(apply(M3_MSIS, 2, mean),
                    apply(M3_MSIS, 2, median))
rownames(M3_summary) <- c('mean', 'median')
write.table(round(t(M3_summary),3), paste0("../data/M3summary", "_freq_", frequency, 
                                           "_h_", fh,"_k_", nts, 
                                           "_Dist", Dist_type, ".txt"))
save(M3_MSIS, file=paste0("../data/M3", "_freq_", frequency, 
                          "_h_", fh,"_k_", nts, 
                          "_Dist", Dist_type, ".RData"))

