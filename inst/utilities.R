bs_fc <- function(similarfcs, n.bs = 100){
  similarfcs <- as.matrix(similarfcs)
  bs_similarfcs <- bs_fc_sd <- matrix(0, n.bs, ncol(similarfcs))
  for (i in 1:n.bs){
    bs_similarfcs[i, ] <- apply(similarfcs, 2, bs_median)
    bs_fc_sd[i, ] <- apply(similarfcs, 2, bs_sd)
  }
  
  fcs <- apply(bs_similarfcs, 2, median)
  fcs_sd <- apply(bs_fc_sd, 2, median)
  return(list(bs_similarfcs = bs_similarfcs,
              fcs = fcs,
              fcs_sd = fcs_sd))
}

bs_median <- function(x, size = 100){
  median(sample(x, size, replace = TRUE))
}

bs_sd <- function(x, size = 100){
  sd(sample(x, size, replace = TRUE))
}

bootQuantile <- function(x,i) {
  quantile(x[i], probs=.5)
}

bs_fc_CI <- function(similarfcs, alpha = 0.05, R = 100){
  CIs <- matrix(0, 2, ncol(similarfcs))
  for (i in 1:ncol(similarfcs)){
    boot.median <- boot(yresults$similarfcs[,i], bootQuantile, R = R)
    CIs[,i ] <- boot.ci(boot.median, conf=(1-alpha), type = 'bca')$bca[4:5]}
  return(CIs)
}

mean.boot <- function(x, ind) {
  c(mean(x[ind]), var(x[ind])/length(x))
}

bs_fc_CI <- function(similarfcs, alpha = 0.05, R = 1000){
  CIs <- matrix(0, 2, ncol(similarfcs))
  for (i in 1:ncol(similarfcs)){
    boot.mean <- boot(yresults$similarfcs[,i], mean.boot, R = R)
    CIs[,i ] <- boot.ci(boot.mean, conf=(1-alpha), type = 'bca')$bca[4:5]}
  return(CIs)
}