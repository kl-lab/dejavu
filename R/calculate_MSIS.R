#' Benchmarking the perforamance of Similarity against ETS and SHD
#'
#' @param tsi ith particular reference series
#' @param gamma calibrated significance level quantiles
#' @param fh number of forecasting horizon
#' @param nts number of the most similar series
#' @param Dist_type number of forecasting benchmarks
#'
#' @return Dataframe containing theaccuracy of MSIS, Coverage, Upper coverage, Spread and MASE
#' using ETS, SHD, Similarity and ETS-Similarity separately
#' @author
#' @references Svetunkov & Petropoulos (2018)
#' @export
#'
#' @examples
calculate_MSIS <- function(tsi, gamma = 0.05, fh, nts = 100, Dist_type = 3){
  y = tsset[[tsi]]$x
  yout = as.vector(tsset[[tsi]]$xx[1:fh])
  yfreq = frequency(y)
  # Similarity
  # denom1
  PI_Similarity = Similarity(y, fh = fh, nts = nts, Dist_type = Dist_type, gamma = gamma, scaling = 'sdiff', LoadData = FALSE, FullOutput = TRUE)
  MASE_Similarity = mean(abs(yout - PI_Similarity$fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
  PIL_Similarity = PI_Similarity$PIL
  PIU_Similarity = PI_Similarity$PIU
  opt.delta = PI_Similarity$opt.delta
  MSIS_Similarity  = calculate_PI_MSIS(PIL_Similarity, PIU_Similarity, y, yout, scaling = 'sdiff')
  coverage_Similarity = calculate_PI_coverage(PIL_Similarity, PIU_Similarity, yout)
  ucoverage_Similarity = calculate_PI_ucoverage(PIL_Similarity, PIU_Similarity, yout)
  spread_Similarity = calculate_PI_spread(PIL_Similarity, PIU_Similarity, y, scaling = 'sdiff')
  # denom2
  PI_Similarity = Similarity(y, fh = fh, nts = nts, Dist_type = Dist_type, gamma = gamma, scaling = 'snaive', LoadData = FALSE, FullOutput = TRUE)
  PIL_Similarity = PI_Similarity$PIL
  PIU_Similarity = PI_Similarity$PIU
  opt.delta = PI_Similarity$opt.delta
  MSIS_rel_Similarity  = calculate_PI_MSIS(PIL_Similarity, PIU_Similarity, y, yout, scaling = 'snaive')
  spread_rel_Similarity = calculate_PI_spread(PIL_Similarity, PIU_Similarity, y, scaling = 'snaive')
  # ets
  fit = ets(y)
  fcs = as.vector(forecast(fit, h=fh)$mean)
  PI_ets = forecast(fit, h=fh, level=100*(1-gamma))
  PIL_ets = as.vector(PI_ets$lower)
  PIU_ets = as.vector(PI_ets$upper)
  MASE_ets = mean(abs(yout - fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
  MSIS_ets = calculate_PI_MSIS(PIL_ets, PIU_ets, y, yout, scaling = 'sdiff')
  MSIS_rel_ets = calculate_PI_MSIS(PIL_ets, PIU_ets, y, yout, scaling = 'snaive')
  coverage_ets = calculate_PI_coverage(PIL_ets, PIU_ets, yout)
  ucoverage_ets = calculate_PI_ucoverage(PIL_ets, PIU_ets, yout)
  spread_ets = calculate_PI_spread(PIL_ets, PIU_ets, y, scaling = 'sdiff')
  spread_rel_ets = calculate_PI_spread(PIL_ets, PIU_ets, y, scaling = 'snaive')
  # SHD
  ST <- F
  if (yfreq > 1){ ST <- SeasonalityTest(y, yfreq) }
  if (ST==T){
    Dec <- decompose(y,type="multiplicative")
    des_input <- y/Dec$seasonal
    SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-yfreq+1):length(Dec$seasonal)], fh), fh)
  }else{
    des_input <- y ; SIout <- rep(1, fh)
  }


  f1 <- ses(des_input, h=fh) #Ses
  f2 <- holt(des_input, h=fh, damped=F) #Holt
  f3 <- holt(des_input, h=fh, damped=T) #Damped
  fcs <- (f1$mean+f2$mean+f3$mean)*SIout/3 #Comb
  MASE_SHD = mean(abs(yout - fcs)) / mean(abs(diff(as.vector(y), lag=frequency(y))))
  PIL_SHD <- (f1$lower[, 2]+f2$lower[, 2]+f3$lower[, 2])*SIout/3 #Comb
  PIU_SHD <- (f1$upper[, 2]+f2$upper[, 2]+f3$upper[, 2])*SIout/3 #Comb
  MSIS_SHD = calculate_PI_MSIS(PIL_SHD, PIU_SHD, y, yout, scaling = 'sdiff')
  MSIS_rel_SHD = calculate_PI_MSIS(PIL_SHD, PIU_SHD, y, yout, scaling = 'snaive')
  coverage_SHD = calculate_PI_coverage(PIL_SHD, PIU_SHD, yout)
  ucoverage_SHD = calculate_PI_ucoverage(PIL_SHD, PIU_SHD, yout)
  spread_SHD = calculate_PI_spread(PIL_SHD, PIU_SHD, y, scaling = 'sdiff')
  spread_rel_SHD = calculate_PI_spread(PIL_SHD, PIU_SHD, y, scaling = 'snaive')
  # combination Similarity + ets
  PIL_comb = (PIL_Similarity + PIL_ets)/2
  PIU_comb = (PIU_Similarity + PIU_ets)/2
  MSIS_comb = calculate_PI_MSIS(PIL_comb, PIU_comb, y, yout, scaling = 'sdiff')
  MSIS_rel_comb = calculate_PI_MSIS(PIL_comb, PIU_comb, y, yout, scaling = 'snaive')
  coverage_comb = calculate_PI_coverage(PIL_comb, PIU_comb, yout)
  ucoverage_comb = calculate_PI_ucoverage(PIL_comb, PIU_comb, yout)
  spread_comb = calculate_PI_spread(PIL_comb, PIU_comb, y, scaling = 'sdiff')
  spread_rel_comb = calculate_PI_spread(PIL_comb, PIU_comb, y, scaling = 'snaive')

  accuracy_comp <- data.frame(MSIS_ets, MSIS_SHD, MSIS_Similarity, MSIS_comb,
                              MSIS_rel_ets, MSIS_rel_SHD, MSIS_rel_Similarity, MSIS_rel_comb,
                              coverage_ets, coverage_SHD, coverage_Similarity, coverage_comb,
                              ucoverage_ets, ucoverage_SHD,ucoverage_Similarity, ucoverage_comb,
                              spread_ets, spread_SHD, spread_Similarity, spread_comb,
                              spread_rel_ets, spread_rel_SHD, spread_rel_Similarity, spread_rel_comb,
                              MASE_Similarity, MASE_ets, MASE_SHD,
                              opt.delta)
  return(accuracy_comp)
}


calculate_PI_coverage <- function(PIL, PIU, yout){
  return(sum(yout <= PIU & yout >= PIL)/length(yout))
}

calculate_PI_ucoverage <- function(PIL, PIU, yout){
  return(sum(yout <= PIU)/length(yout))
}

calculate_PI_spread <- function(PIL, PIU, y, scaling = c('sdiff', 'snaive')){
    if (scaling[1] == 'sdiff')
    {
        denom = mean(abs(diff(as.vector(y), lag=frequency(y))))
    }else{
        PIL_naive = PIU_naive = rep(NA, length(y) - frequency(y) - 1)
        for (i in 1:(length(y) - frequency(y) - 1)){
            y.naive = snaive(head(y, i + frequency(y)), h=1, level=95)
            PIL_naive[i] = y.naive$lower
            PIU_naive[i] = y.naive$upper
        }
        denom = mean(PIU_naive - PIL_naive)
    }
    return(mean(PIU - PIL)/denom)
}

#' Calculate the mean scaled interval score(MSIS) to evaluate the performance of generated predictive intervals
#'
#' @param PIL lower bounds for the prediction interval
#' @param PIU upper bounds for the prediction interval
#' @param y "vector" the observation series
#' @param yout "vector" the generated series
#' @param gamma calibrated significance level quantiles
#' @param scaling scaling method: using different scaling or not
#'
#' @return The value calculated by MSIS function
#' @author
#' @export
#'
#' @examples
calculate_PI_MSIS <- function(PIL, PIU, y, yout, gamma = 0.05, scaling = c('sdiff', 'snaive')){
    if (scaling[1] == 'sdiff')
    {
        denom = mean(abs(diff(as.vector(y), lag=frequency(y))))
    }else{
        PIL_naive = PIU_naive = rep(NA, length(y) - frequency(y) - 1)
        for (i in 1:(length(y) - frequency(y) - 1)){
            y.naive = snaive(head(y, i + frequency(y)), h=1, level=100*(1-gamma))
            PIL_naive[i] = y.naive$lower
            PIU_naive[i] = y.naive$upper
        }
        yout.naive = tail(y, length(y) - frequency(y) - 1)
        denom = mean((PIU_naive - PIL_naive +
                      (2 / gamma) * (PIL_naive - yout.naive) * (PIL_naive > yout.naive) +
                      (2 / gamma) * (yout.naive - PIU_naive) * (PIU_naive < yout.naive)))
    }
    PI_MSIS = mean((PIU - PIL +
                    (2 / gamma) * (PIL - yout) * (PIL > yout) +
                    (2 / gamma) * (yout - PIU) * (PIU < yout))) / denom
    return(PI_MSIS)
}
