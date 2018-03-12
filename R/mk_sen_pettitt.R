##____________________________________________________________________________##
##  Function to calculate Mann-Kendall, Pettitt, Sen                          ##
##  Pierre L'HERMITE - 2017-07-31 - mk_sen_pettitt.R                          ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Obtain trend with Mann-Kendall test, breaking point with 
#                Pettitt test and Sen test to have the coefficient of 
#                regression 
##----------------------------------------------------------------------------##
#   Arguments: data [zoo] : vector with daily, monthly, seasonnal or annual
#                           with date in %Y-%m-%d
##----------------------------------------------------------------------------##
#   Values: res [list] : res_mk [] : resume of Mann-Kendall test
#                        symbol_mk [vect] : vector with MK symbol according 
#                        to pvalue of MK test ("-" if pvalue > 0.1, "+" if
#                        0.1 > pvalue > 0.05, "++" if 0.05 > pvalue > 0.01, and
#                        "+++" if pvalue < 0.01)
#                        pettitt [] : resume of Pettitt test
#                        symbol_pettitt [vect] : vector with Pettitt symbol 
#                        according to Pettitt test ("-" if pvalue > 0.1, "+" if
#                        0.1 > pvalue > 0.05, "++" if 0.05 > pvalue > 0.01, and
#                        "+++" if pvalue < 0.01)
#                        slope [numeric] : slope according to Sen test
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

mk_sen_pettitt <- function(data)
{
  ##__Checking______________________________________________________________####
  # Data input checking
  if (missing(data)) {stop("Must have data \n"); return(NULL)}
  
  if (!is.zoo(data)) {stop("Data must be a zoo"); return(NULL)}
  
  ##__Calculation___________________________________________________________####
  # Remove NA value from zoo data
  na.index <- which(is.na(data))
  
  if (length(na.index) != 0) {
    data <- data[-na.index]
  } else {
    data
  }
  
  # Transformation data into time series for different test
  val <- coredata(data)
  datats <- ts(val, start = 1, end = length(val))
  
  # Different test: Pettitt, MK and Sen
  pettitt <- pettitt.test(datats)
  slope <- sens.slope(datats, conf.level = 0.95)
  res_mk <- mk.test(datats)
  
  if (res_mk$p.value <= 0.01) {
    (symbol_mk <- "+++")
  } else if (res_mk$p.value >= 0.01 & res_mk$p.value <= 0.05) {
    (symbol_mk <- "++")
  } else if (res_mk$p.value >= 0.05 & res_mk$p.value <= 0.1) {
    (symbol_mk <- "+")
  } else {
    (symbol_mk <- "-")
  }
  
  if (pettitt$p.value <= 0.01) {
    (symbol_pettitt <- "+++")
  } else if (pettitt$p.value >= 0.01 & pettitt$p.value <= 0.05) {
    (symbol_pettitt <- "++")
  } else if (pettitt$p.value >= 0.05 & pettitt$p.value <= 0.1) {
    (symbol_pettitt <- "+")
  } else {
    (symbol_pettitt <- "-")
  }
  
  res <- list(res_mk, symbol_mk, pettitt, symbol_pettitt, 
              as.numeric(slope[1]))
  return(res)
}