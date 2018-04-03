#'Calculate Mann-Kendall, Pettitt, Sen tests
#'
#'Obtain trend with Mann-Kendall test, breaking point with 
#'Pettitt test and Sen test to have the coefficient of regression 
#'
#'@param  data [zoo] vector with daily, monthly, seasonnal or annual 
#'with date in \%Y-\%m-\%d
#'
#'@return \emph{res_test} [list] list that contains
#'\itemize{
#'\item \emph{$res_mk} [list] resume of Mann-Kendall test
#'\item \emph{$symbol_mk} [zoo] vector with MK symbol according 
#'to pvalue of MK test:
#'\itemize{
#'\item "-" if pvalue > 0.1\cr
#'\item "+" if 0.1 > pvalue > 0.05\cr
#'\item "++" if 0.05 > pvalue > 0.01\cr
#'\item "+++" if pvalue < 0.01\cr
#'}
#'\item \emph{$pettitt} [list] resume of Pettitt test
#'\item \emph{$symbol_pettitt} [zoo] vector with Pettitt symbol according to
#'Pettitt test:
#'\itemize{
#'\item "-" if pvalue > 0.1\cr
#'\item "+" if 0.1 > pvalue > 0.05\cr
#'\item "++" if 0.05 > pvalue > 0.01\cr
#'\item "+++" if pvalue < 0.01\cr
#'}
#'\item\emph{$slope} [numeric] slope according to Sen test
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'result <- mk_sen_pettitt(data)

mk_sen_pettitt <- function(data)
{
  library(trend)
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
  symbol_mk <- zoo(symbol_mk, index(data))
  
  if (pettitt$p.value <= 0.01) {
    (symbol_pettitt <- "+++")
  } else if (pettitt$p.value >= 0.01 & pettitt$p.value <= 0.05) {
    (symbol_pettitt <- "++")
  } else if (pettitt$p.value >= 0.05 & pettitt$p.value <= 0.1) {
    (symbol_pettitt <- "+")
  } else {
    (symbol_pettitt <- "-")
  }
  symbol_pettitt <- zoo(symbol_pettitt, index(data))
  
  res_test <- list(res_mk, symbol_mk, pettitt, symbol_pettitt, 
                   slope = as.numeric(slope[1]))
  return(res_test)
}