#'Calculate Mann-Kendall, Pettitt, Sen tests
#'
#'Obtain trend with Mann-Kendall test, breaking point with 
#'Pettitt test and Sen test to have the coefficient of regression 
#'
#'@param  data [zoo] : vector with daily, monthly, seasonnal or annual 
#'with date in \%Y-\%m-\%d
#'
#'@return \emph{res_test} [list] : list that contains
#'\itemize{
#'\item \emph{$issp} [zoo] : zoo with the issp values with date in \%Y-\%m-\%d
#'\item \emph{$length_zoo} [zoo] : zoo with the length of drought with date in
#'\%Y-\%m-\%d [day]
#'\item \emph{$drought_type} [zoo] : zoo with the type of the period for each month
#'\item \emph{$drought_number} [data.frame] : dataframe with the number of different
#'period by type:
#'\itemize{
#'\item Extwet (issp > 2)\cr
#'\item Verywet (1.99 > issp > 1.5)\cr
#'\item Wet (1.49 > issp > 1)\cr
#'\item Normal (0.99 > issp > -0.99)\cr
#'\item Dry (-1 > issp > -1.49)\cr
#'\item VeryDry (-1.5 > issp > -1.99)\cr
#'\item ExtDry (-2 > issp))}
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'How to use function
#'
#'@references
#'
#'@details
#'
#'@seealso
#'\code{\link[package name]{function name}}

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
  
  res_test <- list(res_mk, symbol_mk, pettitt, symbol_pettitt, 
              as.numeric(slope[1]))
  return(res_test)
}