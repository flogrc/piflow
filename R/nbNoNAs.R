#'Calculate the number of dates without missing values per year or month
#'
#'For a daily or a monthly time series, calculate the number of days without
#' missing values per year or per month
#'
#'@param  tsData [zoo] zoo object with the daily or monthly time series. The
#' date format must be:
#' \itemize{  
#'   \item "\%Y-\%m-\%d" for daily data
#'   \item "\%Y-\%m-01" for monthly data
#' }
#'@param  tstp [character] indicate the period for the calculation. Could be:
#' \itemize{  
#'   \item "years": to calculate the number of days or months per year
#'   \item "months": to calculate the number of days per month
#'   \item "mPerY": to calculate the number of days per month per year
#' }
#'@param  hydroYear [numeric] vector of the translated years when calculating
#' the number of dates per hydrological year (by default, calendar year: NULL)
#'
#'@return \emph{missV} [zoo] : zoo object with the annual or monthly computed
#' time series of number of dates without missing values. The date is in the
#' format: "\%Y-MM-01", corresponding of the start of the year
#' \itemize{  
#'   \item "\%Y-MM-01" for annual computation, corresponding of the start month of the year
#'   \item "\%Y-\%m-01" for monthly computation
#' }
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'
#'
#'@details
#-------------------------------------------------------------------------------
nbNoNAs <- function(tsData, tstp = "years", hydroYear = NULL) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(tsData)) { stop("tsData must be a zoo"); return(NULL) }
  if (!is.character(tstp)) { stop("tstp must be a character"); return(NULL) }
  
  ##__Define_The_Time_Step__________________________________________________####
  switch(tstp,
         "years" = {
           if (exists("hydroYear")) {
             if (!is.numeric(hydroYear)) {
               stop("hydroYear must be a numeric"); return(NULL)
             }
             tstpId <- factor(hydroYear, levels = unique(hydroYear))
           } else {
             y <- as.numeric(format(time(tsData), "%Y"))
             tstpId <- factor(y, levels = unique(y))
           }
         },
         "months" = {
           m <- as.numeric(format(time(tsData), "%m"))
           tstpId <- factor(m, levels = unique(m))
         },
         "mPerY" = {
           mpy <- format(time(tsData), "%Y-%m")
           tstpId <- factor(mpy, levels = unique(mpy))
         }, {
           stop("unkown value for choice of tstp: ", tstp, "\n")
         })
  
  ##__Calculate_Numer_Of_Days_Without_Missing_Value_________________________####
  missV <- aggregate(tsData, by = tstpId, FUN = function(x) {
    return(length(which(!is.na(x))))
  })
  
  if (tstp == "mPerY") {
    missV <- zoo(coredata(missV), time(tsData))
  }
  return(missV)
}
