#'Extend a daily or monthly time series
#'
#'Extension of a daily or monthly time series with the chosen first and 
#'last dates, and transformation of irregular zoo in regular zoo by 
#'adding NA when missing dates.
#'
#'@param tsData [zoo] zoo object with the daily or monthly time series.
#'The date format must be:
#'\itemize{
#'  \item{"\%Y-\%m-\%d" for daily data}
#'  \item{"\%Y-\%m-01" for monthly data}
#'}
#'@param startDate [Date] the first date of the extension. The date format 
#'must be:
#'\itemize{
  #'  \item{"\%Y-\%m-\%d" for daily data}
  #'  \item{"\%Y-\%m-01" for monthly data}
#'} (by default, first date of tsData: start(tsData))
#'@param endDate [Date] the last date of the extension. The date format 
#'must be:
#'\itemize{
#'  \item{"\%Y-\%m-\%d" for daily data}
#'  \item{"\%Y-\%m-01" for monthly data}
#'} (by default, last date of tsData: end(tsData))
#'
#'@return extTsData [zoo] regular zoo object with the daily or monthly extended 
#'time series. The date format is:
#'\itemize{
#'  \item{"\%Y-\%m-\%d" for daily data}
#'  \item{"\%Y-\%m-01" for monthly data}
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'

tsExtension <- function(tsData, startDate = start(tsData), endDate = end(tsData)) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(tsData)) { stop("tsData must be a zoo"); return(NULL) }
  if (!isDate(startDate)) {
    stop("startDate must be a date"); return(NULL)
  }
  if (!isDate(endDate)) {
    stop("endDate must be a date"); return(NULL)
  }
  # --- Check the time step and the date format
  if (!(periodicity(tsData)$scale %in% c("daily", "monthly"))) {
    stop("dailyData must be a daily or monthly time series \n"); return(NULL)
  }
  if (periodicity(tsData)$scale == "monthly") {
    if (format(startDate, "%d") != "01") {
      stop("format of 'startDate' must be '%Y-%m-01'")
    }
    if (format(endDate, "%d") != "01") {
      stop("format of 'endDate' must be '%Y-%m-01'")
    }
    if (sum(as.numeric(format(time(tsData), "%d")) - 1) != 0) {
      stop("format of 'tsData' must be '%Y-%m-01'")
    }
  }
  ##__Check_Regularity_Zoo_And_Ts_Extension_If_Necessary____________________####
  # --- Get the time step
  timestep <- periodicity(tsData)$scale
  switch(timestep,
         "daily" = { tstp <- "days" },
         "monthly" = { tstp <- "months" }, {
           stop("unkown value for choice of timestep: ", timestep, "\n")
         })
  # --- Ts extension
  seqDates <- seq(as.Date(startDate), as.Date(endDate), by = tstp)
  # --- NA fill preparation
  naDates <- zoo(rep(NA, length(seqDates)), as.Date(seqDates))
  winTsData <- window(tsData, start = startDate, end = endDate)
  # --- Merge for extension and filling
  extTsData <- merge.zoo(naDates, winTsData)
  extTsData <- extTsData[, -1]
  
  return(extTsData)
}
