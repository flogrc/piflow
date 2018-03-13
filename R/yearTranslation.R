##____________________________________________________________________________##
##  Function which create a vector for the computation of hydrological years  ##
##  Florine Garcia - 20180306 - yearTranslation                               ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: creation of a vector years translated to compute statistics on
#                  hydrological years
##----------------------------------------------------------------------------##
#   Arguments: tsData [zoo]: zoo object with the daily or monthly time series
#                (date format: "%Y-%m-%d")
#              startYear [character]: indicate the start of the hydrological
#                 year the fate format "MM-01" (default: calendar year, "01-01")
##----------------------------------------------------------------------------##
#   Value: years [numeric]: vector of the translated years
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
yearTranslation <- function(tsData, startYear, ...) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(tsData)) { stop("tsData must be a zoo"); return(NULL) }
  if (!is.character(startYear)) {
    stop("startDate must be a character"); return(NULL)
  }
  
  ##__year_translation______________________________________________________####
  dates <- time(tsData)
  y <- as.numeric(format(dates[1], "%Y"))
  startDate <- paste0(y, "-", startYear)
  
  if (dates[1] < startDate) {
    iyear <- y - 1
  } else {
    iyear <- as.numeric(format(dates[1], "%Y"))
  }
  
  years <- c(iyear, rep(NA, (length(dates) - 1)))
  for (id in 2:length(dates)) {
    if (format(dates[id], "%m-%d") == startYear) {
      iyear <- iyear + 1
    }
    years[id] <- iyear
  }
  
  return(years)
}
