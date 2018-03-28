#'Calculate annual data from monthly data
#'
#'Transform a monthly time series into an annual one. The user can give a
#' threshold of missing values authorized per year for the aggregation and choose
#' the day to start the year (hydrological year or not)
#'
#'@param  monthlyData [zoo] zoo object with the monthly time series. The date format
#' must be "\%Y-\%m-01"
#'@param  FUN [function] function that have to be applied for the annual
#' aggregation (by default: mean)
#'@param  startYear [character] indicate the month to start the hydrological
#' year. The format must be "MM" (by default, calendar year: "01")
#'@param threshold [numeric] threshold of missing values authorized to compute
#' the annual aggregation. Values must be between 0 and 1 (by default, 10\%: 0.1)
#'@param ... further arguments
#'
#'@return \emph{annualData} [zoo] : zoo object with the annual computed time
#' series. The date is in the format "\%Y-MM-01", corresponding of the start
#' month of the year
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'monthly2annual(monthlyPrec, FUN = sum, startYear = "08", threshold = 0.1)
#'
#'@details
#-------------------------------------------------------------------------------
monthly2annual <- function(monthlyData, FUN = mean, startYear = "01", threshold = 0.1, ...) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(monthlyData)) { stop("monthlyData must be a zoo"); return(NULL) }
  if (!is.function(FUN)) { stop("FUN must be a function"); return(NULL) }
  if (!is.character(startYear)) {
    stop("startYear must be a character"); return(NULL)
  }
  if (!is.numeric(threshold)) {
    stop("threshold must be a numeric"); return(NULL)
  }
  # --- Check the time step and the date format
  if (periodicity(monthlyData)$scale != "monthly") {
    stop("monthlyData must be a monthly time series \n"); return(NULL)
  }
  # --- Check the start of the year
  if (nchar(startYear) != 2) {
    stop("startYear format must be 'MM'"); return(NULL)
  }
  # --- Check the threshold value
  if ((threshold < 0) | (threshold > 1)) {
    stop("threshold must be in [0; 1]")
  }
  
  ##__Time_Series_Extension_And_Time_Translate______________________________####
  # --- Time series extension if irregular time series
  dates <- time(monthlyData)
  y <- as.numeric(format(dates, "%Y"))
  monthlyData <- tsExtension(monthlyData)
  
  # --- Time translation if the computation is not the calendar year
  hydroYear <- yearTranslation(monthlyData, startYear)
  
  ##__Annual_Aggregation____________________________________________________####
  years <- factor(hydroYear, levels = unique(hydroYear))
  if (!identical(FUN, sum)) {
    annualData <- aggregate(monthlyData, by = years, FUN = FUN, na.rm = TRUE, ...)
  } else {
    annualData <- aggregate(monthlyData, by = years, FUN = mean, na.rm = TRUE)
  }
  
  ##__Handle_NA_Values______________________________________________________####
  nan.index <- which(is.nan(annualData))
  if (length(nan.index) > 0) { annualData[nan.index] <- NA }
  # --- Number of days per year without NA values
  adi <- nbNoNAs(monthlyData, hydroYear = hydroYear, tstp = "years")
  # --- Take the threshold of NAs into account
  ## Number of total days per year
  yearDays <- aggregate(hydroYear, by = list(years), FUN = function(x) {
    return(length(x))
  })
  yearDays <- yearDays$x
  ## Threshold
  thresholdYear <- round(yearDays*(1 - threshold), 0)
  annualData[coredata(adi) < thresholdYear] <- NA
  
  if (identical(FUN, sum)) {
    coredata(annualData) <- coredata(annualData) * yearDays
  }
  
  ##__Output_Date_Format____________________________________________________####
  annualData <- zoo(as.numeric(annualData),
                    as.Date(paste0(time(annualData), "-", startYear, "-01")))
  return(annualData)
}
