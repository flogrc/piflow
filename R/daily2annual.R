#'Function to calculate annual data from daily data
#'
#'Transform a daily regular time series into an annual one. The
#'user can give a threshold of missing values authorized per year for the
#'aggregation and choose the day to start the year (hydrological or not)
#'
#'@param  dailyData [zoo] zoo object with the daily time series. The date format
#'must be "\%Y-\%m-\%d"
#'@param  startYear [character] indicate the month to start the
#' hydrological year. The format must be "MM" (by default,
#' calendar year: "01")
#'@param  FUN [function] function that have to be applied for the annual
#'aggregation (by default: mean)
#'@param threshold [numeric] : threshold of missing values authorized to
#'compute the annual aggregation. Values must be between 0 and 1
#'(by default, 10\%: 0.1)
#'
#'@return \emph{annualData} [zoo] : zoo object with the annual computed time series.
#'The date is in the format "\%Y-MM-01", corresponding of the start of the year
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'daily2annual(dailyprec, startYear = "08", FUN = sum, threshold = 0.1)
#'
#'@details

daily2annual <- function(dailyData, startYear = "01", FUN = mean, threshold = 0.1) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(dailyData)) { stop("dailyData must be a zoo"); return(NULL) }
  if (!is.character(startYear)) {
    stop("startYear must be a character"); return(NULL)
  }
  if (!is.function(FUN)) { stop("FUN must be a function"); return(NULL) }
  if (!is.numeric(threshold)) {
    stop("threshold must be a numeric"); return(NULL)
  }
  # --- Check the time step and the date format
  if (periodicity(dailyData)$scale != "daily") {
    stop("dailyData must be a daily time series \n"); return(NULL)
  }
  # --- Check the start of the year
  if (nchar(startYear) != 2) {
    stop("startYear format must be 'MM'"); return(NULL)
  }
  # --- Check the threshold value
  if (threshold > 1) {
    stop("threshold must be < 1")
  }
  
  ##__Time_series_extension_and_Time_translate______________________________####
  # --- Time series extension
  dates <- time(dailyData)
  y <- as.numeric(format(dates, "%Y"))
  if (!is.regular(dailyData)) {
    dailyData <- tsExtension(dailyData)
  }
  # --- Time translation if the computation is not the calendar year
  hydroYear <- yearTranslation(dailyData, startYear)
  
  ##__Time_series_extension_and_Time_translate______________________________####
  ## Calcul du nombre de lacunes par an
  Adi <- nbNAs(dailyData, tstp = "years", hydroYear = hydroYear)
  
  ## Aggregation au pas de temps annuel
  # if (exists("hydroYear")) {
  years <- factor(hydroYear, levels = unique(hydroYear))
  # } else {
  #   years <- factor(y, levels = unique(y))
  # }
  if (!identical(FUN, sum)) {
    AnnualData <- aggregate(dailyData, by = years, FUN = FUN, na.rm = TRUE)
  } else {
    AnnualData <- aggregate(dailyData, by = years, FUN = mean, na.rm = TRUE)
  }
  
  ## Gestion des NA
  nan.index <- which(is.nan(AnnualData))
  if (length(nan.index) > 0) { AnnualData[nan.index] <- NA }
  
  ## Prise en compte du seuil de lacunes
  yearDays <- aggregate(hydroYear, by = list(hydroYear), FUN = function(x) {
    return(length(x))
  })
  yearDays <- yearDays$x
  thresholdYear <- round(yearDays*(1-threshold), 0)
  AnnualData[coredata(Adi) < thresholdYear] <- NA
  if (identical(FUN, sum)) {
    coredata(AnnualData) <- coredata(AnnualData) * yearDays
  }
  
  ## Mise en forme de la date "%Y-%m-%d"
  AnnualData <- zoo(as.numeric(AnnualData),
                    as.Date(paste0(time(AnnualData), "-", startYear)))
  return(AnnualData)
}
