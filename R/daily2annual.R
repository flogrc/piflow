##____________________________________________________________________________##
##  Function to calculate annual data from daily data                         ##
##  Florine Garcia - 20150520 - daily2annual                                  ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Function : Transform a daily regular time series into an annual one with
#     the threshold of missing values authorized per year given by the user and
#     the choice of the day to start the year (hydrological or not)
##----------------------------------------------------------------------------##
#   Arguments : dailyData [zoo] : zoo object with the daily time series (date
#                 format: "%Y-%m-%d")
#               startYear [character]: indicate the start of the hydrological
#                 year the fate format "MM-01" (default: calendar year, "01-01")
#               FUN [function] : function that have to be applied for the annual
#                 aggregation (by default mean)
#               threshold [numeric] : threshold of missing values authorized to
#                 compute the annual aggregation. Values must be between 0 and 1
#                 (by default 10%: 0.1)
##----------------------------------------------------------------------------##
#   Sortie : AnnualData [zoo] : zoo object with the annual computed time series
#              (date in the format "%Y-MM-01", corresponding of the start of the
#              year)
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#   WARNINGS : Fonction modifiee le 20170802 par Florine Garcia :
#                - ajout option calcul annee hydro
#                - Adi calcule dans la fonction
#     !!!!! Meme format de sortie quelque soit le type de calcul. Exemple : Pour
#     les annees civiles, sortie 1959-01-01 correspond au calcul du 01/01/1959
#     au 31/12/1959. Pour une annee hydro demarrant le 01/08, sortie 1959-01-01
#     correspond au calcul du 01/08/1958 au 31/07/1959.
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
daily2annual <- function(dailyData, startYear = "01-01", FUN = mean,
                         threshold = 0.1) {
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
  # --- Check the time step
  if (periodicity(dailyData)$scale != "daily") {
    stop("dailyData must be a daily time series \n"); return(NULL)
  }
  # --- Check the start of the year
  if (substr(startYear, 4, 5) != "01") {
    stop("startYear format must be 'MM-01'"); return(NULL)
  }
  # --- Check the threshold value
  if (threshold > 1) {
    threshold <- threshold / 100
    warning("threshold was > 1 and then divided by 100")
  }
  
  ##__Time_series_extension_and_Time_translate______________________________####
  # --- Time series extension
  dates <- time(dailyData)
  y <- as.numeric(format(dates, "%Y"))
  startDate <- paste0(y[1], "-", startYear)
  if (dates[1] < startDate) {
    startDate <- paste0((y[1] - 1), "-", startYear)
  }
  seqDates <- seq(as.Date(startDate, paste0(y[length(y)], startYear)), by = tstp)
  endDate <- seqDates[(length(seqDates) - 1)]
  rm(seqDates); gc()
  if (dates[length(dates)] > endDate) {
    extDates <- seq(endDate, paste0((y[length(y)] + 1), startYear), by = tstp)
    endDate <- extDates[(length(extDates) - 1)]
    rm(extDates); gc()
  }
  extData <- tsExtension(dailyData, startDate, endDate)
  
  # --- Time translation if the computation is not the calendar year
  hydroYear <- yearTranslation(extData, startYear)
  
  ##__Time_series_extension_and_Time_translate______________________________####
  ## Calcul du nombre de lacunes par an
  Adi <- nbNAs(tsData, tstp = "years", hydroYear = hydroYear)
  
  ## Aggregation au pas de temps annuel
  years <- factor(y, levels = unique(y))
  if (!identical(FUN, sum)) {
    AnnualData <- aggregate(dailyData, by = years, FUN = FUN, na.rm = TRUE)
  } else {
    AnnualData <- aggregate(dailyData, by = years, FUN = mean, na.rm = TRUE)
  }
  
  ## Gestion des NA
  nan.index <- which(is.nan(AnnualData))
  if (length(nan.index) > 0) { AnnualData[nan.index] <- NA }
  
  ## Prise en compte du seuil de lacunes
  yearDays <- rep(365, length(unique(y)))
  yearDays[IsLeapYear(unique(y))] <- 366
  thresholdYear <- round(yearDays*(1-threshold), 0)
  AnnualData[coredata(Adi) < thresholdYear] <- NA
  if (identical(FUN, sum)) {
    coredata(AnnualData) <- coredata(AnnualData) * yearDays
  }
  
  ## Mise en forme de la date "%Y-%m-%d"
  AnnualData <- zoo(as.numeric(AnnualData), as.Date(paste0(time(AnnualData),
                                                           "-01-01")))
  return(AnnualData)
}
