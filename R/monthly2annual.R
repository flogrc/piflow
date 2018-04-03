#'Calculate annual data from monthly data
#'
#'Transform a monthly time series into an annual one. The user can give a
#' threshold of missing values authorized per year for the aggregation and choose
#' the day to start the year (hydrological year or not)
#'
#'@param  monthlyData [zoo] zoo object with the monthly time series. The date
#'format must be "\%Y-\%m-\%d"
#'@param  FUN [function] function that have to be applied for the annual
#'aggregation (by default: mean)
#'@param  startYear [character] indicate the month to start the hydrological
#'year. The format must be "MM-01" (by default, calendar year: "01")
#'@param threshold [numeric] threshold of missing values authorized to compute
#'the annual aggregation. Values must be between 0 and 1 (by default, 10\%: 0.1)
#'@param na.rm [boolean] boolean vector (by default, TRUE)
#'@param ... further arguments
#'
#'@return \emph{\code{annualData}} [zoo] zoo object with the annual computed
#'time series. The date is in the format "\%Y-MM-01", corresponding of the start
#' month of the year
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'monthly2annual(monthlyprec, FUN = sum, startYear = "08-01", threshold = 0.1,
#'na.rm =TRUE)

#   WARNINGS : Fonction modifiee le 20170802 par Florine Garcia :
#                - ajout option calcul annee hydro
#                - Aai calcule dans la fonction
#     !!!!! Meme format de sortie quelque soit le type de calcul. Exemple : Pour
#     les annees civiles, sortie 1959-01-01 correspond au calcul du 01/01/1959
#     au 31/12/1959. Pour une annee hydro demarrant le 01/08, sortie 1959-01-01
#     correspond au calcul du 01/08/1958 au 31/07/1959.

monthly2annual <- function(monthlyData, startYear = "01-01", FUN = mean,
                           na.rm = TRUE, threshold = 0.1) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(monthlyData)) { stop("monthlyData must be a zoo"); return(NULL) }
  # --- Checking time step
  if (sfreq(monthlyData) != "monthly") {
    stop("monthlyData must be a monthly serie \n"); return(NULL)
  }
  if (!is.character(startYear)) {
    stop("startYear must be a character"); return(NULL)
  }
  if (substr(startYear, 4, 5) != "01") {
    stop("startYear format must be 'MM-01'"); return(NULL)
  }
  if (threshold > 1) {
    threshold <- threshold / 100
    warning("threshold was > 1 and then divided by 100")
  }
  
  ## Translation si option annee hydro
  dates <- time(monthlyData)
  y <- as.numeric(format(dates, "%Y"))
  if (startYear != "01-01") {
    if (dates[1] != as.Date(paste0(y[1], "-", startYear))) {
      if (dates[1] < as.Date(paste0(y[1], "-", startYear))) {
        inday <- which(dates == as.Date(paste0(y[1], "-", startYear)))
        monthlyData <- monthlyData[inday:length(monthlyData)]
      }
      if (dates[1] > as.Date(paste0(y[1], "-", startYear))) {
        ldays <- seq(as.Date(paste0(y[1], "-", startYear)), dates[1],
                     by = "months")
        ldata <- zoo(rep(NA, length(ldays)-1), as.Date(ldays[-length(ldays)]))
        monthlyData <- c(ldata, monthlyData)
      }
      dates <- time(monthlyData)
      y <- as.numeric(format(dates, "%Y"))
    }
    iday <- which(dates == as.Date(paste0(y[1]+1, "-01-01")))
    hdates <- dates[iday:length(dates)]
    mdays <- seq(hdates[length(hdates)], as.Date(paste0(y[length(y)]+1,
                                                        "-12-01")),
                 by = "months")
    mdays <- mdays[-1]
    hdates <- c(hdates, mdays)
    time(monthlyData) <- hdates[1:length(dates)]
    dates <- time(monthlyData)
    y <- as.numeric(format(dates, "%Y"))
  }
  
  ## Calcul du nombre de lacunes par an
  Aai <- dwi(monthlyData, out.unit = "years")
  
  ## Aggregation au pas de temps annuel
  years <- factor(y, levels = unique(y))
  if (!identical(FUN, sum)) {
    annualData <- aggregate(monthlyData, by = years, FUN = FUN, na.rm = na.rm)
  } else {
    annualData <- aggregate(monthlyData, by = years, FUN = mean, na.rm = na.rm)
  }
  
  ## Gestion des NA
  nanIndex <- which(is.nan(annualData))
  if (length(nanIndex) > 0) 
    annualData[nanIndex] <- NA
  
  ## Prise en compte du seuil de lacunes
  thresholdYear <- round(12*(1-threshold), 0)
  annualData[coredata(Aai) < thresholdYear] <- NA
  if (identical(FUN, sum)) {
    coredata(annualData) <- coredata(annualData) * 12
  }
  
  ## Mise en forme de la date "%Y-%m-%d"
  time(annualData) <- as.Date(paste0(time(annualData), "-01-01"))
  annualData <- zoo(as.numeric(annualData), time(annualData))
  return(annualData)
}
