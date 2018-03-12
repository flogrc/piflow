##____________________________________________________________________________##
##  Function to calculate annual data from monthly data                       ##
##  Florine Garcia - 20151221 - fc.monthly2annual                             ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Transformer les donnees mensuelles en donnees annuelles avec
#     le niveau de lacunes autorisees donnes par l'utilisateur et choix de
#     l'annee de calcul civile ou hydro
##----------------------------------------------------------------------------##
#   Arguments : monthlyData [zoo] : vecteur contenant chaque donnee par mois
#                 (date au format "%Y-%m-%d")
#               debYear [character]: debut de l'annee pour le calcul de la
#                 moyenne au format "MM-01" (par defaut annee civile "01-01")
#               FUN [function] : fonction a appliquer annuellement (par defaut
#                 mean)
#               na.rm [boolean] : vecteur booleen, argument de FUN (par defaut
#                 TRUE)
#               threshold [numeric] : seuil autorise pour la prise en compte des
#                 lacunes (par defaut 10%)
##----------------------------------------------------------------------------##
#   Sortie : annualData [zoo] : vecteur contenant chaque donnee par annee (date
#              au format "%Y-%m-%d")
##----------------------------------------------------------------------------##
##----------------------------------------------------------------------------##
#   WARNINGS : Fonction modifiee le 20170802 par Florine Garcia :
#                - ajout option calcul annee hydro
#                - Aai calcule dans la fonction
#     !!!!! Meme format de sortie quelque soit le type de calcul. Exemple : Pour
#     les annees civiles, sortie 1959-01-01 correspond au calcul du 01/01/1959
#     au 31/12/1959. Pour une annee hydro demarrant le 01/08, sortie 1959-01-01
#     correspond au calcul du 01/08/1958 au 31/07/1959.
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
monthly2annual <- function(monthlyData, debYear = "01-01", FUN = mean,
                           na.rm = TRUE, threshold = 0.1) {
  ## Verification arguments d'entree
  if (!is.zoo(monthlyData)) { stop("monthlyData must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(monthlyData) != "monthly") {
    stop("monthlyData must be a monthly serie \n"); return(NULL)
  }
  if (!is.character(debYear)) {
    stop("debYear must be a character"); return(NULL)
  }
  if (substr(debYear, 4, 5) != "01") {
    stop("debYear format must be 'MM-01'"); return(NULL)
  }
  if (threshold > 1) {
    threshold <- threshold / 100
    warning("threshold was > 1 and then divided by 100")
  }
  
  ## Translation si option annee hydro
  dates <- time(monthlyData)
  y <- as.numeric(format(dates, "%Y"))
  if (debYear != "01-01") {
    if (dates[1] != as.Date(paste0(y[1], "-", debYear))) {
      if (dates[1] < as.Date(paste0(y[1], "-", debYear))) {
        inday <- which(dates == as.Date(paste0(y[1], "-", debYear)))
        monthlyData <- monthlyData[inday:length(monthlyData)]
      }
      if (dates[1] > as.Date(paste0(y[1], "-", debYear))) {
        ldays <- seq(as.Date(paste0(y[1], "-", debYear)), dates[1],
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
