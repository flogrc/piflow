#'Create a vector for the computation of hydrological years
#'
#'Creation of a vector years translated to compute statistics on
#'hydrological years
#'
#'@param tsData [zoo] zoo object with the daily or monthly time series.
#'The date format must be:
#'\itemize{
#'  \item{"\%Y-\%m-\%d" for daily data}
#'  \item{"\%Y-\%m-01" for monthly data}
#'}
#'@param startYear [character] indicate the month to start the hydrological year.
#'The format must be "MM" (by default, calendar year: "01")
#'
#'@return years [character] vector of the translated years
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'

yearTranslation <- function(tsData, startYear) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(tsData)) { stop("tsData must be a zoo"); return(NULL) }
  if (!is.character(startYear)) {
    stop("startDate must be a character"); return(NULL)
  }
  # --- Check the time step and the date format
  if (!(periodicity(tsData)$scale %in% c("daily", "monthly"))) {
    stop("dailyData must be a daily or monthly time series \n"); return(NULL)
  }
  # --- Check the start of the year
  if (nchar(startYear) != 2) {
    stop("startYear format must be 'MM'"); return(NULL)
  }
  
  ##__year_Translation______________________________________________________####
  dates <- time(tsData)
  y <- as.numeric(format(dates[1], "%Y"))
  startDate <- paste0(y, "-", startYear, "-01")
  # --- Dates before start
  if (dates[1] < startDate) {
    datesBefore <- dates[1:(which(dates == startDate) - 1)]
    yBefore <- as.character(rep((y - 1), length(datesBefore)))
    dates <- dates[which(dates == startDate):length(dates)]
  }
  # --- Translation
  timestep <- periodicity(tsData)$scale
  switch(timestep,
         "daily" = { tstp <- "days" },
         "monthly" = { tstp <- "months" }, {
           stop("unkown value for choice of timestep: ", timestep, "\n")
         })
  years <- format(seq(as.Date(startDate), by = tstp, length.out = length(dates)),
                  "%Y")
  
  if (exists("yBefore")) {
    years <- c(as.character(yBefore), years)
  }
  
  return(years)
}
