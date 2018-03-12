##____________________________________________________________________________##
##  Function which extend a time series                                       ##
##  Florine Garcia - 20180306 - tsExtension                                   ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Extension of a daily or monthly time series with the chosen
#                  first and last dates, and transformation of irregular zoo in
#                  regular zoo
##----------------------------------------------------------------------------##
#   Arguments: tsData [zoo]: zoo object with the daily or monthly time series
#                (date format: "%Y-%m-%d")
#              startDate [character]: the first date for the extension (date
#                format: "%Y-%m-%d") 
#              endDate [character] : the last date for the extension (date
#                format: "%Y-%m-%d") 
##----------------------------------------------------------------------------##
#   Value: extTsData [zoo]: regular zoo object with the daily or monthly
#            extended time series (date format: "%Y-%m-%d")
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
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
  
  ##__Check_Regularity_zoo_and_ts_extension_if_necessary____________________####
  # --- get the time step
  timestep <- periodicity(tsData)$scale
  switch(timestep,
         "daily" = { tstp <- "days" },
         "monthly" = { tstp <- "months" }, {
           stop("unkown value for choice of timestep: ", timestep, "\n")
         })
  # --- Ts extension
  dates <- time(tsData)
  y <- as.numeric(format(dates, "%Y"))
  seqDates <- seq(as.Date(startDate), as.Date(endDate), by = tstp)
  
  ## Soit
  naDates <- zoo(rep(NA, length(seqDates)), as.Date(seqDates))
  winTsData <- window(tsData, start = startDate, end = endDate)
  extTsData <- merge.zoo(naDates, winTsData)
  extTsData <- extTsData[,-1]
  # --- Check regularity

  return(extTsData)
}
