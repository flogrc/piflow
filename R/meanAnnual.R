#'Calculate the mean annual value of a time series
#'
#'Calculate the mean annual value of a daily, monthly or annual time series
#'
#'@param  tsData [zoo] zoo object with the daily, monthly or annual time series
#'@param  ... further arguments passed to or from monthly2annual or daily2annual
#'
#'@return \emph{mA} [numeric] : named vector containing:
#' \itemize{  
#'   \item "value" the value of the computed mean annual value
#'   \item "samples" the number of data without missing values used to compute
#' the mean annual value
#' }
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'How to use function
#'
#'@references
#'
#'@details
#'
#'@seealso
#'\code{\link[piflow]{daily2annual}}
#'\code{\link[piflow]{monthly2annual}}
#-------------------------------------------------------------------------------
meanAnnual <- function(tsData, ...) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  if (!is.zoo(tsData)) { stop("tsData must be a zoo"); return(NULL) }
  
  ##__Computation_Depending_On_The_Time_Steps_______________________________####
  # --- Annual aggregation
  timestep <- periodicity(tsData)$scale
  switch(timestep,
         "yearly" = { annualData <- tsData },
         "monthly" = { annualData <- monthly2annual(tsData, ...) },
         "daily" = { annualData <- daily2annual(tsData, ...) }, {
           stop("unkown value for choice of timestep: ", timestep, "\n")
         })
  # --- mean Annual
  mA <- c(mean(annualData, na.rm = TRUE), length(which(!is.na(annualData))))
  names(mA) <- c("value", "samples")
  
  return(mA)
}