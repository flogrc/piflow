##____________________________________________________________________________##
##  Function which ...                                                        ##
##  Pierre L'Hermite - 20180115 - fc.SDI.R                                    ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : 
##----------------------------------------------------------------------------##
#   Arguments : varIn1 [class] : 
#               varIn2 [class] : 
#               ...
##----------------------------------------------------------------------------##
#   Sortie : varOut [class] : 
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
sdi <- function(MonthlyFlow, Period) {
  ## Verification arguments d'entree
  if (!is.zoo(MonthlyData)) { stop("MonthlyData must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(MonthlyData) != "monthly") {
    stop("MonthlyData must be a daily serie \n"); return(NULL)
  }
  
  #Somme selon le pas de temps
  tmp_mean <- as.data.frame(matrix(NA, ncol = Delta, nrow = length(MonthlyData)))
  for(i in 1:Delta) {
    tmp_mean[, i] <- coredata(MonthlyData)[i:(length(MonthlyData)+i-1)]
  }
  somme <- apply(tmp_mean, 1, sum, na.rm = FALSE)
  
  
  return(varOut)
}
