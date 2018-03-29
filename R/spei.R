#'Calculate SPEI
#'
#'Calculate the standardized precipitation-evaportranspiration index and the 
#'drought specifications 
#'
#'@param  prec_data [zoo] rainfall monthly data in zoo class with date 
#'in \%Y-\%m-\%d
#'@param  evapo_data [zoo] evapotranspiration monthly data in zoo class 
#'with date in \%Y-\%m-\%d
#'@param  time_step [numeric] by default = 12, time step to sum monthly data
#'(1, 3, 6, 9, 12, 24 and 48)
#'@param  distribution [character] distribution of data 
#'(log_Logistic, gamma, grev, genlog, normal)
#'
#'@return \emph{resspei} [list] : list that contains
#'@return \emph{output 2} [class] : explanation [unit]
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
#'\code{\link[package name]{function name}}

#                           (spei, length_zoo, drought_type, drought_number)
#           spei [zoo] : zoo with the spei values with date in %Y-%m-%d
#           length_zoo [zoo] : zoo with the length of drought with date
#                              in %Y-%m-%d
#           drought_type [zoo] : zoo with the type of the period for
#                                 each month 
#           drought_number [dataframe] : dataframe with the number of 
#                           different period by type
#                           Extwet [spei>2], Verywet [1.99>spei>1.5],
#                           wet [1.49>spei>1], Normal [0.99>spei>-0.99],
#                           Dry [-1>spei>-1.49], VeryDry [-1.5>spei>-1.99],
#                           ExtDry [-2>spei])
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

spei <- function(prec_data, evapo_data, time_step = 12,
                 distribution = "log-Logistic") {
  
  ##__Checking______________________________________________________________####
  # Data input checking
  if (!is.zoo(prec_data)) { stop("prec_data must be a zoo"); return(NULL)}
  if (!is.zoo(evapo_data)) { stop("evapo_data must be a zoo"); return(NULL)}
  
  # Time step checking
  if (periodicity(prec_data)$scale != "monthly") {
    stop("prec_data must be a monthly serie \n"); return(NULL)
  }
  if (periodicity(evapo_data)$scale != "monthly") {
    stop("evapo_data must be a monthly serie \n"); return(NULL)
  }
  
  ##__Calculation___________________________________________________________####
  library(SPEI)
  
  diff <- prec_data - evapo_data
  
  # Using SPEI package to calculate spei
  res_spei <- spei(coredata(diff[which(!is.na(diff))]), scale = time_step,
                   distribution = distribution, na.rm = TRUE)
  spei <- zoo(as.numeric(res_spei$fitted),
              order.by = index(diff[which(!is.na(diff))]))
  
  ##__Index analysis________________________________________________________####
  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <- 0
  drought_type <- rep(NA, length(spei))
  
  for (i in 1:length(coredata(spei))) {
    if (is.na(coredata(spei)[i])) {
    } else if ((coredata(spei)[i] >= 3)) {
      ext_wet <- ext_wet + 1
      drought_type[i] <- 3
    } else if ((2.99 > coredata(spei)[i]) && (coredata(spei)[i] > 2)) {
      very_wet <- very_wet + 1
      drought_type[i] <- 2
    } else if ((1.99 > coredata(spei)[i]) && (coredata(spei)[i] > 1)) {
      wet <- wet + 1
      drought_type[i] <- 1
    } else if ((0.99 > coredata(spei)[i]) && (coredata(spei)[i] > -0.99)) {
      normal <- normal+1
      drought_type[i] <- 0
    } else if ((-1 >= coredata(spei)[i]) && (coredata(spei)[i] > -1.99)) {
      dry <- dry + 1
      drought_type[i] <- - 1
    } else if ((-2 >= coredata(spei)[i]) && (coredata(spei)[i] > -2.99)) {
      very_dry <- very_dry + 1
      drought_type[i] <- - 2
    } else if ((coredata(spei)[i] <= -3)) {
      ext_dry <- ext_dry + 1
      drought_type[i] <- - 3
    } else {}
  }
  
  drought_number <- rbind.data.frame(ext_wet, very_wet, wet, normal, dry,
                                     very_dry, ext_dry)
  colnames(drought_number) <- c("Rain gauge")
  row.names(drought_number) <- c("Extreme Wet", "Very Wet", "Wet", "Normal",
                                 "Dry", "Very Dry", "Extreme Dry")
  
  # Calculation of the drought length
  length_drought <- numeric()
  
  n <- 0
  p <- 0
  for (ilength in 1:length(spei)) {
    if (is.na(spei[ilength])){
      length_drought[ilength] <- NA
    } else if (spei[ilength] > 0) {
      n <- 0
      p <- p + 1
      length_drought[ilength] <- p
    } else {
      p <- 0
      n <- n - 1 
      length_drought[ilength] <- n
    }
  }
  
  length_zoo <- zoo(as.numeric(length_drought), index(spei))
  
  resspei <- list(spei = spei, drougth_length = length_zoo,
                 drought_number_type = drought_number, type_time = drought_type)
  
  return(Resultat)
}