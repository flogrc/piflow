##____________________________________________________________________________##
##  Function to calculate Percent of Normal Precipitation (PNI)               ##
##  Pierre L'HERMITE - 2017-10-17 - pni.R                                     ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Calculation of percent of normal index (PNI)                 ##
##----------------------------------------------------------------------------##
#   Arguments: monthly_data [zoo] : rainfall monthly data in zoo class 
#                                   with date in %Y-%m-%d
#              time_step [numeric] : default = 12, time step to sum monthly 
#                                   precipitation (1, 3, 6, 9, 12, 24 and 48)
##----------------------------------------------------------------------------##
#   Values: respni [list] : list with 3 zoo et 1 dataframe
#                           (pni, length_zoo, drought_type, drought_number)
#           pni [zoo] : zoo with the pni values with date in %Y-%m-%d
#           length_zoo [zoo] : zoo with the length of drought with date
#                              in %Y-%m-%d
#           drought_type [zoo] : zoo with the type of the period for
#                                 each month 
#           drought_number [dataframe] : dataframe with the number of 
#                           different period by type
#                           Extwet [pni>2], Verywet [1.99>pni>1.5],
#                           wet [1.49>pni>1], Normal [0.99>pni>-0.99],
#                           Dry [-1>pni>-1.49], VeryDry [-1.5>pni>-1.99],
#                           ExtDry [-2>pni])
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

pni <- function(monthly_data, time_step = 12){
  
  ##__Checking______________________________________________________________####
  # Data input checking
  if (!is.zoo(monthly_data)) { stop("monthly_data must be a zoo"); return(NULL)}
  
  # Time step checking
  if (periodicity(monthly_data)$scale != "monthly") {
    stop("monthly_data must be a monthly serie \n"); return(NULL)
  }
  
  ##__Calculation___________________________________________________________####
  # Sum with time step
  tmp_mean <- as.data.frame(matrix(NA, ncol = time_step,
                                   nrow = length(monthly_data)))
  
  for(i in 1:time_step) {
    tmp_mean[, i] <- coredata(monthly_data)[i:(length(monthly_data) + i - 1)]
  }
  
  sum <- apply(tmp_mean, 1, sum, na.rm = FALSE)
  sum_mean <- mean(sum, na.rm = TRUE)
  sum_zoo <- zoo(sum, order.by = index(monthly_data))
  
  # Calculation of PNI
  pni <- (sum_zoo/sum_mean)*100
  
  ##__Index analysis________________________________________________________####
  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <- 0
  
  drought_type <- rep(NA, length(pni))
  
  for(ilength in 1:length(pni)){
    if( is.na(pni[ilength])){
    } else if (pni[ilength] >= 160){
      ext_wet <- ext_wet + 1
      drought_type[ilength] <- 3
    } else if ((pni[ilength] >= 145 && (pni[ilength] < 160))) {
      very_wet <- very_wet + 1
      drought_type[ilength] <- 2
    } else if ((pni[ilength] > 120 && (pni[ilength] < 145))) {
      wet <- wet + 1
      drought_type[ilength] <- 1
    } else if((120 >= pni[ilength]) && (pni[ilength] > 80)){
      normal <- normal + 1
      drought_type[ilength] <- 0
    } else if((80 >= pni[ilength]) && (pni[ilength] > 55)){
      dry <- dry + 1
      drought_type[ilength] <- - 1
    } else if((55 >= pni[ilength]) && (pni[ilength] > 40)){
      very_dry <- very_dry + 1
      drought_type[ilength] <- - 2
    } else if((pni[ilength] <= 40)){
      ext_dry <- ext_dry + 1
      drought_type[ilength] <- - 3
    } else {}
  }
  
  drought_number <- rbind.data.frame(ext_wet, very_wet, wet, normal, dry, very_dry,
                             ext_dry)
  colnames(drought_number) <- c("Rain gauge")
  row.names(drought_number) <- c("Extreme Wet", "Very Wet", "Wet", "Normal", "Dry",
                         "Very Dry", "Extreme Dry")
  
  # Calculate the length of drought
  length_drought <- numeric()
  
  n <- 0
  p <- 0
  
  for (ilength in 1:length(pni)) {
    if (is.na(pni[ilength])){
      duree[ilength] <- NA
    } else if (pni[ilength] > 100) {
      n <- 0
      p <- p + 1
      duree[ilength] <- p
    } else {
      p <- 0
      n <- n - 1 
      duree[ilength] <- n
    }
  }
  
  length_zoo <- zoo(as.numeric(duree), index(monthly_data))
  
  respni <- list(pni = pni, drougth_length = length_zoo,
                 drought_number_type = drought_number, type_time = drought_type)
  return(Respni)
}