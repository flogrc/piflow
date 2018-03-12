##____________________________________________________________________________##
##  Function to calculate Rainfall Anomaly Index (RAI)                        ##
##  Pierre L'HERMITE - 2017-10-17 - rai.R                                     ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Calculation of rainfall anomaly index                        ##
##----------------------------------------------------------------------------##
#   Arguments: monthly_data [zoo] : rainfall monthly data in zoo class 
#                                   with date in %Y-%m-%d
#              time_step [numeric] : default = 12, time step to sum monthly 
#                                   precipitation (1, 3, 6, 9, 12, 24 and 48)
##----------------------------------------------------------------------------##
#   Values: resrai [list] : list with 3 zoo et 1 dataframe
#                           (rai, length_zoo, drought_type, drought_number)
#           rai [zoo] : zoo with the rai values with date in %Y-%m-%d
#           length_zoo [zoo] : zoo with the length of drought with date
#                              in %Y-%m-%d
#           drought_type [zoo] : zoo with the type of the period for
#                                 each month 
#           drought_number [dataframe] : dataframe with the number of 
#                           different period by type
#                           Extwet [rai>2], Verywet [1.99>rai>1.5],
#                           wet [1.49>rai>1], Normal [0.99>rai>-0.99],
#                           Dry [-1>rai>-1.49], VeryDry [-1.5>rai>-1.99],
#                           ExtDry [-2>rai])
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

rai <- function(monthly_data, time_step = 12){
  
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
    tmp_mean[, i] <- coredata(monthly_data)[i:(length(monthly_data)+i-1)]
  }
  
  sum <- apply(tmp_mean, 1, sum, na.rm = FALSE)
  sum_zoo <- zoo(sum, order.by = index(monthly_data))
  
  # Average of precipitation, 10 maximum sum and 10 minimum sum of
  # precipitation (na.last put NA at the beginnig or the ending of vector) 
  avg_sum <- mean(sum_zoo, na.rm = TRUE)
  avg_ext_high <- mean(sort(coredata(sum_zoo),na.last = FALSE)
                [(length(sum_zoo)-9):length(sum_zoo)], na.rm = TRUE)
  avg_ext_low <- mean(sort(coredata(sum_zoo),
                   na.last = TRUE)[1:10], na.rm = TRUE)
  
  # Calculation of rainfall anomaly index
  rai <- rep(NA, length(sum_zoo))
  dif <- sum_zoo - avg_sum
  for (i in c(1:length(dif))){
    if (is.na(coredata(dif)[i])) {
      rai[i] <- NA
    } else if(coredata(dif)[i] <= 0){
      rai[i] <- (coredata(dif)[i]*-3)/(avg_ext_low-avg_sum)
    } else {
      rai[i] <- (coredata(dif)[i]*3)/(avg_ext_high-avg_sum)
    }
  }
  rai <- zoo(rai, order.by = index(sum_zoo))
  
  ##__Index analysis________________________________________________________####
  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <- 0
  drought_type <- rep(NA, length(rai))
  
  for (i in 1:length(coredata(rai))) {
    if (is.na(coredata(rai)[i])) {
    } else if ((coredata(rai)[i] >= 3)) {
      ext_wet <- ext_wet + 1
      drought_type[i] <- 3
    } else if ((2.99 > coredata(rai)[i]) && (coredata(rai)[i] > 2)) {
      very_wet <- very_wet + 1
      drought_type[i] <- 2
    } else if ((1.99 > coredata(rai)[i]) && (coredata(rai)[i] > 1)) {
      wet <- wet + 1
      drought_type[i] <- 1
    } else if ((0.99 > coredata(rai)[i]) && (coredata(rai)[i] > -0.99)) {
      normal <- normal+1
      drought_type[i] <- 0
    } else if ((-1 >= coredata(rai)[i]) && (coredata(rai)[i] > -1.99)) {
      dry <- dry + 1
      drought_type[i] <- - 1
    } else if ((-2 >= coredata(rai)[i]) && (coredata(rai)[i] > -2.99)) {
      very_dry <- very_dry + 1
      drought_type[i] <- - 2
    } else if ((coredata(rai)[i] <= -3)) {
      ext_dry <- ext_dry + 1
      drought_type[i] <- - 3
    } else {}
  }
  
  drought_number <- rbind.data.frame(ext_wet, very_wet, wet, normal, dry,
                             very_dry, ext_dry)
  colnames(drought_number) <- c("Pluvio")
  row.names(drought_number) <- c("Extreme Wet", "Very Wet", "Wet", "Normal",
                                 "Dry", "Very Dry", "Extreme Dry")
  
  # Calculation of the drought length
  length_drought <- numeric()
  
  n <- 0
  p <- 0
  for (ilength in 1:length(rai)) {
    if (is.na(rai[ilength])){
      length_drought[ilength] <- NA
    } else if (rai[ilength] > 0) {
      n <- 0
      p <- p + 1
      length_drought[ilength] <- p
    } else {
      p <- 0
      n <- n - 1 
      length_drought[ilength] <- n
    }
  }
  
  length_zoo <- zoo(as.numeric(length_drought), index(rai))
  
  resrai <- list(rai = rai, drougth_length = length_zoo,
                 drought_number_type = drought_number, type_time = drought_type)
}