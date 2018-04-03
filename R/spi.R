##____________________________________________________________________________##
##  Function to calculate SPI                                                 ##
##  Pierre L'HERMITE - 2017-10-12 - spi.R                                     ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Calculate the standardized precipitation index and the       ##
#                drought specifications                                       ##
##----------------------------------------------------------------------------##
#   Arguments: prec_data [zoo]: rainfall monthly data in zoo class 
#                               with date in %Y-%m-%d
#              time_step [numeric] : default = 12, time step to sum monthly 
#                                   precipitation (1, 3, 6, 9, 12, 24 and 48)
#              distribution [character] : distribution of data (log_Logistic,
#                                           gamma, grev, genlog, normal)
##----------------------------------------------------------------------------##
#   Values: resspi [list] : list with 3 zoo et 1 dataframe
#                           (spi, length_zoo, drought_type, drought_number)
#           spi [zoo] : zoo with the spi values with date in %Y-%m-%d
#           length_zoo [zoo] : zoo with the length of drought with date
#                              in %Y-%m-%d
#           drought_type [zoo] : zoo with the type of the period for
#                                 each month 
#           drought_number [dataframe] : dataframe with the number of 
#                           different period by type
#                           Extwet [spi>2], Verywet [1.99>spi>1.5],
#                           wet [1.49>spi>1], Normal [0.99>spi>-0.99],
#                           Dry [-1>spi>-1.49], VeryDry [-1.5>spi>-1.99],
#                           ExtDry [-2>spi])
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

spi <- function(prec_data, time_step = 12, distribution = "gamma"){
  
  ##__Checking______________________________________________________________####
  # Data input checking
  if (!is.zoo(prec_data)) { stop("prec_data must be a zoo"); return(NULL)}

  # Time step checking
  if (periodicity(prec_data)$scale != "monthly") {
    stop("prec_data must be a monthly serie \n"); return(NULL)
  }
  
  ##__Calculation___________________________________________________________####
  library(SPEI)
  
  # First month
  firstmonth <- as.numeric(substr(index(prec_data[1]), 6, 7))
  
  # Using SPEI package to calculate spi
  res_spi <- SPEI::spi(coredata(MonthlyData[which(!is.na(MonthlyData))]),
                       scale = time_step, distribution = distribution,
                       na.rm = TRUE)
  spi <- zoo(as.numeric(res_spi$fitted),
              order.by = index(MonthlyData[which(!is.na(MonthlyData))]))
  
  ##__Index analysis________________________________________________________####
  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <- 0
  drought_type <- rep(NA, length(spi))
  
  for (i in 1:length(coredata(spi))) {
    if (is.na(coredata(spi)[i])) {
    } else if ((coredata(spi)[i] >= 3)) {
      ext_wet <- ext_wet + 1
      drought_type[i] <- 3
    } else if ((2.99 > coredata(spi)[i]) && (coredata(spi)[i] > 2)) {
      very_wet <- very_wet + 1
      drought_type[i] <- 2
    } else if ((1.99 > coredata(spi)[i]) && (coredata(spi)[i] > 1)) {
      wet <- wet + 1
      drought_type[i] <- 1
    } else if ((0.99 > coredata(spi)[i]) && (coredata(spi)[i] > -0.99)) {
      normal <- normal+1
      drought_type[i] <- 0
    } else if ((-1 >= coredata(spi)[i]) && (coredata(spi)[i] > -1.99)) {
      dry <- dry + 1
      drought_type[i] <- - 1
    } else if ((-2 >= coredata(spi)[i]) && (coredata(spi)[i] > -2.99)) {
      very_dry <- very_dry + 1
      drought_type[i] <- - 2
    } else if ((coredata(spi)[i] <= -3)) {
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
  for (ilength in 1:length(spi)) {
    if (is.na(spi[ilength])){
      length_drought[ilength] <- NA
    } else if (spi[ilength] > 0) {
      n <- 0
      p <- p + 1
      length_drought[ilength] <- p
    } else {
      p <- 0
      n <- n - 1 
      length_drought[ilength] <- n
    }
  }
  
  length_zoo <- zoo(as.numeric(length_drought), index(spi))
  
  Resultat <- list(spi = spi, drougth_length = length_zoo,
                   drought_number_type = drought_number, type_time = drought_type)
  return(Resultat)
}