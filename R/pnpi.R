#'Percent of Normal Index (pnpi)
#'
#'Function which run pnpi for rainfall and give some results about different 
#'periods (Dry, Normal and Wet), with the length of period, the type of period
#'and the date and the type of period over the data serie.
#'
#'@param  monthly_data [zoo] rainfall monthly data in zoo class 
#'with date in \%Y-\%m-\%d
#'@param  time_step [numeric] default = 12, time step (1, 3, 6, 9, 12, 24 and 48)
#'
#'@return \emph{respnpi} [list] : that contains
#'\itemize{
#'\item \emph{pnpi} [zoo] : zoo with the pnpi values with date in %Y-\%m-\%d
#'\item \emph{length_zoo} [zoo] : zoo with the length of drought with date in
#'\%Y-\%m-\%d [day]
#'\item \emph{drought_type} [zoo] : zoo with the type of the period for each month
#'\item \emph{drought_number} [data.frame] : dataframe with the number of
#'different period by type:
#'\itemize{
#'\item Extwet (pnpi >= 160)\cr
#'\item Verywet (160 > pnpi >= 145)\cr
#'\item Wet (145 > pnpi >= 120)\cr
#'\item Normal (120 > pnpi >= 80)\cr
#'\item Dry (80 > pnpi >= 55)\cr
#'\item VeryDry (50 > pnpi >= 40)\cr
#'\item ExtDry (40 > pnpi))}
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'## Data preparation
#'data("Prec_data")
#'prec <- zoo(PluvioData$TabCompleteP, PluvioData$TabDatesR)
#'
#'## Index
#'result <- pnpi(prec, time_step = 3)
#'
#'## Plot index
#'plot_trend(result$pnpi, trend = TRUE, data_kind = "Precipitation",
#'name = PluvioData$PluvioName, axis_name_x = "Date",
#'axis_name_y = Monthly precipitation [mm/month], midvalue = 100)
#'
#'@references
#'Willeke G, Hosking JRM, Wallis JR, Guttman NB (1994) The national drought 
#'atlas,\emph{Institute for water resources report 94-NDs-4. Army Corps of 
#'Engineers}, Washington, D.C.\cr
#'Amirataee, B. and Montaseri, M. (2017) The performance of SPI and PNPI in analyzing
#'the spatial and temporal trend of dry and wet periods over Iran, \emph{Natural Hazards},
#'1, 89-106.
#'\url{https://doi.org/10.1007/s11069-016-2675-4}
#'
#'@seealso
#'\code{\link[piflowtest]{plot_trend}}: plot the index

pnpi <- function(monthly_data, time_step = 12){
  
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
  
  # Calculation of pnpi
  pnpi <- (sum_zoo/sum_mean)*100
  
  ##__Index analysis________________________________________________________####
  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <- 0
  
  drought_type <- rep(NA, length(pnpi))
  
  for(ilength in 1:length(pnpi)){
    if( is.na(pnpi[ilength])){
    } else if (pnpi[ilength] >= 160){
      ext_wet <- ext_wet + 1
      drought_type[ilength] <- 3
    } else if ((pnpi[ilength] >= 145 && (pnpi[ilength] < 160))) {
      very_wet <- very_wet + 1
      drought_type[ilength] <- 2
    } else if ((pnpi[ilength] > 120 && (pnpi[ilength] < 145))) {
      wet <- wet + 1
      drought_type[ilength] <- 1
    } else if((120 >= pnpi[ilength]) && (pnpi[ilength] > 80)){
      normal <- normal + 1
      drought_type[ilength] <- 0
    } else if((80 >= pnpi[ilength]) && (pnpi[ilength] > 55)){
      dry <- dry + 1
      drought_type[ilength] <- - 1
    } else if((55 >= pnpi[ilength]) && (pnpi[ilength] > 40)){
      very_dry <- very_dry + 1
      drought_type[ilength] <- - 2
    } else if((pnpi[ilength] <= 40)){
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
  
  for (ilength in 1:length(pnpi)) {
    if (is.na(pnpi[ilength])){
      duree[ilength] <- NA
    } else if (pnpi[ilength] > 100) {
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
  
  respnpi <- list(pnpi = pnpi, drougth_length = length_zoo,
                 drought_number_type = drought_number, type_time = drought_type)
  return(respnpi)
}