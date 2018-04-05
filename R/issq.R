#'Calculate ISSQ index of Pita for discarge
#'
#'Calculate ISSQ with discharge anomaly with quantile 50 by month with the
#'length, the drought type and the intensity
#'
#'@param  monthly_data [zoo] discharge monthly data in zoo class with date 
#'in \%Y-\%m-\%d
#'
#'@return List that contains
#'\itemize{
#'\item \emph{$issq} [zoo] zoo with the issq values with date in \%Y-\%m-\%d
#'\item \emph{$length_zoo} [zoo] zoo with the length of drought with date in
#'\%Y-\%m-\%d [day]
#'\item \emph{$drought_type} [zoo] zoo with the type of the period for each month
#'\item \emph{$drought_number} [data.frame] dataframe with the number of different
#'period by type:
#'\itemize{
#'\item Extwet (issq > 2)\cr
#'\item Verywet (1.99 > issq > 1.5)\cr
#'\item Wet (1.49 > issq > 1)\cr
#'\item Normal (0.99 > issq > -0.99)\cr
#'\item Dry (-1 > issq > -1.49)\cr
#'\item VeryDry (-1.5 > issq > -1.99)\cr
#'\item ExtDry (-2 > issq)}
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'## Loading data
#'data("dailyPrec_bv1")
#'
#'## Index
#'result <- issq(dailyPrec)
#'
#'## Plot index
#'plot_trend(result$issq, trend = TRUE, data_kind = "Flow",
#'name = PluvioData$PluvioName, axis_name_x = "Date",
#'axis_name_y = Monthly flow [mm/month], midvalue = 0)
#'
#'@references
#'
#'@seealso
#'\code{\link[piflowtest]{plot_trend}} plot the index

issq <- function(monthly_data) {
  
  ##__Checking______________________________________________________________####
  # Data input checking
  if (!is.zoo(monthly_data)) { stop("monthly_data must be a zoo"); return(NULL)}
  
  # Time step checking
  if (periodicity(monthly_data)$scale != "monthly") {
    stop("monthly_data must be a monthly serie \n"); return(NULL)
  }
  
  ##__Index calculation_____________________________________________________####
  # Anomaly between precipitation and monthly median
  
  months <- substr(index(monthly_data), 6, 7)
  decile <- aggregate(monthly_data, by = months,
                       FUN = quantile, probs = seq(0, 1, 0.1), na.rm = TRUE)
  diff <- monthly_data - coredata(decile[, 6])[as.numeric(months)]
  
  #Sum of differences
  res <- rep(NA, length(diff))
  som <- 0
  
  for (i in 1:length(coredata(diff))){
    if(is.na(coredata(diff)[i])){
      res[i] <- coredata(diff)[i]
    }
    else if(coredata(diff)[i] > 0){
      som <- som + coredata(diff)[i]
      res[i] <- som
    }
    else if (som >= 0 || is.na(som)){
      som <- 0
      som <- som + coredata(diff)[i]
      res[i] <- som
    }
    else {
      som <- som + coredata(diff)[i]
      res[i] <- som
    }
  }  
  
  #issq calcultation
  vect_index <- numeric()
  
  mapa <- mean(res, na.rm = T)  
  ecapa <- sd(res, na.rm = T)
  
  vect_index <- ((res-mapa) / ecapa)
  
  issq <- zoo(as.numeric(vect_index), index(monthly_data))
  
  ##__Index analysis________________________________________________________####
  # Calculate the length of drought
  length_drought <- numeric()
  n <- 0
  p <- 0
  for (i in 1:length(issq)){
    if (is.na(issq[i])){
      length_drought[i] <- NA
    } else if (issq[i] > 0){
      n <- 0
      p <- p + 1
      length_drought[i] <- p
    } else{
      p <- 0
      n <- n - 1 
      length_drought[i] <- n
    }
  }
  
  lengthzoo <- zoo(as.numeric(length_drought), index(monthly_data))
  
  #Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <-0
  drought_type <- rep(NA, length(issq))
  for(i in 1:length(issq)){
    if( is.na(issq[i])){
    } 
    else if((issq[i] >= 2)){
      ext_wet <- ext_wet + 1
      drought_type[i] <- 3
    } else if((1.99 > issq[i]) && (issq[i]> 1.5)){
      very_wet <- very_wet + 1
      drought_type[i] <- 2
    } else if((1.49 > issq[i]) && (issq[i]> 1)){
      wet <- wet + 1
      drought_type[i] <- 1
    } else if((0.99 > issq[i]) && (issq[i]> -0.99)){
      normal <- normal + 1
      drought_type[i] <- 0
    } else if((-1 >= issq[i]) && (issq[i]> -1.49)){
      dry <- dry + 1
      drought_type[i] <- - 1
    } else if((-1.5 >= issq[i]) && (issq[i]> -1.99)){
      very_dry <- very_dry + 1
      drought_type[i] <- - 2
    } else if((issq[i] <= -2)){
      ext_dry <- ext_dry + 1
      drought_type[i] <- - 3
    } else {}
  }
  
  drought_number <- rbind.data.frame(ext_wet, very_wet, wet, normal,
                                     Dry, very_dry, ext_dry)
  colnames(drought_number) <- c("Pluvio")
  row.names(drought_number) <- c("ExWet", "VWet", "Wet", "Normal",
                                 "Dry", "VDry", "ExDry")
  
  resissq <- list(issq = issq, drought_length = lengthzoo,
                  drought_number_type = drought_number,
                  type_time = drought_type)
  return(resissq)
  
}