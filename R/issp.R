#'Calculate ISSP index of Pita
#'
#'Calculate the index about rainfall anomaly by month with the
#'length, the drought type and the intensity
#'
#'@param monthly_data [zoo] rainfall monthly data in zoo class
#'with date in \%Y-\%m-\%d
#'
#'@return \emph{resissp} [list] : list that contains
#'\itemize{
#'\item \emph{$issp} [zoo] : zoo with the issp values with date in %Y-\%m-\%d
#'\item \emph{$length_zoo} [zoo] : zoo with the length of drought with date in
#'\%Y-\%m-\%d [day]
#'\item \emph{$drought_type} [zoo] : zoo with the type of the period for each month
#'\item \emph{$drought_number} [data.frame] : dataframe with the number of different
#'period by type:
#'\itemize{
#'\item Extwet (issp > 2)\cr
#'\item Verywet (1.99 > issp > 1.5)\cr
#'\item Wet (1.49 > issp > 1)\cr
#'\item Normal (0.99 > issp > -0.99)\cr
#'\item Dry (-1 > issp > -1.49)\cr
#'\item VeryDry (-1.5 > issp > -1.99)\cr
#'\item ExtDry (-2 > issp))}
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'## Data preparation
#'load("data/Prec_data.Rdata")
#'prec <- zoo(PluvioData$TabCompleteP, PluvioData$TabDatesR)
#'
#'## Index
#'result <- issp(prec)
#'
#'## Plot index
#'plot_trend(result$issp, trend = TRUE, data_kind = "Precipitation",
#'name = PluvioData$PluvioName, axis_name_x = "Date",
#'axis_name_y = Monthly precipitation [mm/month], midvalue = 0)
#'
#'@references
#'Pita, M.F. (2000). Un nouvel indice de sécheresse pour les domaines
#'méditerranéens. Application au bassin de Guadalquivir (sud-ouest de l'Espagne).
#'\emph{Plubications de l'Association Internationale de Climatologie, 13}, 225-234.
#'\url{https://idus.us.es/xmlui/bitstream/handle/11441/32523/nice_2000_actes.pdf?sequence=1}
#'
#'@seealso
#'\code{\link[piflowtest]{plot_trend}}: plot the index

issp <- function(monthly_data) {

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
  median_calc <- aggregate(monthly_data, by = months, FUN = median, na.rm = TRUE)
  diff <- monthly_data - coredata(median_calc)[as.numeric(months)]

  # Sum of differences
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

  # Issp calculation
  vect_index <- numeric()

  mapa <- mean(res, na.rm = T)
  ecapa <- sd(res, na.rm = T)

  vect_index <- ((res-mapa) / ecapa)

  issp <- zoo(as.numeric(vect_index), index(monthly_data))

  ##__Index analysis________________________________________________________####
  # Calculate the length of drought
  length_drought <- numeric()

  n <- 0
  p <- 0

  for (i in 1:length(issp)){
    if (is.na(issp[i])){
      length_drought[i] <- NA
    } else if (issp[i] > 0){
      n <- 0
      p <- p + 1
      length_drought[i] <- p
    } else{
      p <- 0
      n <- n - 1
      length_drought[i] <- n
    }
  }

  length_zoo <- zoo(as.numeric(length_drought), index(monthly_data))

  # Drought type and number of drought
  ext_wet <- very_wet <- wet <- normal <- dry <- very_dry <- ext_dry <-0

  drought_type <- rep(NA, length(issp))

  for(i in 1:length(issp)){
    if( is.na(issp[i])){
    }
    else if((issp[i] >= 2)){
      ext_wet <- ext_wet + 1
      drought_type[i] <- 3
    } else if((1.99 > issp[i]) && (issp[i]> 1.5)){
      very_wet <- very_wet + 1
      drought_type[i] <- 2
    } else if((1.49 > issp[i]) && (issp[i]> 1)){
      wet <- wet + 1
      drought_type[i] <- 1
    } else if((0.99 > issp[i]) && (issp[i]> -0.99)){
      normal <- normal + 1
      drought_type[i] <- 0
    } else if((-1 >= issp[i]) && (issp[i]> -1.49)){
      dry <- dry + 1
      drought_type[i] <- - 1
    } else if((-1.5 >= issp[i]) && (issp[i]> -1.99)){
      very_dry <- very_dry + 1
      drought_type[i] <- - 2
    } else if((issp[i] <= -2)){
      ext_dry <- ext_dry + 1
      drought_type[i] <- - 3
    } else {}
  }

  drought_number <- rbind.data.frame(ext_wet, very_wet, wet, normal,
                                     dry, very_dry, ext_dry)
  colnames(drought_number) <- c("Rain gauge")
  row.names(drought_number) <- c("ExWet", "VWet", "Wet", "Normal", "Dry",
                                 "VDry", "ExDry")

  resissp <- list(issp = issp, drought_length = length_zoo,
                  drought_number_type = drought_number, type_time = drought_type)
  return(resissp)

}
