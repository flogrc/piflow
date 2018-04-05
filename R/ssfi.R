#'Calculate Standardized StreamFlow Index (SSFI)
#'
#'Calculate SSFI and the drought specifications with the length, the drought 
#'type and the intensity
#'
#'@param  prec_data [zoo] rainfall monthly data in zoo class with date 
#'in \%Y-\%m-\%d
#'@param  time_step [numeric] by default = 12, time step to sum monthly data
#'(1, 3, 6, 9, 12, 24 and 48)
#'@param  distribution [character] distribution of data 
#'(log_Logistic, gamma, grev, genlog, normal)
#'
#'@return list that contains
#'@return \emph{ssfi} [zoo] zoo with the ssfi values with date in \%Y-\%m-\%d
#'@return \emph{drought_type} [zoo] zoo with the type of the period for each
#'month
#'@return \emph{drought_number} [data.frame] dataframe with the number of 
#'different period by type
#'\itemize{
#'\item Extwet (ssfi > 2)\cr
#'\item Verywet (1.99 > ssfi > 1.5)\cr
#'\item Wet (1.49 > ssfi > 1)\cr
#'\item Normal (0.99 > ssfi > -0.99)\cr
#'\item Dry (-1 > ssfi > -1.49)\cr
#'\item VeryDry (-1.5 > ssfi > -1.99)\cr
#'\item ExtDry (-2 > ssfi)
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'How to use function
#'
#'@references
#'Modarres, R., (2007) Streamflow drought time series forecasting. 
#'\emph{Stochastic Environmental Research and Risk Assessment}
#'21: 223–233.
#'\url{https://doi.org/10.1007/s00477-006-0058-1}
#'
#'@seealso
#'\code{\link[piflowtest]{plot_trend}}: plot the index

ssfi <- function(prec_data, Delta=12, Distribution = "gamma"){
  
  ## Verification arguments d'entree
  if (!is.zoo(prec_data)) { stop("prec_data must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(prec_data) != "monthly") {
    stop("prec_data must be a daily serie \n"); return(NULL)
  }
  
  #Premier mois
  firstmonth <- as.numeric(substr(index(prec_data[1]), 6, 7))
  
  # Using SPEI package to calculate ssfi
  res_ssfi <- SPEI::spi(coredata(prec_data[which(!is.na(prec_data))]),
                         scale = time_step, distribution = distribution,
                         na.rm = TRUE)
  ssfi <- zoo(as.numeric(res_ssfi$fitted),
             order.by = index(prec_data[which(!is.na(prec_data))]))
  
  #Comptage du nombre de secheresse
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry<-0
  Sech <- rep(NA, length(ssfi))
  for(iLength in 1:length(coredata(ssfi))){
    if( is.na(coredata(ssfi)[iLength])){
    } else if((coredata(ssfi)[iLength] >= 2)){
      ExWet <- ExWet + 1
      Sech[iLength] <- 3
    } else if((1.99 > coredata(ssfi)[iLength]) && (coredata(ssfi)[iLength] > 1.5)){
      VWet <- VWet + 1
      Sech[iLength] <- 2
    } else if((1.49 > coredata(ssfi)[iLength]) && (coredata(ssfi)[iLength] > 1)){
      Wet <- Wet + 1
      Sech[iLength] <- 1
    } else if((0.99 > coredata(ssfi)[iLength]) && (coredata(ssfi)[iLength] > -0.99)){
      Normal <- Normal + 1
      Sech[iLength] <- 0
    } else if((-1 >= coredata(ssfi)[iLength]) && (coredata(ssfi)[iLength] > -1.49)){
      Dry <- Dry + 1
      Sech[iLength] <- - 1
    } else if((-1.5 >= coredata(ssfi)[iLength]) && (coredata(ssfi)[iLength] > -1.99)){
      VDry <- VDry + 1
      Sech[iLength] <- - 2
    } else {
      ExDry <- ExDry + 1
      Sech[iLength] <- - 3
    } 
  }
  nombre <- as.data.frame(c(ExWet, VWet, Wet, Normal, Dry, VDry, ExDry))
  colnames(nombre)<-c("Pluvio")
  row.names(nombre)<-c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")
  
  #calcul la duree des periodes seches et humides
  duree <- numeric()
  neg <- 0
  pos <- 0
  for (iLength in 1:length(ssfi)) {
    if (is.na(ssfi[iLength])){
      duree[iLength] <- NA
    } else if (ssfi[iLength] > 0) {
      neg <- 0
      pos <- pos + 1
      duree[iLength] <- pos
    } else {
      pos <- 0
      neg <- neg - 1 
      duree[iLength] <- neg
    }
  }
  
  dureezoo<-zoo(as.numeric(duree), index(ssfi))
  
  Resultat<-list(ssfi = ssfi, Drought = nombre, Time = dureezoo, SechTime = Sech)
  
  return(Resultat)
}