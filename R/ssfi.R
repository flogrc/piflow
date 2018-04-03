##____________________________________________________________________________##
##  Function to calculate SSFI                                                ##
##  Pierre L'HERMITE - 2017-10-12 - fc.SSFI.R                                 ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Fonction : Calcul l'indice standardise des debits avec les types de       ##
#              secheresse et leur duree                                       ##
##----------------------------------------------------------------------------##
#   Arguments : MonthlyData [zoo] : vecteur contenant les donnees mensuelles
#                                   de debit avec la date au format 
#                                   %Y-%m-%d [mm/month]
#               Delta [numeric] : valeur du pas de temps pour le SSFI (1, 3, 6,
#                                 9, 12, 24, et 48 en general)
#                                 [en nombre de mois]
#               Distribution [character] : distribution des donnees (gamma,
#                                          grev, genlog, normal)
##----------------------------------------------------------------------------##
#   Sortie    : ResSSFI [list] : liste contenant 2 zoo et un data frame
#                               (SSFI , Drought, Time)
#               SSFI [zoo] : vecteur contenant les valeurs du SSFI avec la
#                           date au format %Y-%m-%d
#               Drought [df] : recapitulatif du nombre de secheresse par les
#                             differentes categories de le SSFI (ExtWet [SSFI>2],
#                             VeryWet [1.99>SSFI>1.5], Wet [1.49>SSFI>1], Normal
#                             [0.99>SSFI>-0.99], Dry [-1>SSFI>-1.49], VeryDry
#                             [-1.5>SSFI>-1.99], ExtDry [-2>SSFI])
#               Time [zoo] : vecteur contenant la duree des differentes periodes
#                            avec la date au format %Y-%m-%d. Lorsque l'indice
#                            positif, c'est une periode humide et lorsque
#                            l'indice est negatif, la periode est seche
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

ssfi <- function(MonthlyData, Delta=12, Distribution = "gamma"){
  
  library(hydroTSM)
  ## Verification arguments d'entree
  if (!is.zoo(MonthlyData)) { stop("MonthlyData must be a zoo"); return(NULL) }
  # --- Verification du pas de temps
  if (sfreq(MonthlyData) != "monthly") {
    stop("MonthlyData must be a daily serie \n"); return(NULL)
  }
  
  #Premier mois
  firstmonth <- as.numeric(substr(index(MonthlyData[1]), 6, 7))
  
  # Using SPEI package to calculate spi
  res_ssfi <- SPEI::spi(coredata(MonthlyData[which(!is.na(MonthlyData))]),
                         scale = time_step, distribution = distribution,
                         na.rm = TRUE)
  ssfi <- zoo(as.numeric(res_ssfi$fitted),
             order.by = index(MonthlyData[which(!is.na(MonthlyData))]))
  
  #Comptage du nombre de secheresse
  ExWet <- VWet <- Wet <- Normal <- Dry <- VDry <- ExDry<-0
  Sech <- rep(NA, length(SSFI))
  for(iLength in 1:length(coredata(SSFI))){
    if( is.na(coredata(SSFI)[iLength])){
    } else if((coredata(SSFI)[iLength] >= 2)){
      ExWet <- ExWet + 1
      Sech[iLength] <- 3
    } else if((1.99 > coredata(SSFI)[iLength]) && (coredata(SSFI)[iLength] > 1.5)){
      VWet <- VWet + 1
      Sech[iLength] <- 2
    } else if((1.49 > coredata(SSFI)[iLength]) && (coredata(SSFI)[iLength] > 1)){
      Wet <- Wet + 1
      Sech[iLength] <- 1
    } else if((0.99 > coredata(SSFI)[iLength]) && (coredata(SSFI)[iLength] > -0.99)){
      Normal <- Normal + 1
      Sech[iLength] <- 0
    } else if((-1 >= coredata(SSFI)[iLength]) && (coredata(SSFI)[iLength] > -1.49)){
      Dry <- Dry + 1
      Sech[iLength] <- - 1
    } else if((-1.5 >= coredata(SSFI)[iLength]) && (coredata(SSFI)[iLength] > -1.99)){
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
  for (iLength in 1:length(SSFI)) {
    if (is.na(SSFI[iLength])){
      duree[iLength] <- NA
    } else if (SSFI[iLength] > 0) {
      neg <- 0
      pos <- pos + 1
      duree[iLength] <- pos
    } else {
      pos <- 0
      neg <- neg - 1 
      duree[iLength] <- neg
    }
  }
  
  dureezoo<-zoo(as.numeric(duree), index(SSFI))
  
  Resultat<-list(SSFI = SSFI, Drought = nombre, Time = dureezoo, SechTime = Sech)
  
  return(Resultat)
}