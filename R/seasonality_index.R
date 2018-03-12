##____________________________________________________________________________##
##  Function to know when is the 3 parts of low flow                          ##
##  Pierre L'HERMITE - 2017-07-20 - seasonality_index.R                       ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Calcul le jour  donnees avec le filtre passe bas d'Hanning 
#              d'ordre 2
##----------------------------------------------------------------------------##
#   Arguments: data [zoo] : vecteur contenant les donnees journalieres,
#                            mensuelles et saisonnieres, 
#                            avec la date au format %Y-%m-%d
#               debYear [character] : "%m-%d" mois à partir duquel le calcul 
#                                     commence
#               Pourc_debut [numeric] : Pourcentage du debut de l'etiage 
#                                       (defaut 10%)
#               Pourc_milieu [numeric] : Pourcentage du milieu de l'etiage 
#                                       (defaut 50%)
#               Pourc_fin [numeric] : Pourcentage de la fin de l'etiage 
#                                       (defaut 90%)
##----------------------------------------------------------------------------##
#   Values: zoolist [list] : liste contenant 3 zoo indiquant le premier jour
#                                ou le pourcentage est atteint
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------

seasonality_index <- function(data, debYear = "01-01", Pourc_debut = 0.1,
                              Pourc_milieu = 0.5, Pourc_fin = 0.9)
{
  source(file.path("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/Functions/Code_deja_pret/fc.CalcStat.R"))
  
  #na.index permet d'eliminer les NA dans le zoo de donnee
  na.index <- which(is.na(data))
  if(length(na.index) != 0){
    data <- data[-na.index]
  } else {data}
  
  #creation vecteur
  d1 <- numeric()
  d2 <- numeric()
  d3 <- numeric()
  Deb <- numeric()
  Mil <- numeric()
  Fin <- numeric()
  cumul <- numeric()
  tempo <- c("au debut", "au milieu", "a la fin")
  k <- 0 
  
  #Obtention du debit seuil Q50 sur les donnees journalieres
  res <- fc.CalcStat(data, debYear=debYear)
  Q50 <- as.numeric(res$DC[4])
  
  #extraire annee
  ystart <- as.numeric(format(start(data), format="%Y"))
  yend <- as.numeric(format(end(data), format="%Y"))
  
  #Extraction par annee
  for (k in (ystart:yend)) {
    QJ <- extract(data, trgt = k)
    
    #Difference entre le debit journalier et le debit seuil et remplacement des valeurs positives par 0
    Diff <- QJ - Q50
    for (i in 1:length(QJ)) {
      if(Diff[i] > 0){
        Diff[i] <- 0
      }
    }
    
    #cumul des anomalies afin d'avoir le max de deficit
    cumul <- numeric()
    cumul[1] <- Diff[1]
    for (i in (2:length(Diff))) {
      cumul[i] <- cumul[i-1] + Diff[i]
    }
    
    #Calcul du debit du debut/mid/fin d'etiage
    Debut <- Pourc_debut*min(cumul, na.rm = TRUE)
    Mid <- Pourc_milieu*min(cumul, na.rm = TRUE)
    End <- Pourc_fin*min(cumul, na.rm = TRUE)
    
    #Calcul de la position dans l'annee du debut/mid/fin d'etiage
    for (i in 1:length(cumul)) {
      d1[i] <- cumul[i] - Debut
      d2[i] <- cumul[i] - Mid
      d3[i] <- cumul[i] - End
    }
    
    Deb[k] <- which.min(abs(d1))
    Mil[k] <- which.min(abs(d2))
    Fin[k] <- which.min(abs(d3))
  }
  
  #Tableau recapitulatif du debut, milieu et fin de la periode de deficit
  toto <- data.frame(c(ystart:yend), Deb[ystart:yend], Mil[ystart:yend], Fin[ystart:yend])
  colnames(toto) <- c("Dates","Deb","Mil","Fin")
  
  #creation de zoo pour les tendances
  datesim <- seq(as.Date(paste0(ystart,"-01-01")),as.Date(paste0(yend,"-01-01")), by="year")
  
  Debz <- zoo(as.numeric(Deb[ystart:yend]),order.by=datesim)
  Milz <- zoo(as.numeric(Mil[ystart:yend]),order.by=datesim)
  Finz <- zoo(as.numeric(Fin[ystart:yend]),order.by=datesim)
  
  zoolist <- list(Debz, Milz, Finz)
  
  return(zoolist)
}