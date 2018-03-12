testLombard <- function(data, val_score, date) #val_score=1,2,3
{
  
  Ind_run <- which(!is.na(data))
  data <- data[Ind_run]
  
  indice<-order(data)
  rang<-order(indice) #rangs du vecteur
  
  #fonction score i = 1 : moyenne, i = 2 : variance : (r'v'lood), i = 3 : vorionce
  phi<-fc.phi_Lombard(val_score, length(data), rang)
  phim<-mean(phi)
  phisd<-sd(phi)
  
  S<-(phi-phim)/phisd
  
  #Somme sur les valeurs de S
  somme<-cumsum(S) 
  
  #creation et remplissage matrice L
  L<-matrix(NA, nrow = length(data), ncol=length(data))
  for (i in c(1:((length(data)-1)))){
    for (j in c((i+1) : length(data))){
      L[i,j]<-sum(somme[i:j])
    }
  }
  #Mettre les elements au carre
  Lsq<-L*L
  
  Vn<-0
  for (i in c(1:(length(data)-1))){
    for (j in c((i+1) : length(data))){
      Vn<-Vn+Lsq[i,j]
    }
  }
  
  #Division pour utiliser la valeur critique de l'article de Lombard
  Tn<-Vn/(length(data)^5)
  
  ## Determination t1 et t2
  #Creation et remplissage de la matrice M
  M<-matrix(NA, nrow = length(data), ncol=length(data))
  for (i in c(1:(length(data)-1))){
    for (j in c((i+1) : length(data))){
      u<-i/length(data)
      v<-j/length(data)
      sig<-((((1-u)^3*(1+3*u))/12)-((1-v)^3*(1+3*v))/12)-((((1-v)^2)*(v^2-u^2))/2)
      M[i,j]<-abs(L[i,j])/sqrt(sig)
    }
  }
  
  if (Tn > 0.0403){
    NApos <- which(is.na(M))
    Mbis <- M[NApos]<-0
    for (i in 1:length(data)){
      maximum[i] <- max(M[,i], na.rm = TRUE)
    }
    Res <- max(maximum)
    coo <- which(M == Res, arr.ind=TRUE)
    t1 <- as.numeric(coo[1,1])
    t2 <- as.numeric(coo[1,2])
  } else {
    t1 <- 0
    t2 <- 0
  }
  
  if (t1 == 0){
    an1 <- NA
    an2 <- NA
  } else {
    an1 <- date[t1]
    an2 <- date[t2]
  }
}