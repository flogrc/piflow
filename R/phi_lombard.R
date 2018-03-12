#score : 1 = rupture en moyenne , 2 = rupture en variance (Mood), 3 = rupture en variance (Logarithmique)
#length : longueur de l'echantillon
# Rang : rang obtenu par order[order]

phi_lombard <- function(score, long , Rang)
{
  if (score == 1){
    phi <- 2*(Rang/(long+1))-1
  } else if (score == 2){
    phi <- ((2*(Rang/(long+1))-1)^2)
  } else if (score == 3){
    phi <- log(1-(Rang/(long+1)))
  }
  return(phi)
}
