#'Filter data with Hanning filter order 2
#'
#'Eliminate variations to data series to observe better the long term variation
#'
#'@param monthly_data [zoo] rainfall daily, monthly, seasonnality or annual data
#'in zoo class with date in \%Y-\%m-\%d
#'
#'@return \emph{\code{hanzoo}} [zoo] vector with data after hanning filter 
#'with date in \%Y-\%m-\%d
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'## Loading data
#'data("dailyPrec_bv1")
#'
#'## Index
#'result <- hanning(dailyPrec)
#'
#'@references
#'A.A. Assani, (1999)
#'Analysis of rainfall variability (1916-1996) at Lubumbashi (Congo-Kinshasa) 
#'relative to some atmospheric (southern oscillation) and oceanic
#'(El Nino / La Nina) circulation indicators
#'Sécheresse, vol. 10, n° 4, p. 245-252.
#'\url{http://www.jle.com/fr/revues/sec/e-docs/analyse_de_la_variabilite_temporelle_des_precipitations_1916_1996_a_lubumbashi_congo_kinshasa_en_relation_avec_certains_indicateurs_de_la_circulation_atmospherique_oscillation_australe_et_oceanique_el_ni_o_la_ni_a__25631/article.phtml}
#'
#'@seealso
#'\code{\link[piflowtest]{plot_trend}} function to plot

hanning <- function(monthly_data){

  ##__Checking______________________________________________________________####
  if (!is.zoo(monthly_data)) {
    stop("monthly_data must be a zoo")
    return(NULL)
  }
  
  ind_run <- which(!is.na(coredata(monthly_data)))
  
  ##__Calculation___________________________________________________________####
  han <- rep(NA, length(monthly_data))
  
  han[ind_run[1]] <- ((0.54*coredata(monthly_data[ind_run[1]]))
                      + (0.46*coredata(monthly_data[ind_run[2]])))
  han[ind_run[2]] <- ((0.25*coredata(monthly_data[ind_run[1]]))
                      + (0.5*coredata(monthly_data[ind_run[2]]))
                      + (0.25*coredata(monthly_data[ind_run[3]])))
  
  for (i in c(3:(length(ind_run)-2))){
    han[ind_run[i]] <- ((0.06*coredata(monthly_data[ind_run[i-2]]))
                        + (0.25*coredata(monthly_data[ind_run[i-1]]))
                        + (0.38*coredata(monthly_data[ind_run[i]]))
                        + (0.25*coredata(monthly_data[ind_run[i+1]]))
                        + (0.06*coredata(monthly_data[ind_run[i+2]])))
  }
  
  han[ind_run[length(ind_run)]-1] <- ((0.25*coredata(monthly_data[ind_run[length(ind_run)]-2]))
                                      + (0.5*coredata(monthly_data[ind_run[length(ind_run)]-1])) 
                                      + (0.25*coredata(monthly_data[ind_run[length(ind_run)]])))
  han[ind_run[length(ind_run)]] <- ((0.54*coredata(monthly_data[ind_run[length(ind_run)]]))
                                    +(0.46*coredata(monthly_data[ind_run[length(ind_run)]-1])))
  
  hanzoo <- zoo(as.numeric(han), index(monthly_data))
  return(hanzoo)
}