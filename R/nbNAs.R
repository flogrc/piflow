##____________________________________________________________________________##
##  Function which ...                                                        ##
##  Author(s) name - AAAAMMDD - functionName                                  ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: 
##----------------------------------------------------------------------------##
#   Arguments: varIn1 [class]: 
#              varIn2 [class]: 
#              ...
##----------------------------------------------------------------------------##
#   Value: varOut [class]: 
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
nbNAs <- function(tsData, tstp = "years", hydroYear) {
  ##__Check_Input_Arguments_________________________________________________####
  # --- Check the class
  
  
  ##__Section_1_____________________________________________________________####
  switch(tstp,
         "years" = {
           if (exists("hydroYear")) {
             tstpId <- factor(hydroYear, levels = unique(hydroYear))
           } else {
             y <- as.numeric(format(time(tsData), "%Y"))
             tstpId <- factor(y, levels = unique(y))
           }
         },
         "months" = {
           m <- as.numeric(format(time(tsData), "%m"))
           tstpId <- factor(m, levels = unique(m))
         },
         "mPerY" = {
           mpy <- format(time(tsData), "%Y-%m")
           tstpId <- factor(mpy, levels = unique(mpy))
         }, {
           stop("unkown value for choice of tstp: ", tstp, "\n")
         })
  
  ##__Section_2_____________________________________________________________####
  missV <- aggregate(tsData, by = tstpId, FUN = function(x) {
    return(length(which(!is.na(x))))
  })
  
  if (tstp == "mPerY") {
    missV <- zoo(coredata(missV), index(tsData))
  }
  return(missV)
}
