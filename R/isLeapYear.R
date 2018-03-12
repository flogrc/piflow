##____________________________________________________________________________##
##  Function which test if a year is a leap year                              ##
##  Florine Garcia - 20180301 - isLeapYear                                    ##
##____________________________________________________________________________##
##----------------------------------------------------------------------------##
#   Description: Return TRUE if a year is a leap year
##----------------------------------------------------------------------------##
#   Arguments: year [numeric]: the value of the year(s) to be tested
##----------------------------------------------------------------------------##
#   Value: leapYear [boolean]: the logical vector indicating if the year(s)
#            is(are) a leap year(s)
##----------------------------------------------------------------------------##
#-------------------------------------------------------------------------------
isLeapYear <- function(year) {
  ##__Check_arguments_______________________________________________________####
  if (!is.numeric(year)) { stop("year must be a numeric"); return(NULL) }
  
  ##__Leap_year_test________________________________________________________####
  leapYear <- (year %% 4 == 0) & ((year %% 100 != 0) |
                                    (year %% 100 == 0 & year %% 400 == 0))
  return(leapYear)
}
