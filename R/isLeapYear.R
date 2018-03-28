#'Test if a year is a leap year
#'
#'Return TRUE if a year is a leap year
#'
#'@param  year [numeric] the value of the year(s) to be tested
#'
#'@return \emph{\code{leapYear}} [boolean] : the logical vector indicating if
#'the year(s) is(are) a leap year(s)
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'isLeapYear(2017)
#'isLeapYear(c(2004, 2012, 2015))

isLeapYear <- function(year) {
  ##__Check_arguments_______________________________________________________####
  if (!is.numeric(year)) { stop("year must be a numeric"); return(NULL) }
  
  ##__Leap_year_test________________________________________________________####
  leapYear <- (year %% 4 == 0) & ((year %% 100 != 0) |
                                    (year %% 100 == 0 & year %% 400 == 0))
  return(leapYear)
}
