#'Test date class
#'
#'Test if a data is a date class
#'
#'@param date [] the class you will test
#'
#'@return
#'\emph{
#'\enumerate{
#'\code{class} the logical vector indicating if the date is in
#'date class (boolean) \cr
#'\item Return TRUE if a date is a date class
#'\item Return FALSE if a date is another class
#'}
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'isDate(2017-01-02)
#'isDate(as.Date("2017-01-02"))

isDate <- function(date)
{
  class <- inherits(date, "Date")
  return(class)
}
