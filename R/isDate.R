#'Test date class
#'
#'Test if a data is a date class
#'
#'@param test_class [] the class you will test
#'
#'@return
#'\enumerate{
#'\emph{\code{class}} the logical vector indicating if the test_class is in
#'date class (boolean) \cr
#'\item Return \emph{\code{TRUE}} if a test_class is a date class
#'\item Return \emph{\code{FALSE}} if a test_class is another class
#'}
#'
#'@author Florine Garcia (florine.garcia@gmail.com)
#'@author Pierre L'Hermite (pierrelhermite@yahoo.fr)
#'
#'@examples
#'isDate(2017-01-02)
#'isDate(as.Date("2017-01-02"))

isDate <- function(test_class)
{
  class <- inherits(test_class, "Date")
  return(class)
}
