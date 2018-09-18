
#' function to retry an iteration of a for loop if there is an error.
#' @param expr is the expression to retry
#' @param maxErrors is the number of retrys that will occur before moving on to the next iteration
#' @param sleep is the lag time between retrys
#' @examples
#'
#' x <- c(1,2,'z',4,5,6,7)
#'
#' for(i in 1:length(x)){
#' retry(expr = {print(runif(x[i]))}, maxErrors = 3, sleep = 1)
#' }
#'
#'
#' @export
retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts > maxErrors) {
      print('bad luck, but were going to the next iteration')
      return(i = i + 1)
    } else {
      print(paste0('attempt',attempts))
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr),silent = TRUE)
  }
  return(retval)
}
