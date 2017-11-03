#########################################################################
#' @export numberOfDays
#'
#' @title numberOfDays
#' 
#' @description numberOfDays is used to calculate an integer for the month length
#'  
#'@param yyyy numeric, year
#'   
#'@param mm numeric, month
#'
#'@return month length as integer
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_daily.R}}.
#'
#' 
##########################################################################


numberOfDays <- function(yyyy, mm) {
  date <- as.Date(paste0(yyyy, "-", mm, "-01"))
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}