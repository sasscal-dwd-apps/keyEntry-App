#########################################################################
#' @export numberOfDays
#'
#' @title numberOfDays
#' 
#' @description numberOfDays is used to calculate an integer for the month length
#'  
#' @param df1 data.frame, temp dataframe with climate values 
#'   
#' @param formRule data.frame, worksheet from form with rules for calculation. 
#'
#' @param  row.days integer vector, number of days in the selected month
#' 
#' @return numeric, arithmetic mean for the selected variable rule is described in the .xsl sheed
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_daily.R}}.
#'
#' 
##########################################################################

ruleMean <- function(df1, formRule, row.days){
  elements_prev <- colnames(formRule[grep("element", colnames(formRule))])
  elements <- formRule[elements_prev[grep("_name", elements_prev,invert = T)]]
  # Check how many elements are beeing taken into account in the formula
    headers <- as.character(elements[1,])
    result <- sapply(1:length(row.days), function(i){
      mean(as.vector(as.matrix(df1[i,headers])))
    })
  return(result)
}