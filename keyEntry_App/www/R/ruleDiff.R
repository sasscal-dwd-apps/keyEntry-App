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
#' @return numeric, value is calculated as difference between two values from .xsl form
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_daily.R}}.
#'
#' 
##########################################################################

ruleDiff <- function(df1, formRule, row.days){
  elements_prev <- colnames(formRule[grep("element", colnames(formRule))])
  elements <- formRule[elements_prev[grep("_name", elements_prev,invert = T)]]
  # Check how many elements are beeing taken into account in the formula
  headers <- as.character(elements[1,])
  if (length(headers)==1){
    result <- sapply(1:length(row.days), function(i){
      if(i == length(row.days)){
        NA
      }else{
        value1 <- df1[i, headers]
        value2 <- df1[i+1, headers]
        value2-value1
      }
    })
    return(unlist(result))
  }else if(length(headers)==2){
    result <- sapply(1:length(row.days), function(i){
      diff(as.vector(as.matrix(df1[i,headers])))
    })
  }else{
    return(NA)
  }
  return(result)
}