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
#' @return numeric, value is calculated as between two values from .xsl form
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_daily.R}}.
#'
#' 
##########################################################################

ruleBtwn <- function(df1, formRule, row.days){
  elements_prev <- colnames(formRule[grep("element", colnames(formRule))])
  elements <- formRule[elements_prev[grep("_name", elements_prev,invert = T)]]
  # Check how many elements are beeing taken into account in the formula
  headers <- as.character(elements[1,])
  # Find the table
  id <- which(colnames(formRule)=="between_table")
  df.table <- formRule[c(2:nrow(formRule)), c(id:ncol(formRule))]
  colnames(df.table) <- formRule[1,c(id:ncol(formRule))]
  result <- sapply(1:length(row.days), function(i){
    id <- findInterval(df1[i, headers], as.numeric(df.table$min))
    if (!is.na(id) && id == 0){
      -999
    }else{
      df.table[id,3]
    }
  })
  return(as.numeric(unlist(result)))
}