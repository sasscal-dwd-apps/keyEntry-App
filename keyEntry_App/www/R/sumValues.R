##########################################################################
#' @export sumValues
#'
#' @title sumValues 
#'
#' @description calculate the sumValues for each column
#'  
#'@param x data.frame, table with observation elements
#'
#'@param row.days numeric vector,  row names of the table
#'
#' @param rownameSum character string, default value 'SUM' indicates the row where the calculated is displayed 
#'
#'@return x data.frame, updated data.frame including max
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_hourly.R, createForm_daily.R }}.
#'
#' 
##########################################################################


sumValues <- function(x, row.days, rownameSum = 'SUM'){
  for(i in 1:ncol(x)){
    id <- which(rownames(x) == rownameSum)
    if (class(x[[i]]) == "character"){
      x[[i]][id] <- NA
    }else{
      values <- x[[i]][row.days]
      # Check if the values are text or not
      if (class(values) == "character"){
        x[[i]][id] <- "NA"
      }else if (length(which(is.na(values) == T)) == length(values)){
        x[[i]][id] <- sum(values,na.rm = F)
      }else{
        x[[i]][id] <- sum(values,na.rm = T)
      }
      x[[i]][id]
    }
  }
  return(x)
}