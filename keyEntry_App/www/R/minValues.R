##########################################################################
#' @export minValues
#'
#' @title minValues 
#'
#' @description calculate the minValues for each column
#'  
#'@param x data.frame, table with observation elements
#'
#'@param row.days numeric vector,  row names of the table
#'
#'@return x data.frame, updated data.frame including max
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_hourly.R, createForm_daily.R }}.
#'
#' 
##########################################################################

minValues <- function(x, row.days, rownameMin = 'MIN.'){
  id <- which(rownames(x) == rownameMin)
  for(i in 1:ncol(x)){
    if (class(x[[i]]) == "character"){
      x[[i]][id] <- NA
    }else{
      values <- x[[i]][row.days]
      if (length(which(is.na(values) == T)) == length(values)){
        x[[i]][id] <- min(values,na.rm = F)
      }else{
        x[[i]][id] <- min(values,na.rm = T)
      }
      x[[i]][id]
    }
  }
  return(x)
}