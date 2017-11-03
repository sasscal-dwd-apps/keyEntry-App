##########################################################################
#' @export meanValues
#'
#' @title meanValues 
#'
#' @description calculate the meanValues for each column
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


meanValues <- function(x, row.days, rownameMean = 'MEAN'){
  for(i in 1:ncol(x)){
    id <- which(rownames(x) == rownameMean)
    if (class(x[[i]]) == "character"){
      x[[i]][id] <- NA
    }else{
      values <- x[[i]][row.days]
      if (length(which(is.na(values) == T)) == length(values)){
        x[[i]][id] <- mean(values,na.rm = F)
      }else{
        x[[i]][id] <- round(mean(values, na.rm = T), 1)
      }
      x[[i]][id]
    }
  }
  return(x)
}