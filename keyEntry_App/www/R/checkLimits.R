##########################################################################
#' @export checkLimits
#'
#' @title checkLimits as quality controll
#'
#' @description check limits as quality control while type into the web form
#' 
#'@param varName character, name of the selected variable
#'
#'@param minValue numeric, minimum value of the column 
#'
#'@param maxValue numeric,  maximum value of the column   
#'
#'@param row.days numeric vector, row names in a vector  

#'@return character, with information if values are within or outside the limits 
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_hourly, createForm_daily}}.


checkLimits <- function(varName, minValue, maxValue, row.days){
  checkLimitsPrev <- sapply(1:length(varName), function(i){
    # Check if there are NA in the min & max values
    min <- minValue[i]
    max <- maxValue[i]
    rowCondition <- paste0("(",paste0("row==", c(1:length(row.days))-1, collapse = " || "),")")
    if (!is.na(min) && !is.na(max)){
      text <- paste0("(col==", i-1," & ", rowCondition," & (value<", min," || value>", max, "))")
    }else if (is.na(min) && is.na(max)){
      return()
    }else if (is.na(min)){
      text <- paste0("(col==", i-1," & ", rowCondition," & value>", max, ")")
    }else if (is.na(max)){
      text <- paste0("(col==", i-1," & ", rowCondition," & value<", min,")")
    }
    text
  })
  checkLimitsCondition <<- paste(unlist(checkLimitsPrev), collapse = " || ")
  return(checkLimitsCondition)
}