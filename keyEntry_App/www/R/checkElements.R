##########################################################################
#' @export checkElements
#'
#' @title checkElements for quality control
#'
#' @description check elements while entered if values are outside the limits the cell 
#'              colored red
#'
#'@param climsoftCode numeric, element code for variable
#'
#'@param row.days numeric vector, row names in a vector  

#'@return integer, empty for red color in rhandsontable or not emtpy 
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{createForm_hourly, createForm_daily}}.
##############################################################################

checkElements <- function(climsoftCode, row.days){
  
  ###########################################################################
  # First check (Tmax-Tmin > 0)
  # TMax & TMin
  maxCode <- 2
  minCode <- 3
  maxId <- which(climsoftCode == maxCode)
  minId <- which(climsoftCode == minCode)
  if (length(maxId) == 0){
    checkElementsCondition <- maxId
  }else if(length(minId) == 0){
    checkElementsCondition <- minId
  }else{
  checkElementsPrev <- sapply(1:length(row.days), function(i){
    text1 <- paste0("(row == ", i-1, " & col == ", maxId-1, 
                    " & instance.getDataAtCell(",i-1, ", ", maxId-1, 
                    ") < instance.getDataAtCell(",i-1,", ", minId-1, "))")
    text2 <- paste0("(row == ", i-1, " & col == ", minId-1,
                    " & instance.getDataAtCell(",i-1, ", ", minId-1, 
                    ") > instance.getDataAtCell(",i-1,", ", maxId-1, "))")
    c(text1, text2)
  })
  checkElementsCondition <<- paste(unlist(checkElementsPrev), collapse = " || ")
  }
  return(checkElementsCondition)
}