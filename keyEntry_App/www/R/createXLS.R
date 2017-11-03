##########################################################################
#' @export createXLS
#'
#' @title create XLS table
#'
#' @description Create an xls workbook output including metadata, data, limits and climsoft information 
#'
#'@param filename character, name of the saved file
#'
#'@param metadata data.frame, include information on metservice, key entry form, station metadata, time period, key entered person
#'                           and generation time
#'
#'@param data data.frame, data entered into the key entry web application
#'
#'@param data_limits data.frame, key entry limits for all variables
#'
#'@param climsoft_code data.frame, all variables including required information for climsoft
#'
#'@return excel workbook
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{saveXLS.R}}.
#################################################################################################

createXLS <- function(filename, metadata, data, data_limits, climsoft_code){
  currentDateTime <- Sys.time()
  ##############################################################################
  # Create "xls" file
#   if (file.exists(filename)){
#     unlink(filename)
#   }
  exc <- loadWorkbook(filename, create = TRUE)
 test<<-data 
  # Sheet: Metadata 
  createSheet(exc,'metadata')
  writeWorksheet(exc, metadata, sheet = "metadata", startRow = 1, startCol = 1)
  saveWorkbook(exc)
  
  # Sheet: Data
  createSheet(exc,'data')
  writeWorksheet(exc, data, sheet = "data", startRow = 1, startCol = 1, 
                 rownames = c("", rownames(data)), header = T)
  saveWorkbook(exc)
  
  # Sheet: Limits
  createSheet(exc,'limits')
  writeWorksheet(exc, data_limits, sheet = "limits", startRow = 1, startCol = 1)
  saveWorkbook(exc)
  
  # Sheet: Limits
  createSheet(exc,'climsoft')
  writeWorksheet(exc, climsoft_code, sheet = "climsoft", startRow = 1, startCol = 1)
  saveWorkbook(exc)
}