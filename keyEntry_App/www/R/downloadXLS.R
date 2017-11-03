##########################################################################
#' @export downloadXLS
#'
#' @title downloadXLS 
#'
#' @description Download option for key entered data from the web application to a selected folder
#'
#' @param input shiny object. It is a list-like object.
#' It stores the current values of all of the widgets in the
#' application. The Shiny App uses the \code{input}
#' to receive user input from the client web browser.
#' 
#' @param output shiny object. It is a list-like object that stores
#' instructions for building the R objects in the Shiny application.
#' Shiny application.
#' 
#' @param session shiny object. It is a special object that is used for finer
#' control over a user's app session.
#'
#'@param tmpDir character, path where save files are stored
#'
#'@param filename character, name of the saved file
#'
#'@param inputsId integer, number of the read worksheed called, in the server function
#'
#'@param formInputs data.frame, use the excelsheet elements
#'
#'@param outputName character string, unique identifier for temp folder. 
#'
#'@return output for shiny,  write .xls file to a user defined location
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{server.R}}.
#################################################################################################

downloadXLS <- function(input, output, session, tmpDir, filename, inputsId, formInputs, outputName){
  tmpFile1 <- file.path(tmpDir, paste0(filename, ".RData"))
  tmpFile2 <- file.path(tmpDir, paste0(filename, "_limits.RData"))
  xlsFile <- gsub(".RData", ".xls", basename(tmpFile1))
  
  output[[outputName]] <- downloadHandler(
    filename = function() {xlsFile},
    content = function(file) {
      xlsFileNew <- basename(xlsFile)
      # Metadata
      metadata_prev <- sapply(1:length(inputsId), function(i){
        tt <- input[[inputsId[i]]]
        tt}) 
      metadata <<- data.frame(
        "inputId" = c("metService", "form", "form_type", formInputs$code, NA),
        "input" = c("Met Service","Form", "Form Type", formInputs$input, "File generated on:"),
        "value" = c(input$metService, input$form, timePeriod, metadata_prev, as.character(Sys.time()))
      )
      
      # climsoft_code
      columns <- c("element_abbr", "climsoft_code", "units", "type", 
                   "time_period", "time")
      climsoft_code <- data.frame(formElements[,columns])
      
      # Data
      load(tmpFile1)
      
      # Limits 
      load(tmpFile2)
      createXLS(xlsFileNew, metadata, tmp.df1, tmp.df2, climsoft_code)
      file.rename(xlsFileNew, file)
      
      # Log file
      fileConn <- file.path(tmpDir, paste0(filename, ".txt"))
      txt <- paste0(Sys.time()," - ", textLog07)
      write(txt, fileConn, append = T)
      print(txt)
    }
  )
  return(output)
}