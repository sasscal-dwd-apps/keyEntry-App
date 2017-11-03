##########################################################################
#' @export saveXLS
#'
#' @title save Excel wookbook to file
#'
#' @description Save an excel workbook to a selected folder 
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
#'@param uiSaveData01 character string, unique identifier for temp folder. 
#'
#'@return output for shiny, create an interactiv form sheet for hourly values
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{server.R}}.
#################################################################################################

saveXLS <- function(input, output, session, tmpDir, filename, inputsId, formInputs, formElements, uiSaveData01){
  # Create Button and warning
  uid <- substr(UUIDgenerate(),1,4)
  saveData01 <- paste0("saveData01_", uid)
  output[[uiSaveData01]] <- renderUI({
    bootstrapPage(
      actionButton(saveData01, width = "150px",
                   label = text10a,
                   icon = icon("save",  "fa-1x")),
      bsModal("popup0", 
              trigger = saveData01,
              title = NULL,
              size = "small",
              paste0(text10b)#, ": '", basename(xlsFile), "'")
      ))
  })
  
  # Create Excel File
  observeEvent(input[[saveData01]],{
    # Log file - Create Form
    fileConn <- file.path(tmpDir, paste0(filename, ".txt"))
    write(paste0(Sys.time()," - ", textLog05), fileConn, append = T)
    
    # Load the data 
    tmpFile1 <<- file.path(tmpDir, paste0(filename, ".RData"))
    tmpFile2 <<- file.path(tmpDir, paste0(filename, "_limits.RData"))
    
    # File name 
    xlsFile <<- gsub(".RData", ".xls", tmpFile1)
    
    # Metadata 
    metadata_prev <- sapply(1:length(inputsId), function(i){
      tt <- input[[inputsId[i]]]
      tt}) 
    metadata <<- data.frame(
      "inputId" = c("metService", "form", "form_type", formInputs$code, NA),
      "input" = c("Met Service","Form", "Form Type", formInputs$input, "File generated on:"),
      "value" = c(input$metService, input$form, timePeriod, metadata_prev, as.character(Sys.time()))
    )
    
    load(tmpFile1)
    load(tmpFile2)
    
    ##############################################################################
    # Create "xls" file
    columns <- c("element_abbr", "climsoft_code", "units", "type", 
                 "time_period", "time")
    climsoft_code <<- data.frame(formElements[,columns])
    xlsFile <<- file.path(tmpDir, gsub(".RData", ".xls", basename(tmpFile1)))
    
    df1 <<- tmp.df1
    df1_limits <<- tmp.df2
    createXLS(xlsFile, metadata, tmp.df1, tmp.df2, climsoft_code)
  })
  return(output)
}