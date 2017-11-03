##########################################################################
#' @export createForm_daily
#'
#' @title Get import table create hourly form in browser
#'
#' @description Use an import excel sheet in the folder hourly to create a hourly forms in a browser 
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
#' @param inputsId integer, number of the read worksheed called, in the server 
#' function
#'
#' @param wb class. imported excel workbook from selected folder 
#'
#' @param formInputs data.frame, use the excelsheet elements  
#'
#' @param formElements data.frame, use the excelsheet elements  
#'
#' @param tableIds input/output variable, required by shiny to display the
#' interactive table
#'
#' @param buttonCreate shiny output, defines the number and functions of 
#' required interactive button
#'
#' @param formSheets character vector, names of all worksheets in the 
#' imported workbook
#'
#'
#' @return output for shiny, create an interactiv form sheet for hourly values
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{server.R}}.
#'
#' @author Posada R, Riede JO (SASSCAL/DWD), April 2017
################################################################################

createForm <- function(input, output, session, inputsId, wb, 
                             formInputs, formElements, tableIds, 
                             buttonCreate, formSheets, timePeriod){
  
  library(uuid)
  
  observe({
    if (input[[buttonCreate]] == 0)
      return()
    isolate({
      currentDateTime <- Sys.time()
      options(warn=-1)
      values <- reactiveValues()
      
      ##########################################################################
      # GET MANDATORY INPUTS
      year <- sprintf("input%03d", which(formInputs$code == "yyyy"))
      yyyy <- sprintf("%02d", as.numeric(input[[year]]))
      month <-  sprintf("input%03d", which(formInputs$code == "mm"))
      mm <- sprintf("%02d", as.numeric(input[[month]]))
      
      ##########################################################################
      # TYPE OF FORM
      if (timePeriod == "daily"){
      rows <- sprintf("%02d", c(1:numberOfDays(yyyy, mm)))
      }else if (timePeriod == "hourly"){
        rows <- sprintf("%04d", 100*seq(1,24,1))
      }else{
        return()
      }
      
      ##########################################################################
      # FILE NAMES
      tmpDir <- file.path("www", "tmp_files")
      tmpFilePrev <- c(input$metService, input$form, 
                       sapply(1:length(inputsId), 
                              function(i){input[[inputsId[i]]]}))
      filename <- tolower(paste0(tmpFilePrev, collapse = "_"))
      tmpFile1 <- file.path(tmpDir, paste0(filename,".RData"))
      csvFile <- file.path(tmpDir, paste0(filename,".csv"))
      xlsFile <- file.path(tmpDir, paste0(filename, ".xls"))
      tmpFile2 <- file.path(tmpDir, paste0(filename,"_limits.RData"))
      
      ##########################################################################
      # GET ELEMENTS (sheet: "elements")
      varName <- formElements$element_name
      varHeader <- formElements$element_abbr
      varType <- formElements$element_type
      
      ##########################################################################
      # CLIMSOF CODES
      climsoftCode <- formElements$climsoft_code
      
      ##########################################################################
      # COLUMN NAMES
      col.names <- varHeader
      id <- which(!is.na(formElements$rule))
      ##########################################################################
      # ROW NAMES & CONDITIONS
      row.calculations <- c("MAX.", "MIN.", "SUM", "MEAN")
      row.names <- c(rows, row.calculations)
      row.names2 <- row.names
      row.highlight <- c(((length(row.names)-length(row.calculations))+1):
                           length(row.names))
      
      # Text used in rhandsontable to set up a condition as java code
      rowCondition <- paste0("row==", row.highlight-1, collapse = " || ")
      
      ##########################################################################
      # 
      #                 CREATE DATA.FRAME FOR KEY ENTRY (df11)
      #
      ##########################################################################
      if (file.exists(tmpFile1)){
        load(tmpFile1)
        df1 <- tmp.df1
      }else{
        df.empty <- data.frame(matrix(numeric(0), nrow = length(row.names), 
                                      ncol = length(col.names)))
        df1 <- df.empty
      }
      colnames(df1) <- col.names
      rownames(df1) <- row.names
      
      # CHECK TYPE OF INPUT
      id00 <- which(colnames(formElements) == "type")
      if (length(id00) == 1){
        id01 <- which(formElements[,id00] == "text")
        df1[,id01] <- lapply(df1[,id01], as.character)
      }
      
      values[["df1"]] <- df1
      
      df11 <- reactive({
        if (!is.null(input[[tableIds[1]]])) {  
          temp <- hot_to_r(input[[tableIds[1]]])
          values[["df1"]] <- temp
        }
        df <- values[["df1"]]
        df
      }) %>% debounce(1000)
      
      ##########################################################################
      # 
      #                   CREATE DATA.FRAME FOR LIMITS (df22)
      #
      ##########################################################################
      if (file.exists(tmpFile2)){
        load(tmpFile2)
        df2 <- tmp.df2
      }else{
        minValue <- formElements$minimum
        maxValue <- formElements$maximum
        scaleFactor <- formElements$scale_factor
        df2 <- data.frame(varHeader, varName, maxValue, minValue, scaleFactor)
        tmp.df2 <- df2
        save(tmp.df2, file = tmpFile2)
      }
      
      if (exists("tmp.df2")){
        minValue <- tmp.df2$minValue
        maxValue <- tmp.df2$maxValue
        scaleFactor <- tmp.df2$scale_factor
      }else{
        maxValue <- df2$maxValue
        minValue <- df2$minValue
        scaleFactor <- df2$scale_factor
      }
      
      values[["df2"]] <- df2
      
      df22 <- reactive({
        if (!is.null(input[[tableIds[2]]])) {  
          temp <- hot_to_r(input[[tableIds[2]]])
          values[["df2"]] <- temp
        }
        df <- values[["df2"]]
        df
      })
      
      ##########################################################################
      # 
      #                           CHECK LIMITS
      #
      ##########################################################################
      # Make a comparison of elements, such as "temp_min" & "temp_max"
      # The output is a text used in rhandsontable to set up a condition as 
      # js code
      checkElementsCondition <- checkElements(climsoftCode, rows)
      
      # Create a "renderer" to show values above the limit in red (js code)
      renderer <- makeLimits(tmp.df2, df22(), rows, rowCondition, 
                             checkElementsCondition, varName)
      
      ##########################################################################
      #
      #                           CREATE 'RHANDSONTABLES'
      #
      ##########################################################################
      # Create rhandsontable for keyEntry form (df11)
      tempNew11 <- makeRules(formElements, wb, df11(), formSheets, rows)
      hot1 <- rhandsontable(tempNew11, useTypes = TRUE, 
                            row_highlight = row.highlight) %>% 
        hot_cols(colWidths = 55, rowHeights = 23) %>%
        hot_cols(halign = "htCenter") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_col(
          id, readOnly = TRUE,
          renderer = paste0(
            "function(instance, td, row, col, prop, value, cellProperties) {",
            "Handsontable.TextCell.renderer.apply(this, arguments);
      }")
        ) %>% hot_cols(renderer = renderer)
      
      
      output[[tableIds[1]]] <- renderRHandsontable({
        hot1
      })
      ##########################################################################
      # Save df11
      observe({   
        if (!is.null(input[[tableIds[1]]])) {
          tmp.df1 <- hot_to_r(input[[tableIds[1]]])
          save(tmp.df1, file = tmpFile1)
          }
      })
      # Create rhandsontable for limits (df22)
      output[[tableIds[2]]] <- renderRHandsontable({
        tempNew22 <- df22()
        hot2 <- rhandsontable(tempNew22, rowHeaders = NULL) %>% 
          hot_col(3, format = "0[.]0") %>%
          hot_col(4, format = "0[.]0")
        hot2
      })
      
      ##########################################################################
      # Save df22
      observe({   
        if (!is.null(input[[tableIds[2]]])) {
          tmp.df2 <- hot_to_r(input[[tableIds[2]]])
          save(tmp.df2, file = tmpFile2)
        }
      })
      
      ##########################################################################
      #
      #                 CALCULATE VALUES (MIN, MAX, MEAN, SUM)
      #
      ##########################################################################
      # Create button
      uid <- substr(UUIDgenerate(),1,4)
      calculateValues <- paste0("calculateValues_", uid)
      output[[uiSaveData00]] <- renderUI({
        actionButton(calculateValues, width = "150px",
                     label = text20,
                     icon = icon("calculator",  "fa-1x"))
      })
      observeEvent(input[[calculateValues]],{
        tempNew <- makeRules(formElements, wb, df11(), formSheets, rows)
        hot <- rhandsontable(tempNew, row_highlight = row.highlight) %>% 
          hot_cols(colWidths = 55) %>%
          hot_cols(halign = "htCenter") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_cols(renderer = renderer) %>%
          hot_col(id, readOnly = TRUE, renderer = paste0(
            "function(instance, td, row, col, prop, value, cellProperties) {",
            "Handsontable.TextCell.renderer.apply(this, arguments);
              }")
          )
        
        output[[tableIds[1]]] <- renderRHandsontable({hot})
      })
    })
  })
  
  return(output)
}