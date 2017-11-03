##########################################################################
#' @export sendMail
#'
#' @title sendMail  
#'
#' @description Excel workbook of the application \code{\link[KeyEntry]{server.R}}, can be send by mail
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
#'@param formInputs data.frame, use the excelsheet inputs
#'
#'@param formElements data.frame, use the excelsheet elements
#'
#'@param uiSaveData04 character string, unique identifier for temp folder. 
#'
#'@param uiMessage character string, unique identifier for shiny ui to display message from send mail function
#'
#'@return output for shiny, message to be displayed in interface. Popupwindows to send .xls file via email 
#'#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{server.R}}.
#################################################################################################

sendMail <- function(input, output, session, tmpDir, filename, inputsId, formInputs, formElements, uiSaveData04, uiMessage){

  ##############################################################################
  # Libraries
  library(mailR)
  library(uuid)
  library(shinyBS)
  library(XLConnect)
  library(uuid)
  
  ##################################################################
  #
  #                         SEND E-MAIL
  #
  ##################################################################
  # Emails to send the data from
  observe({
    if (tolower(input$metService) == "inamet"){
      choices <- "formularios.inamet@gmail.com"
      choices2 <- "e.g. receiver@gmail.com"
      inputsFunction2 <- function(inputId, label, choices2){
        textInput(inputId = inputId, label = label, placeholder = choices2)
      }
      inputsFunction <- function(inputId, label, choices2){
        selectInput(inputId = inputId, label = label, choices = choices)
      }
    }else if (tolower(input$metService) == "zmd"){
      choices <- "forms.zmd@gmail.com"
      choices2 <- "climateforms@zmd.gov.zm"
      inputsFunction2 <- function(inputId, label, choices2){
        selectInput(inputId = inputId, label = label, choices = choices2)
      }
      inputsFunction <- get("inputsFunction2")
    }else{
      print("caracCOLES")
      choices <- "e.g. sender@gmail.com"
      choices2 <- "e.g. receiver@gmail.com"
      inputsFunction2 <- function(inputId, label, choices2){
        textInput(inputId = inputId, label = label, placeholder = choices2)
      }
      inputsFunction <- get("inputsFunction2")
    }
    
    uid <- substr(UUIDgenerate(), 1, 4)
    loginNew <- paste0("loginNew_", uid)
    connectButtonId <- paste0("testing_", uid)
    print(connectButtonId)
    output[[uiSaveData04]] <- renderUI({
      #       uid <- substr(UUIDgenerate(), 1, 4)
      #       loginNew <- paste0("loginNew_", uid)
      #       connectButtonId <- paste0("testing_", uid)
      bootstrapPage(
        actionButton(
          connectButtonId,
          label = text11,
          icon = icon("envelope", "fa-1x")),
        bsModal(
          "popup", text11a, trigger = connectButtonId,
          size = "small",
          tabPanel(
            "loginPanel", br(),
            tags$form(
              inputsFunction("sender", 
                             label = text12,
                             choices = choices),
              inputsFunction2("receiver", text13, choices2),
              passwordInput("passwd", 
                            label = text14),
              actionButton(loginNew, 
                           label = h5("OK")
              )
            )
          )
        )
      )
    })
    
    
    
    observeEvent(input[[loginNew]],{
      toggleModal(session, modalId = "popup", toggle = "close")
      ##############################################################################
      form <- input$form
      metService <- input$metService
      from <- input$sender
      to <- input$receiver
      textInfo <- paste0(text18,"'", filename,"'")
      host.name <- "smtp.gmail.com"
      port <- 465
      tls <- F
      ssl <- T
      user.name <- input$sender
      passwd <- input$passwd
      currentDate <- Sys.Date()
      currentDateTime <- Sys.time()
      
      # Load the data 
      # Load the data file
      tmpFile1 <- file.path(tmpDir, paste0(filename, ".RData"))
      tmpFile2 <- file.path(tmpDir, paste0(filename, "_limits.RData"))
      load(tmpFile1)
      df <- tmp.df1
      
      # Load the limits file
      load(tmpFile2)
      df_limits <- tmp.df2
      
      ##############################################################################
      tmpDir2 <- file.path(tmpDir, "email")
      if (dir.exists(tmpDir2)){
      }else{
        dir.create(tmpDir2)
      }
      # Create "xls" file
      # Metadata
      metadata_prev <- sapply(1:length(inputsId), function(i){
        tt <- input[[inputsId[i]]]
        tt}) 
      metadata <- data.frame(
        "inputsId" = c("metService", "form", inputsId, NA),
        "input" = c("Met Service","Form", formInputs$input, "File generated on:"),
        "value" = c(metService, form, metadata_prev, as.character(currentDateTime))
      )
      
      ##############################################################################
      # Create "xls" file
      columns <- c("element_abbr", "climsoft_code", "units", "type", 
                   "time_period", "time")
      climsoft_code <- data.frame(formElements[,columns])
      xlsFile <- file.path(tmpDir2, gsub(".RData", ".xls", basename(tmpFile1)))
      createXLS(xlsFile, metadata, df, df_limits, climsoft_code)
      
      
      ##############################################################################
      # Create "csv" file
      csvFile <- file.path(tmpDir2, gsub(".RData", ".csv", basename(tmpFile1)))
      write.csv(df, file = csvFile, append = T)
      
      ##############################################################################
      # Send E-Mail
      body <- paste0(text16, currentDate)
      body2 <- body
      tr <<- tryCatch(send.mail(from = from, 
                                to = to,
                                subject = textInfo,
                                body = body,
                                smtp = list(host.name = host.name, 
                                            port = port, 
                                            user.name = user.name, 
                                            passwd = passwd, 
                                            ssl = ssl,
                                            tls = tls),
                                authenticate = TRUE,
                                send = TRUE,
                                attach.files = c(csvFile, xlsFile),
                                debug = F), error = function(e) NULL)
      if (is.null(tr)){
        message <- text17
        #return(message)
      }else{
        unlink(tmpDir2, recursive = T)
        message <- paste0(currentDateTime, ": ", textInfo)
        # Log file
        fileConn <- file.path(tmpDir, paste0(filename, ".txt"))
        txt <- paste0(Sys.time(), " - ", textLog08, "'", input$receiver,"'")
        write(txt, fileConn, append = T)
        print(txt)
      }
      message2 <- message
      output[[uiMessage]] <- renderText({message2})
      
    })
  })
  return(output)
}

