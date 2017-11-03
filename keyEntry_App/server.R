

shinyServer(function(input,output,session){
  
  observe({
    if (!is.null(input$metService) && input$metService != ""){
      
      ##########################################################################
      # GET FORMS
      filesList_prev <- dir(file.path(formsDir, input$metService), recursive = T)
      filesList <- basename(filesList_prev)
      print(filesList_prev)
      print(filesList)
      if (length(filesList) == 0){
        forms <- NA
        output$uiForms <- renderUI({})
      }else{
        forms <- c("", sapply(1:length(filesList), function(i){
          strsplit(filesList[i], c(".xls"))[[1]][1]
        }))
        output$uiForms <- renderUI({
          selectInput("form", text06, forms, forms[1])
        })
      }
      
      ##########################################################################
      observe({
        if (!is.null(input$form) && !is.na(input$form) && input$form !=""){
          ######################################################################
          # GET EXCEL FILENAME
          formExcel <<- file.path(formsDir, input$metService, 
                                  filesList_prev[grep(input$form, 
                                                      filesList_prev)])
          ######################################################################
          observe({
            if (length(formExcel)>0){
              ##################################################################
              # TIME PERIOD OF FORM
              if (length(grep("daily", formExcel))>0){
                timePeriod <<- "daily"
              }else if (length(grep("monthly", formExcel))>0){
                timePeriod <<- "monthly"
              }else if (length(grep("hourly", formExcel))>0){
                timePeriod <<- "hourly"
              }else{
                print(paste("No time period identified"))
                return()
              }
              
              # if there are more forms available then there is a need 
              # to choose the right one
              if(length(formExcel)>1){
                formExcel <-formExcel[2]
              }
              ##################################################################
              # READ XLS FILE
              wb <- loadWorkbook(formExcel)
              formSheets <- getSheets(wb)
              
              ##################################################################
              # GET ELEMENTS (sheet: "elements")
              formElements <<- readWorksheet(wb, sheet = "elements", header = T)
              
              ##################################################################
              # GET INPUTS (sheet: "inputs")
              formInputs <- readWorksheet(wb, sheet = "inputs", header = T)
              
              inputsList <- lapply(1:length(formInputs$input), function(i){
                id <- which(tolower(formSheets) == tolower(formInputs$input[i]))
                if (length(id) > 0){
                  # Include station_name if exists
                  if(formInputs$code[i] == "id"){
                    choices1 <- as.character(as.matrix(
                      readWorksheet(wb, sheet = id, 
                                    header = T)$station_id))
                    choices2 <- as.character(as.matrix(
                      readWorksheet(wb, sheet = id, 
                                    header = T)$station_name))
                    result <- data.frame(choices1, choices2)
                    choices <- as.vector(apply(result, 1, paste, collapse = " - "))
                    
                  }else{
                    choices <- as.character(as.matrix(
                      readWorksheet(wb, sheet = id, 
                                    header = T, colTypes =  "character")[1]))
                  }
                  list(funct = "selectInput", choices = c("", choices))
                }else{
                  list(funct = "textInput")
                }
              })
              
              # Labels of the inputs
              names(inputsList) <- formInputs$input
              
              # Inputs id
              inputsId <- sprintf("input%03d", c(1:length(formInputs$input)))
              
              # Create inputs in the web form
              output$uiInputs <- renderUI({
                uiOutputs <- list(
                  lapply(c(1:length(inputsList)), function(i){
                    inputsFunction <- get(inputsList[[i]]$funct)
                    if (inputsList[[i]]$funct == "selectInput"){
                      div(
                        style = "display:inline-block;vertical-align:top; ", 
                        inputsFunction(inputId = inputsId[i], 
                                       label = names(inputsList[i]), 
                                       choices = inputsList[[i]]$choices, 
                                       selected = "",
                                       width = formInputs$width[i])
                      )
                    }else{
                      div(
                        style = "display:inline-block;vertical-align:top; ", 
                        inputsFunction(inputId = inputsId[i], 
                                       label = names(inputsList[i]),
                                       width = formInputs$width[i])
                      )
                    }
                  }),
                  div(
                    style = "display:inline-block;vertical-align:top; width: 50px;",
                    HTML("<br>")
                  ),
                  div(
                    style = "display:inline-block;margin-top: 25px;",
                    uiOutput("uiButtons")
                  )
                )
              })
              
              ##################################################################
              # Check when all the inputs are fulfilled
              values = reactiveValues()
              values[["df2"]] <- c()
              
              values[["df2"]] <- sapply(1:length(inputsId), function(i){
                observe({
                  if (!is.null(input[[inputsId[i]]]) && input[[inputsId[i]]] != ""){
                    values[["df2"]][i] <- input[[inputsId[i]]]
                  }else{
                    values[["df2"]][i] <- NA
                  }
                })
              })
              
              observe({
                if(!is.null(values) && 
                   length(which(!is.na(unlist(values[["df2"]])))) == length(inputsId)){
                  k <<- 1
                  buttonCreate <- paste0("buttonCreate", paste0(sample(letters, 4), 
                                                                collapse = ""))
                  output$uiButtons <- renderUI({
                    div(style = "display:inline-block", 
                        uiOutputs <- list(
                          actionButton(buttonCreate, text04, width = "80px")
                        )
                    )
                    do.call(tagList, uiOutputs)
                  })
                  
                  if (exists("filename")){
                    filename_old <<- filename
                  }else{
                    filename_old <<- NA
                  }
                  
                  ##############################################################
                  # EVENT - CREATE FORM
                  observeEvent(input[[buttonCreate]], {
                    ############################################################
                    # Create temporary directory 
                    tmpDir <- file.path("www", "tmp_files")
                    if (dir.exists(tmpDir)){
                    }else{
                      dir.create(tmpDir)
                    }
                    
                    ############################################################
                    # Create dynamic "ui" id
                    letters <- sample(letters)[1:3]
                    numbers <- sample(100)[1:3]
                    uid <- paste0(numbers,letters, collapse = "")
                    
                    ############################################################
                    # Create dynamic Output (mainOutput)
                    mainOutput <- paste0("mainOutput_", uid)
                    output$uiMainOutput <- renderUI({
                      uiOutputs <- list(
                        uiOutput(mainOutput)
                      )
                      do.call(tagList,uiOutputs)
                    })
                    
                    ############################################################
                    #
                    #                       CREATE TABS
                    #
                    ############################################################
                    # Create uiOutputs for the Tabs
                    tabNames <- c(text07, text08)
                    n <- length(tabNames)
                    tabIds <- paste0("tab_",uid, c(1:n))
                    tableIds <- paste0("table_",uid, c(1:n))
                    output[[mainOutput]] <- renderUI({
                      Tabs <- lapply(1:n, function(i){
                        tabName <- tabNames[i]
                        tabId <- tabIds[i]
                        tabPanel(tabName, uiOutput(tabId))
                      })
                      do.call(tabsetPanel,Tabs)
                    })
                    
                    # Create uiOutput for the tables
                    for (i0 in 1:n){
                      local({
                        uid <- paste0(UUIDgenerate(), 1, 4)
                        my_i0 <- i0
                        tabId <- tabIds[my_i0]
                        tableId <- tableIds[my_i0]
                        uiConnect <<- paste0("uiConnect_", uid, my_i0)
                        uiMessage <<- paste0("message_", my_i0)
                        output[[tabId]] <- renderUI({
                          uiOutputs <- list(
                            rHandsontableOutput(tableId),
                            uiOutput(uiConnect),
                            HTML("<br>"),
                            uiOutput(uiMessage),
                            HTML("<br>"),
                            HTML("<br>")
                          )
                          do.call(tagList,uiOutputs)
                        })
                      })
                    }
                    
                    ############################################################
                    # 
                    #                       BUTTONS
                    # 
                    ############################################################
                    # Filename where data are stored
                    tmpDir <<- file.path("www", "tmp_files")
                    tmpFilePrev <<- c(input$metService, input$form, 
                                      sapply(1:length(inputsId), 
                                             function(i){input[[inputsId[i]]]}))
                    filename <<-  tolower(paste0(tmpFilePrev, collapse = "_"))
                    print(filename)
                    uiSaveData00 <<- paste0("saveData00_", uid)
                    uiSaveData01 <<- paste0("saveData01_", uid)
                    uiSaveData02 <<- paste0("saveData02_", uid)
                    uiSaveData03 <<- paste0("saveData03_", uid)
                    uiSaveData04 <<- paste0("saveData04_", uid)
                    
                    buttonPosition <- "display: inline-block;vertical-align:top; width: 150px;"
                    spaceBtwnButtons <- "display: inline-block;vertical-align:top; width: 25px;"
                    
                    output[[uiConnect]] <- renderUI({
                      bootstrapPage(
                        HTML("<br>"),
                        div(style = buttonPosition, uiOutput(uiSaveData00)),
                        div(style = spaceBtwnButtons, HTML("<br>")),
                        div(style = buttonPosition, uiOutput(uiSaveData01)),
                        div(style = spaceBtwnButtons, HTML("<br>")),
                        div(style = buttonPosition,  
                            downloadButton(uiSaveData02, label = text09)), 
                        div(style = spaceBtwnButtons, HTML("<br>")),
                        div(style = buttonPosition, 
                            downloadButton(uiSaveData03, label = text10)),
                        div(style = spaceBtwnButtons, HTML("<br>")),
                        div(style = buttonPosition, uiOutput(uiSaveData04))
                      )
                    })
                    
                    # Save a backup
                    saveXLS(input, output, session, tmpDir, filename, inputsId, 
                            formInputs, formElements, uiSaveData01)
                    
                    # Download as csv
                    downloadCSV(input, output, session, tmpDir, filename, 
                                uiSaveData02)
                    
                    # Download as xls
                    downloadXLS(input, output, session, tmpDir, filename, 
                                inputsId, formInputs, uiSaveData03)
                    
                    # Send E-Mail
                    output <- sendMail(input, output, session, tmpDir, filename, 
                                       inputsId, formInputs, formElements,  
                                       uiSaveData04, uiMessage)
                    
                    
                    ############################################################
                    #
                    #                       FORMS
                    #
                    ############################################################
                    observe({
                      if (exists("timePeriod")){
                        output <- createForm(input, output, session, inputsId, 
                                                    wb, formInputs, formElements, 
                                                    tableIds, buttonCreate, 
                                                    formSheets, timePeriod)
                      }
                    })
                    
                    ############################################################
                    # 
                    #                        LOG FILES
                    #
                    ############################################################
                    # Create Form
                    fileConn <- file.path(tmpDir, paste0(filename, ".txt"))
                    txt1 <- paste0(replicate(95, "#"), collapse = "")
                    txt2 <- paste0(Sys.time()," - ", textLog01, ": ", filename)
                    txt <- c(txt1, txt2)
                    write(txt, fileConn, append = T)
                    print(txt)
                    
                    # Close Form
                    if (exists("filename_old") && !is.na(filename_old) && 
                        filename_old != filename){
                      fileConn <- file.path(tmpDir, paste0(filename_old, ".txt"))
                      txt1 <- paste0(Sys.time(), " - ", textLog03, ": ", filename)
                      txt2 <- paste0(Sys.time(), " - ", textLog02)
                      txt3 <-paste0(replicate(95, "#"), collapse = "")
                      txt <- c(txt1, txt2, txt3)
                      write(txt, fileConn, append = T)
                      print(txt)
                    }
                    
                    # Close Form due to ending the App
                    session$onSessionEnded(function() {
                      fileConn <- file.path(tmpDir, paste0(filename, ".txt"))
                      txt1 <- paste0(Sys.time(), " - ", textLog04)
                      txt2 <- paste0(Sys.time(), " - ", textLog02)
                      txt3 <- paste0(replicate(95, "#"), collapse = "")
                      txt <- c(txt1, txt2, txt3)
                      write(txt, fileConn, append = T)
                      print(txt)
                    })
                  }) # End "observeEvent(input[[buttonCreate]]"
                  
                }else{
                  k <<- 0
                  output$uiButtons <- renderUI({})
                  output$uiMainOutput <- renderUI({})
                }
              }) # End "observe"
              
              
            }else{
              output$formId <- renderUI({})
              output$uiInputs <- renderUI({})
              output$uiButtons <- renderUI({})
              output$uiMainOutput <- renderUI({})
            }
          }) # End "observe"
        }else{
          output$formId <- renderUI({})
          output$uiInputs <- renderUI({})
        }
      }) # End "observe"
    } 
  })  # End "observe"
})