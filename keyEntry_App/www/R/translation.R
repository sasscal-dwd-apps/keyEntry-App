##########################################################################
#' @export translation
#'
#' @title Translate display text
#'
#' @description translate text which is shown trough the UI into the webbrowser
#'  
#'@param language character, character with the used language on the display  
#'
#'@return textInfo, list of character used in the UI
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{Ui.R}}.
#'
#' 
##########################################################################
translation <- function(language){

  ############################################################################
  #
  #                         ENGLISCH
  #
  ############################################################################
  
  language <<- language
  if(language == "german"){
    language <- "english"
  }
  if (language == "english"){
    ##########################################################################
    # Side Panel
    textTitlePanel <- "Key Entry App"
    text00a <- "Select"
    text00 <- "Load file"
    text01 <- "Create key-entry form"
    text02 <- "Met. Service"
    text03 <- "Select Form"
    text04 <- "Create"
    text05 <- "Select File"
    
    ##########################################################################
    # Form
    text06 <- "Form"
    
    ##########################################################################
    # Tables
    text07 <- "Key Entry Form"
    text08 <- "Limits & Scale Factors"
    
    ##########################################################################
    # Buttons
    text09 <- "Download as '.csv'"
    text10 <- "Download as '.xls'"
    text10a <- "Save data"
    text10b <- "A backup of this key-entry form has been saved in the ftp-server"
    text11 <- "Send data per E-Mail"
    text11a <- "Send E-Mail"
    text12 <- "From"
    text13 <- "To"
    text14 <- "Password"
    text15 <- "Remove history"
    text20 <- "Calculate values"
    
    ###########################################################################
    # Email
    text16 <- "E-Mail generated automatically on "
    text17 <- "E-Mail address and/or Password are not valid"
    text18 <- "Key-Entry Form: "
    text19 <- "sent successfully!"
    
    ###########################################################################
    # Text for the log files
    textLog01 <- "Form opened"
    textLog02 <- "Form closed"
    textLog03 <- "Changed to form"
    textLog04 <- "Browser has been closed"
    textLog05 <- "A backup of the form has been made"
    textLog06 <- "Form Downloaded as an ASCII file ('.csv')"
    textLog07 <- "Form Downloaded as an Excel file ('.xls')"
    textLog08 <- "Form sent per E-mail to: "
  }

  ############################################################################
  #
  #                         PORTUGUESE
  #
  ############################################################################
  if (language == "portuguese"){
    ##########################################################################
    # Side Panel
    textTitlePanel <- "Entrada de dados"
    text00a <- "Selecionar"
    text00 <- "Carregar arquivo"
    text01 <- "Criar formulario"
    text02 <- "Servi\u{00E7}o Met."
    text03 <- "Selecionar Formul\u{00E1}rio"
    text04 <- "Criar"
    text05 <- "Selecionar arquivo"

    ###########################################################################
    # SERVER
    text06 <- "Formul\u{00E1}rio"
    
    ###########################################################################
    # Tables
    text07 <- "Formul\u{00E1}rio"
    text08 <- "Limites e Fatores de Escala"

    ###########################################################################
    # Buttons
    text09 <- "Baixar como '.csv'"
    text10 <- "Baixar no Excel"
    text10a <- "Guardar os dados"
    text10b <- "Uma copia de seguran\u{00E7}a deste formul\u{00E1}rio foi salvo no servidor FTP"
    text11 <- "Enviar dados por E-Mail"
    text11a <- "Enviar E-Mail"
    text12 <- "De"
    text13 <- "Para"
    text14 <- "Senha"
    text15 <- "Remover hist\u{00F3}rico"
    text20 <- "Calcular valores"
    
    ###########################################################################
    # Email
    text16 <- "E-Mail gerado automaticamente na data "
    text17 <- "Endere\u{00E7}o de e-mail e/ou senha n\u{00E3}o s\u{00E3}o v\u{00E1}lidos"
    text18 <- "Formulario: "
    text19 <- "enviado com sucesso!"
    
    ###########################################################################
    # Text for the log files
    textLog01 <- "Formul\u{00E1}rio aberto"
    textLog02 <- "Formul\u{00E1}rio rechado"
    textLog03 <- "Alterado para Formul\u{00E1}rio"
    textLog04 <- "Navegador foi fechado"
    textLog05 <- "Uma copia de seguran\u{00E7}a do formul\u{00E1}rio foi feita"
    textLog06 <- "Formul\u{00E1}rio descarregado como um ficheiro ASCII ('.csv')"
    textLog07 <- "Formul\u{00E1}rio descarregado como um ficheiro do Excel ('.xls')"
    textLog08 <- "Formul\u{00E1}rio enviado por E-Mail para:"
  }
  

  variables <- grep("text", ls(), value=TRUE)
  textInfo <- vector("list", length(variables))
  names(textInfo) <- variables
  for (ii in 1:length(variables)){
    textInfo[[ii]] <- get(variables[ii])
  }
  return(textInfo)
}
