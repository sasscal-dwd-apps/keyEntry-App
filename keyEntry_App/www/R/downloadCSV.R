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
#' @param tmpDir character, path where save files are stored
#'
#' @param filename character, name of the saved file
#'
#'
#' @param outputName character string, unique identifier for temp folder. 
#'
#' @return output for shiny, write .csv file to a user defined location
#' 
#'
#' @details This function will be called by the function
#' \code{\link[KeyEntry]{server.R}}.
#################################################################################################



downloadCSV <- function(input, output, session, tmpDir, filename, outputName){
  tmpFile1 <- file.path(tmpDir, paste0(filename, ".RData"))
  tmpFile2 <- file.path(tmpDir, paste0(filename, "_limits.RData"))
  csvFile <- gsub(".RData", ".csv", tmpFile1)
  output[[outputName]] <- downloadHandler(
    filename = function() { basename(csvFile) },
    content = function(file) {
      load(tmpFile1)
      write.csv(tmp.df1, file, row.names =T, sep = ";")
      
      # Log file
      fileConn <- file.path(tmpDir, paste0(filename, ".txt"))
      txt <- paste0(Sys.time()," - ", textLog06)
      write(txt, fileConn, append = T)
      print(txt)
    }
  )
  return(output)
}