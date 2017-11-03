####################################################################
# All packages required for running the program
options(warn = -1, message = F)
suppressMessages(library(jsonlite))
suppressMessages(library(gdata))
suppressMessages(library(shinyBS))
suppressMessages(library(rhandsontable))
suppressMessages(library(shiny))
suppressMessages(library(XLConnect))
suppressMessages(library(uuid))

###########################################################################
#
#                           SOURCE R-SCRIPTS
#
###########################################################################
# Source the R-Scripts placed in the App
dirR <- file.path(".", "www", "R")
pathnames <- list.files(pattern="[.]R$", path=dirR, full.names=TRUE)
sapply(pathnames, FUN=source)

#############################################################################
# SET TIMEZONE
Sys.setenv(TZ = "UTC")

################################################################################
# SET LOCAL SETTINGS
if (file.exists("../localSettings.rda")){
  load("../localSettings.rda")
  language <<- localSettings$language
  metService <- localSettings$metService
}else{
  language <<- "english"
  metService <- "other"
}

################################################################################
# SET LOCAL LANGUAGE
if (language == "portuguese"){
  Sys.setlocale("LC_COLLATE", "Portuguese_Portugal.1252")
  language_abbr <- "pt"
  documentation <- "Ajuda"
}else if (language == "german"){
  Sys.setlocale("LC_COLLATE", "German_Germany.1252")
  language_abbr <- "en"
  documentation <- "Help"
}else{
  Sys.setlocale("LC_COLLATE", "English_United Kingdom.1252")
  language_abbr <- "en"
  documentation <- "Help"
} 

################################################################################
# SET MET SERVICE

if (tolower(metService) == "inamet"){
  theme <- "inamet.css"
  logo <- "logoINAMET.png"
  logoSize <- "50%"
  
}else if (tolower(metService) == "zmd"){
  theme <- "sasscal.css"
  logo <- "logoZMD.png"
  logoSize <- "50%"
  
}else if (tolower(metService) == "dms"){
  theme <- "sasscal.css"
  logo <- "logoDMS.png"
  logoSize <- "50%"
}else if (tolower(metService) == "saws"){
  theme <- "sasscal.css"
  logo <- "logoSAWS.png"
  logoSize <- "50%"
}else{
  theme <- "sasscal.css"
  logo <- "logoNULL.png"
  logoSize <- "30%"
}

###########################################################################
# CREATE DOCUMENTATION
dirDoc <<- file.path(".", "www", "documentation", language_abbr)
rmarkdown::render(file.path(dirDoc, "documentation.Rmd"),  encoding = 'UTF-8')

# Copy the rmarkdowns
newFolder <- file.path("..","docs", language_abbr)
dir.create(newFolder, showWarnings = T, recursive = T)
files_from <- grep(list.files(dirDoc), pattern = ".Rmd$", inv = T, value = T)
files_to <- file.path(newFolder, files_from)
file.copy(file.path(dirDoc,files_from), newFolder, recursive = F)

###########################################################################
# GET TEXT
textInfo <<- translation(language)
for (i1 in c(1:length(textInfo))){
  assign(names(textInfo[i1]),textInfo[[i1]], envir = .GlobalEnv)
}

#############################################################################
# Select what wants to be done
options <- c(text00, text01,"")

# Select Meteorological Service
#wwwDir <<- file.path(".", "www")
formsDir <<- file.path("www", "forms")
metServices <- as.character(as.matrix(metService))
#############################################################################
# INTERFACE
shinyUI(
  fluidPage(theme = file.path("bootstrap",theme),
    #HTML("<br>"),
    fluidRow(
      column(11),
      column(1, a(documentation, target="_blank", 
                  href=file.path("documentation",language_abbr, "documentation.html")))
    ),
    fluidRow(
      column(2, div(img(src=file.path("images", logo), width = logoSize, 
                        height = logoSize),
                    style = "text-align: left;")),
      column(8, h1(textTitlePanel, align = "center")),
      column(2, div(img(src=file.path("images","logoSASSCAL.png"), 
                        width = "65%", height = "65%"),
                    style = "text-align: right;"))
    ),
    fluidRow(
      div(
        style = "display:inline-block;vertical-align:top; width: 40px;", 
        HTML("<br>")
      ),
      # Meteorological Service
      #column(2,
      div(
        style = "display:inline-block;vertical-align:top; width: 100px;", 
             selectInput(
               "metService", text02, choices = metServices,
               selected = ""
             )
      ),
      # Form
     # column(2,
     div(
       style = "display:inline-block;vertical-align:top; width: 100px;", 
             conditionalPanel(
               condition = "input.metService != ''",
               uiOutput("uiForms")
             )
      ),
     div(
       style = "display:inline-block;vertical-align:top; width: 40px;", 
       HTML("<br>")
     ),
      # Other inputs
      #column(8,
     div(
       style = "display:inline-block;vertical-align:top; ", 
             conditionalPanel(
               condition = "input.metService != ''",
               uiOutput("uiInputs")
             )
     )
    ),
    
    # OUTPUTS
    uiOutput("uiMainOutput"),
    uiOutput("uiMessages")

  )
)