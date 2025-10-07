
# Lines needed for deployment
#remotes::install_github("https://github.com/worldhealthorganization/doseresponsemodels/", force = TRUE)
#remotes::install_github("https://github.com/worldhealthorganization/qraLm", force = TRUE)
rm(list = ls())
gc()

################" GLOBAL

options(myVerbose = FALSE)
options(myRepos = "https://worldhealthorganization.github.io/")
# Function to check and install missing packages


# Load necessary libraries and source required files
source_all <- function() {
  libraries <- c(
    "shiny" ,
    "ggplot2" 
    # #"plotly", 
    # "DT", 
    # "shinyjs", 
    # "dplyr", 
    # "doseresponsemodels",
    # "qraLm", "ggplot2", 
    # "shinythemes", "shinyWidgets",
    # # "Hmisc", 
    # "mc2d", 
    # "fontawesome", "matrixStats",
    # "shinydashboard", 
    # "markdown" # Addition RP
  )
  
  # Load or install missing libraries
  lapply(libraries, function(lib) {
    # if (!require(lib, character.only = TRUE)) {
    #   install.packages(lib, dependencies = TRUE)
    # }
    library(lib, character.only = TRUE)
    # }
  })
  
  # Source module files if they exist
  module_files <- list.files(path = "modules", pattern = "\\.R$", full.names = TRUE)
  if (length(module_files) > 0) {
    lapply(module_files, source)
  }
  
  # Source page files if they exist
  page_files <- list.files(path = "pages", pattern = "\\.R$", full.names = TRUE)
  if (length(page_files) > 0) {
    lapply(page_files, source)
  }
}

# Call the function to load libraries and source files
source_all()

library("doseresponsemodels") # Addition RP, so that RSconnect find it
library("qraLm") # Addition RP, so that RSconnect find it
# library("shinythemes") # Addition RP, so that RSconnect find it
# library("shinyWidgets") # Addition RP, so that RSconnect find it
# library("shinydashboard") # Addition RP, so that RSconnect find it
# library("markdown") # Addition RP, so that RSconnect find it

################" UI


# Create UI function
create_ui <- function(id) {
  fluidPage(
    # Move head elements outside navbarPage()
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", href = "www/img/favicon.ico", type = "image/x-icon"),
      tags$script(src = "custom.js"),
      tags$style(HTML("
        body > nav > div {
          display: flex;
          flex-direction: row;
          align-items: center;
          flex-wrap: nowrap;
          background-color: #0093D5;
        }
        #footer-content {
          min-height: 50px;
        }
      ")),
      tags$style(HTML("
      .tab-content {
        padding-top: 20px;  
      }
    ")),
      tags$style(HTML("
      .centered {
        display: flex;
        justify-content: center;
      }
    ")),
      tags$style(HTML("
      .spaced-paragraph {
        margin-bottom: 20px;
      }
    "))
    ),
    
    # NavbarPage (Main App Navigation)
    navbarPage(
      title = div(
        img(src = 'img/logo.svg', height = '30px', 
            alt = "Shiny_qraLm", style = "margin-right: 10px")
      ),
      windowTitle = "Shiny_qraLm",
      id = "navbarPageId",
      collapsible = TRUE,
      position = "static-top",
      inverse = TRUE,
      fluid = TRUE,
      theme = "custom.css",
      selected = "About",
      
      # Define tabs
      tabPanel("About", about_ui("about")),
      tabPanel("Frozen Vegetables", value = "FV", frozenvegetables_ui("frozenvegetables")),
      tabPanel("Smoked Fish", value = "SF", smokedfish_ui("smokedfish")),
      tabPanel("RTE Cantaloupe", value = "CA", rtecantaloupe_ui("rtecantaloupe"))
    ),
    
    # Footer moved outside navbarPage
    tags$footer(
      id = "footer-content",
      style = "position: relative; bottom: 0; left: 0; right: 0;
               z-index: 1; background-color: #0093D5; color: #ffffff;
               display: flex; justify-content: space-between; align-items: center;",
      div(style = "padding: 10px;", "Copyright © 2025, WHO"),
      div(style = "padding: 10px;",
          "Shiny_qraLm website:",
          a(href = "https://github.com/WorldHealthOrganization/Shiny_qraLm/", target = "_blank", "Shiny_qraLm")
      )
    )
  )
}

ui = create_ui("app")

################" Server

# Create the server function
create_server <- function(input, output, session) {
  about_server(input, output, session, "about")
  frozenvegetables_server(input, output, session, "frozenvegetables")
  smokedfish_server(input, output, session, "smokedfish")
  rtecantaloupe_server(input, output, session, "rtecantaloupe")
}

server = function(input, output, session) {
  
  # Memory usage logger
  # mem_timer <- reactiveTimer(1000, session)
  # observe({ mem_timer() 
  #   cat( format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "- Memory used (bytes):", pryr::mem_used(), "\n" ) 
  # })
  
  
  observe({
    showModal(modalDialog(
      title = HTML("<strong>Disclaimer</strong>"),
      HTML("This &ldquo;WHO Risk Estimation Tool&rdquo; for <i>Listeria monocytogenes</i> in 
           Foods runs in its own protected environment and access is SSL encrypted,
           and uploaded data are not saved once you close the session. However, 
           the data will be temporarily stored in the cloud hosting the application
           and thus, users are advised to ensure data is de-identifiable.<br>
           All reasonable precautions have been taken by WHO to verify the calculations 
           performed by this application. However, the application is being distributed
           without warranty of any kind, either express or implied. The responsibility 
           for the use and interpretation of the application’s output lies with 
           the user. In no event shall the World Health Organization be liable 
           for damages arising from its use."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  create_server(input, output, session)
  
}

shinyApp(ui, server)

