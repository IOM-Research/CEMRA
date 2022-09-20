#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 
 
app_ui <- function(request) {
  library(shinycssloaders)
  library(dplyr)
  library(DT)
  library(CEMRA)
  
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      
      # App title ----
      #titlePanel("Covid Exposure Model and Risk App (CEMRA)"),
      headerPanel(
        fluidRow(
          column(11, h1("Covid Exposure Model and Risk App (CEMRA)", 
                        style = "'font-weight': bold; 'font-size': 39px")),
          column(1, offset=-2, img(height = 100, width = 100, src = "www/iomlogo.png"))

        )
        , windowTitle = "Covid Exposure Model and Risk App (CEMRA)"),
      
      
      sidebarLayout(
        sidebarPanel(
          selectInput("SETTING", "Preloaded Setting:",
                      list("Hospital (Single Patient room)" = "Hospital_singlepatient",
                           "Hospital (Two Patient room)"= "Hospital_twopatient",
                           "Hospital (Treatment room)" = "Hospital_singlepatienttreatment",
                           "Office (Meeting room)"="Office_meetings"
                      )),
          fileInput("file1", "Build your own - Setting File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
          #radioButtons("DURATION", "Duration in room:",
          #            list("Short Task (between 3 and 17 minutes)" = "shorttask",
          #                 "Long Task (between 25 and 35 minutes)" = "longtask"
          #               
          #            )),
          selectInput("INFECTED", "Infectiousness:",
                      list("As Specified in the Setting File"="ASISF",
                           "Extremely high" = "EHI",
                           "Very high" = "VHI",
                           "High"="HI",
                           "Moderate" = "MI",
                           "Low"="LI",
                           "Very low" = "VLI",
                           "Extremely low"="ELI",
                           "Unknown"= "Unknown"
                      ), selected="ASISF"),
          selectInput("STAGEOFINFECTION", "Stage of Infection:",
                      list("As Specified in the Setting File"="ASISF",
                        "Pre-peak"="Pre-peak",
                           "Peak" = "Peak",
                           "Around Peak" = "Around Peak",
                           "Post-Peak"="Post-Peak"
                      ), selected="ASISF"),
          selectInput("ENGVAR", "Engineering controls:",
                      list("None" = "None",
                           "Fresh air from small window open 10%"="Freshair",
                           "NIOSH Ventilated headboard" = "VentHead",
                           "Sodeco Mobile Air Purification unit with UVC (UPH/EC-220-F7+F9-CG - 450m³/h)"="UVC"
                      )),
          selectInput("ADMVAR", "Administrative controls:",
                      list("None"="None",
                           "Surface Disinfection (Kurgat et al., 2019)" = "Hygiene",
                           "Surface Disinfection + hand hygiene (Kurgat et al., 2019)" = "Hygiene2"
                      )),
          selectInput("PPEVAR", "PPE controls:",
                      list("None" = "None",
                           "Surgical Mask" = "Surgical Mask",
                           "FFP2" = "FFP2",
                           "FFP3" = "FFP3",
                           "Worksafe AirHood"="Airhood"
                      )),
          sliderInput("simu", "Number of simulations:",
                      min = 0, max = 250,
                      value = 50, step = 1),
          actionButton("button", "Run"),
          downloadButton("downloadData", "Download parameters"),
          
          
          
          
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Model & App", includeMarkdown(app_sys("app/www/info.Rmd"))),
            tabPanel("Preloaded Setting",  includeMarkdown(app_sys("app/www/preloadedscenarios.Rmd"))),
            #tabPanel("Viral load and shedding", includeHTML("data/docs/infectiousness.html")),
            #tabPanel("Controls", includeMarkdown(app_sys("app/www/controls.Rmd"))),
            tabPanel("How to use", includeMarkdown(app_sys("app/www/howtouse.Rmd"))),
            tabPanel("Parameters", DTOutput("params")),
            tabPanel("Risk of Infection", plotOutput("numberinfectedgraph")%>% withSpinner(color="#428bca"), htmlOutput("infectedtextcomparison")),
            tabPanel("Route of transmission", plotOutput("relcon")%>% withSpinner(color="#428bca"), htmlOutput("infectedrelcontext")),
            tabPanel("Acknowledgments", includeMarkdown(app_sys("app/www/acknowledgements.Rmd")))
            )
        )
      )
    )
    
    
    
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CEMRA'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


