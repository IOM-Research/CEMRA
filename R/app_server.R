#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 

 #source("R/COVIDinfectioncalculator.R")
 #source("R/COVIDinfectioncalculatorBATCHnumberinfected.R")
 #source("R/COVIDinfectioncalculatorBATCHrelativecontributions.R")

#devtools::load_all("R/")
library(CEMRA)
  
#################### NEEDS UPDATED WITH CURRENT FILES ###############################  
  
# This function is responsible for loading in the selected file
# more variation in the scenario
  # hospital - single patient room
  # hospital - multi patient room
  # hospital - treatment room
  # office - meeting room
  
filedata <- reactive({
    if (!is.null(input$file1)) {
      infile<-input$file1
      read.csv(infile$datapath)
    } else if(is.null(input$file1) & input$SETTING=="Hospital_singlepatient"){
      infile<-system.file("extdata", "runs/Hospital/Hospital_singlepatient_moderate_peak.csv", package = "CEMRA")
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Hospital_twopatient"){
      infile<-system.file("extdata", "runs/Hospital/Hospital_twopatient_moderate_peak.csv", package = "CEMRA")
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Hospital_singlepatienttreatment"){
      infile<-system.file("extdata", "runs/Hospital/Hospital_singlepatienttreatment_moderate_peak.csv", package = "CEMRA")
      read.csv(infile)
    } else if(is.null(input$file1) & input$SETTING=="Office_meetings"){
      infile<-system.file("extdata", "runs/Office/Office_meetings2.csv", package = "CEMRA")
      read.csv(infile)
    }
  })
  

# Generate the input data

# allow more than one to be selected
modeldata <- reactive({
    
  df <- filedata()
  
  ### Stage of infection
  if(input$STAGEOFINFECTION=="Pre-peak"){
    df$InfStageofInfection<-input$STAGEOFINFECTION
    df$ID<-paste0(df$ID, "\n+ Pre-peak infection")
    df
  } else if(input$STAGEOFINFECTION=="Around peak"){
    df$InfStageofInfection<-input$STAGEOFINFECTION
    df$ID<-paste0(df$ID, "\n+ Around peak infection")
    df
  }else if(input$STAGEOFINFECTION=="Peak"){
    df$InfStageofInfection<-input$STAGEOFINFECTION
    df$ID<-paste0(df$ID, "\n+ Peak infection")
    df
  }else if(input$STAGEOFINFECTION=="Post-peak"){
    df$InfStageofInfection<-input$STAGEOFINFECTION
    df$ID<-paste0(df$ID, "\n+ Post-peak infection")
    df
  } else{
    df$ID<-paste0(df$ID, " ")
    df
  }
  
  
  # if(input$DURATION=="shorttask"){
  #    df$SuTmaxa<-3
  #    df$SuTmaxb<-17
  #    df$SuTmaxc<-13
  #    df
  #    } else if(input$DURATION=="longtask"){
  #      df$SuTmaxa<-25
  #      df$SuTmaxb<-35
  #      df$SuTmaxc<-30
  #      df}else{
  #        df
  #      }
  
  
      # INFECTIOUSNESS
      if(input$INFECTED=="EHI"){
        df$Infcoughrateperhourmax<-70
        df$Infcoughrateperhourmin<-60
        df$Infcoughrateperhourmode<-65
        df$InfsalivaChenscale<-8
        df$InfEairTalkSmean<-7.75
        df$ID<-paste0(df$ID, "\n+ Extremely high infectious")
        df
      } else if(input$INFECTED=="VHI"){
        df$Infcoughrateperhourmax<-60
        df$Infcoughrateperhourmin<-50
        df$Infcoughrateperhourmode<-55
        df$InfsalivaChenscale<-7
        df$InfEairTalkSmean<-7.5
        df$ID<-paste0(df$ID, "\n+ Very high infectious")
        df
      } else if(input$INFECTED=="HI"){
        df$Infcoughrateperhourmax<-50
        df$Infcoughrateperhourmin<-40
        df$Infcoughrateperhourmode<-45
        df$InfsalivaChenscale<-6
        df$InfEairTalkSmean<-7.25
        df$ID<-paste0(df$ID, "\n+ High infectious")
        df
      }else if(input$INFECTED=="MI"){
        df$Infcoughrateperhourmax<-40
        df$Infcoughrateperhourmin<-30
        df$Infcoughrateperhourmode<-35
        df$InfsalivaChenscale<-5
        df$InfEairTalkSmean<-7
        df$ID<-paste0(df$ID, " ")
        df
      } else if(input$INFECTED=="LI"){
        df$Infcoughrateperhourmax<-30
        df$Infcoughrateperhourmin<-20
        df$Infcoughrateperhourmode<-25
        df$InfsalivaChenscale<-4
        df$InfEairTalkSmean<-6.75
        df$ID<-paste0(df$ID, "\n+ Low infectious")
        df
      } else if(input$INFECTED=="VLI"){
        df$Infcoughrateperhourmax<-20
        df$Infcoughrateperhourmin<-10
        df$Infcoughrateperhourmode<-15
        df$InfsalivaChenscale<-3
        df$InfEairTalkSmean<-6.5
        df$ID<-paste0(df$ID, "\n+ Very low infectious")
        df
      }else if(input$INFECTED=="ELI"){
        df$Infcoughrateperhourmax<-10
        df$Infcoughrateperhourmin<-1
        df$Infcoughrateperhourmode<-5
        df$InfsalivaChenscale<-2
        df$InfEairTalkSmean<-6.25
        df$ID<-paste0(df$ID, "\n+ Extremely low infectious")
        df
      }else if(input$INFECTED=="Unknown"){
        df$Infcoughrateperhourmax<-60
        df$Infcoughrateperhourmin<-0
        df$Infcoughrateperhourmode<-30
        df$InfsalivaChenscale<-7.01
        df$InfEairTalkSmean<-6
        df$ID<-paste0(df$ID, "\n+ Unknown infectious")
        df
      }else{
        df$ID<-paste0(df$ID, " ")
        df
      }

    # ENGINEERING CONTROLS
      if(input$ENGVAR=="UVC"){
      df$RoomUVCpurificationinroom<-"Y"
      df$RoomUVCmaxflowrate<-450
      df$RoomUVCeffmin<-0.9
      df$RoomUVCeffmax<-1
      df$ID<-paste0(df$ID, "\n+ UVC air purification")
      df
    } else if (input$ENGVAR=="Freshair"){
      df$Roomwindowsopen<-"Y"
      df$Roomwindspeedmin<-1
      df$Roomwindspeedmax<-4
      df$RoomsoaW<-0.8
      df$RoomsoaH<-1
      df$RoomsoaP<-0.1
      df$ID<-paste0(df$ID, "\n+ Fresh air from small window open 10%")
      df
    } else if (input$ENGVAR=="VentHead"){
      df$InfCexhaleprobmin<-0.06
      df$InfCexhaleprobmax<-0.27
      df$InfCexhaleprobmode<-0.17
      df$ID<-paste0(df$ID, "\n+ Ventilated Headboard")
      df
    } else{
      df
    }
    
    # ADMINISTRATIVE CONTROLS 
  
    if(input$ADMVAR=="Hygiene"){
      df$SuCfomiteprobmin<-0.38
      df$SuCfomiteprobmax<-0.86
      df$SuCfomiteprobmode<-0.583
      df$ID<-paste0(df$ID, "\n + Surface Disinfection")
      df
    } else if(input$ADMVAR=="Hygiene2"){
      df$SuCfomiteprobmin<-0
      df$SuCfomiteprobmax<-0.47
      df$SuCfomiteprobmode<-0.146
      df$ID<-paste0(df$ID, "\n+ Surface Disinfection and hand hygiene")
      df
    }
  
    # PPE CONTROLS
    if(input$PPEVAR=="Surgical Mask"){
      df$SuCinhaleprobmin<-0.2
      df$SuCinhaleprobmax<-0.65
      df$SuCinhaleprobmode<-0.35
      df$SuCSPRAYprobmin<-0.05
      df$SuCSPRAYprobmax<-0.05
      df$SuCSPRAYprobmode<-0.05
      df$ID<-paste0(df$ID, "\n+ Surgical Mask")
      df
    } else if(input$PPEVAR=="FFP2"){
      df$SuCinhaleprobmin<-0.01
      df$SuCinhaleprobmax<-0.35
      df$SuCinhaleprobmode<-0.1
      df$SuCSPRAYprobmin<-0.05
      df$SuCSPRAYprobmax<-0.05
      df$SuCSPRAYprobmode<-0.05
      df$ID<-paste0(df$ID, "\n+ FFP2")
      df  
    } else if(input$PPEVAR=="FFP3"){
      df$SuCinhaleprobmin<-0.005
      df$SuCinhaleprobmax<-0.3
      df$SuCinhaleprobmode<-0.05
      df$SuCSPRAYprobmin<-0.05
      df$SuCSPRAYprobmax<-0.05
      df$SuCSPRAYprobmode<-0.05
      df$ID<-paste0(df$ID, "\n+ FFP3")
      df  
    } else if(input$PPEVAR=="Airhood"){
      df$SuCinhaleprobmin<-0.0003
      df$SuCinhaleprobmax<-0.05
      df$SuCinhaleprobmode<-0.01
      df$SuChandtouchmin<-0	
      df$SuChandtouchmax<-0
      df$SuChandtouchmode<-0
      df$SuCSPRAYprobmin<-0
      df$SuCSPRAYprobmax<-0
      df$SuCSPRAYprobmode<-0
      df$ID<-paste0(df$ID, "\n+ Airhood")
      df
    } else{
      df
    }
    
  })


baselinedata <- reactive({
    df <- filedata()
    df
})

  
# Create the params table for output

# change the metadata based on pre-loaded scenario - if user loaded then no refs??


paramdata <- reactive({
  df  <- modeldata()
  df2 <- tidyr::gather(df, key="Parameter")
  data(metadata)
  df2 <- merge(df2, metadata, by="Parameter", all=T)
  df2 <- with(df2,df2[order(ID) , ])
  df2 <- subset(df2, select = -c(ID) )
})
paramdata2 <- reactive({
  df  <- modeldata()
})


output$downloadData <- downloadHandler(
  filename = function() {
    paste("CEMRAparams.csv", sep = "")
  },
  content = function(file) {
    write.csv(paramdata2(), file, row.names = FALSE)
  }
)

# run the model on the "button"

masteroutput <-eventReactive(input$button, {
  modeldata <- modeldata()
  baselinedata  <-baselinedata()
  
  # Specify how many iterations
  RUN<-do.call("rbind", replicate(input$simu, modeldata, simplify = FALSE))
  RUN2<-do.call("rbind", replicate(input$simu, baselinedata, simplify = FALSE))
  RUN3<-rbind(RUN, RUN2)
  
  # Run the function
  masteroutput<-plyr::mdply(RUN3, COVIDinfectioncalculator)
})

#
output$params <- renderDT({
  datatable(paramdata(), options = list(pageLength = 10, lengthChange = FALSE), rownames= FALSE)
  })

# Generate number of infected plot
output$numberinfectedgraph <- renderPlot({

    masteroutput <- masteroutput()
    masteroutput$numberinfected<-as.numeric(masteroutput$numberinfected)
    masteroutput <- masteroutput %>% select(ID,numberinfected)
  
    library(ggplot2)
    d<-ggplot(masteroutput, aes(x=ID, y=numberinfected))+
      geom_violin()+
      facet_wrap(~ID, scales="free_x")+
      theme(
        axis.title.x=element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank())+
      theme(text = element_text(size=12),
            strip.text = element_text(size=15))
    d<-d + scale_y_continuous(trans='log10')+
      ylab("Log risk per single exposure event")
    d
  })

# Generate number of infected text
output$infectedtextcomparison <- renderText({
 
  masteroutput <- masteroutput()
  masteroutput$numberinfected<-as.numeric(masteroutput$numberinfected)
  masteroutput <- masteroutput %>% select(ID,numberinfected)
  
  masteroutput<-masteroutput %>%
    group_by(ID) %>%
    summarise(mediannumberinfected=median(numberinfected, na.rm=T))
  
  
  # comparison
  masteroutput2 <- masteroutput()
  masteroutput2$numberinfected<-as.numeric(masteroutput2$numberinfected)
  masteroutput2 <- masteroutput2 %>% select(ID,numberinfected)
  
  masteroutput2<-masteroutput2 %>%
    group_by(ID) %>%
    summarise(mediannumberinfected=median(numberinfected, na.rm=T))
  
  
  scenariorisk<-(round(masteroutput$mediannumberinfected[1]*100000,2))
  
  scenario2risk<-(round(masteroutput2$mediannumberinfected[2]*100000,2))
  changeperc<-round(100-
                      ((masteroutput$mediannumberinfected[1]*100000)/
                           (masteroutput2$mediannumberinfected[2]*100000)
                              *100),2)
  changetext<-NA
  changetext[changeperc>0]<-"increase"
  changetext[changeperc==0]<-"difference"
  changetext[changeperc<0]<-"reduction"
  
  
  paste0("The median number of infected people for the scenario: </br><ul><li>",masteroutput$ID[1], " is ",
        "<font color=\"#FF0000\"><b>", scenariorisk,"</b></font>" , " per 100,000 exposure events","</li>",
        "<li>",masteroutput2$ID[2]," is ", 
        "<font color=\"#FF0000\"><b>", scenario2risk, "</b></font>", " per 100,000 exposure events","</li>")
})

# Generate relcon plot
output$relcon <- renderPlot({
  
  # to get 100%
  round_percent <- function(x) { 
    x <- x/sum(x)*100  # Standardize result
    res <- floor(x)    # Find integer bits
    rsum <- sum(res)   # Find out how much we are missing
    if(rsum<100) { 
      # Distribute points based on remainders and a random tie breaker
      o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
      res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
    } 
    res 
  }
  
  
  masteroutput <- masteroutput()
  
  masteroutput<-masteroutput %>% 
    group_by(ID) %>%
    select(ID, rFACE, rLUNGNF, rLUNGFF, rSPRAY) %>%
    mutate_at(., c("rFACE", "rLUNGNF", "rLUNGFF", "rSPRAY"), ~as.numeric(.)) %>%
    summarise(CONTACT_mean =      mean(rFACE  /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_NF_mean = mean(rLUNGNF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_FF_mean = mean(rLUNGFF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              SPRAY_mean =        mean(rSPRAY /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100))
  colnames(masteroutput)[2:5]<-c("Contact", "Inhalation (NF)", "Inhalation (FF)", "Spray")
  
  
  masteroutput <- tidyr::pivot_longer(masteroutput,cols=2:5)
  
  # make sure this works
  masteroutput<- masteroutput %>% group_by(ID) %>%
    mutate(value=round_percent(value))
  
  
  library(hrbrthemes)
  library(waffle)
  library(ggplot2)
  library(dplyr)
  
  group.colors <- c("Contact" = "#E69F00", "Spray" = "#CC79A7", `Inhalation (NF)`= "#56B4E9", `Inhalation (FF)`="#0072B2")
  
  masteroutput %>%
    ggplot(aes(fill = name, values = value)) +
    # this isn't working
    scale_color_manual(
      values = group.colors,
      aesthetics = c("fill")
    )+
    ####################
    expand_limits(x=c(0,0), y=c(0,0)) +
    coord_equal() +
    labs(fill = NULL, colour = NULL) +
    theme_ipsum_rc(grid="") +
    theme_enhance_waffle() -> waffleplot
  
  waffleplot +
    geom_waffle(
      color = "white", size = 0.33
    ) +
    facet_wrap(~ID) +
    theme(strip.text.x = element_text(hjust = 0.5, size=15))
  
})

# Generate rel contr text
output$infectedrelcontext<- renderText({
  
  masteroutput <- masteroutput()
  
  masteroutput<-masteroutput %>% 
    group_by(ID) %>%
    select(ID, rFACE, rLUNGNF, rLUNGFF, rSPRAY) %>%
    mutate_at(., c("rFACE", "rLUNGNF", "rLUNGFF", "rSPRAY"), ~as.numeric(.)) %>%
    summarise(CONTACT_mean =      mean(rFACE  /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_NF_mean = mean(rLUNGNF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              INHALATION_FF_mean = mean(rLUNGFF/(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100),
              SPRAY_mean =        mean(rSPRAY /(rLUNGNF+rLUNGFF+rFACE+rSPRAY)*100))
  colnames(masteroutput)[2:5]<-c("Contact", "Inhalation (NF)", "Inhalation (FF)", "Spray")
  
  masteroutput<-masteroutput %>% tidyr::pivot_longer(cols=2:5, names_to="route", values_to="risk")
  # make sure this works
  masteroutput$value<- round_percent(masteroutput$risk)
  
  
  masteroutput2<-masteroutput[1:4,1:3]
  
  domroute<-with(masteroutput2, route[which.max(risk)])
  domroute<-gsub("_mean", "",domroute)
  domperc<-round(max(masteroutput2$risk),0)
  scenario2<-with(masteroutput2, ID[which.max(risk)])
  
  
  masteroutput3<-masteroutput[5:8,1:3]
  domroute2<-with(masteroutput3, route[which.max(risk)])
  domroute2<-gsub("_mean", "",domroute2)
  domperc2<-round(max(masteroutput3$risk),0)
  scenario3<-with(masteroutput3, ID[which.max(risk)])
  
  
  paste0("The dominant route for the scenario is:</br>","<ul><li>",
         "<font color=\"#FF0000\"><b>", domroute,"</b></font>", 
         " contributing ", 
         "<font color=\"#FF0000\"><b>",domperc,"%","</b></font>",
         " to the total risk ","in ", scenario2 ,"</li>",
         
         "<li>",
         "<font color=\"#FF0000\"><b>", domroute2,"</b></font>",
         " contributing ", 
         "<font color=\"#FF0000\"><b>",domperc2,"%","</b></font>",
         " to the total risk", " in ",scenario3, "</li>"
         )

})


  
  
  
}
