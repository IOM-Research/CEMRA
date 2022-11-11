library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)

donotruninapp2<-function(x){

output<-read.csv("data/output/Hospital/Hospital_Paper_Main.csv")

numberinfectedgraph<-function(output, infectiousness, type){
  
  output<-output[(grepl(infectiousness, output$ID)),]
  
  output$ID<-gsub('(.*)_\\w+', '\\1', output$ID)
  output$ID<-gsub('CEMRA_', '', output$ID)
  output$ID<-gsub('ADM_', '', output$ID)
  output$ID<-gsub('BASELINE_', '', output$ID)
  output$ID<-gsub('ENG_', '', output$ID)
  output$ID<-gsub('PPE_', '', output$ID)
  
  output$ID[output$ID=="noint"]<-"No Intervention"
  output$ID[output$ID=="surgicalmask"]<-"Surgical Mask"
  output$ID[output$ID=="surgicalmask_surfacedis"]<-"Surgical mask + Surface disinfection"
  output$ID[output$ID=="surgicalmask_surfacedishandhygiene"]<-"Surgical mask + Surface disinfection + Hand hygiene"
  output$ID[output$ID=="surgicalmask_freshair"]<-"Surgical Mask + Natural ventilation"
  output$ID[output$ID=="surgicalmask_UVC_bcs"]<-"Surgical Mask + UVC air purification (best efficacy)"
  output$ID[output$ID=="surgicalmask_UVC_irl"]<-"Surgical Mask + UVC air purification (sub-optimal efficacy)"
  output$ID[output$ID=="surgicalmask_VentHead"]<-"Surgical Mask + Ventilated Headboard"
  
  output$ID<-factor(output$ID, levels=c("No Intervention",
                                        "Surgical Mask",
                                        "Surgical mask + Surface disinfection",
                                        "Surgical mask + Surface disinfection + Hand hygiene",
                                        "Surgical Mask + Natural ventilation",
                                        "Surgical Mask + UVC air purification (best efficacy)",
                                        "Surgical Mask + UVC air purification (sub-optimal efficacy)",
                                        "Surgical Mask + Ventilated Headboard",
                                        "FFP2",
                                        "FFP3",
                                        "Airhood"))
  
  if(type=="loginfrisk"){
    
    ##### Output 1: Log infection risk 
    output %>% 
      group_by(ID) %>%
      select(ID, rFACE, rLUNGNF, rLUNGFF, rSPRAY) %>%
      mutate_at(., c("rFACE", "rLUNGNF", "rLUNGFF", "rSPRAY"), ~as.numeric(.))%>%
      pivot_longer(!ID, names_to="cat", values_to="value") %>%
      mutate(cat=factor(cat, levels=c("rFACE","rLUNGNF","rLUNGFF","rSPRAY"))) %>%
      mutate(value=log(value))%>%
      ggplot(., aes(x=cat, y=value, colour=ID)) + 
      geom_boxplot()+
      xlab("Route")+
      ylab("Log Infection")+ scale_x_discrete(labels=c("rFACE" = "Contact", "rLUNGNF" = "Inhalation Near Field",
                                                       "rLUNGFF" = "Inhalation Far Field",
                                                       "rSPRAY" = "Cough Spray")) + 
      theme(axis.text.x=element_text(angle=15, hjust=1),
            text = element_text(size = 18),
            legend.position = "none")+
      scale_colour_grey()
  } else if(type=="percentagedecrease"){
    
    test<-output %>% 
      group_by(ID) %>%
      select(ID, rOVERALL) %>%
      mutate_at(., c("rOVERALL"), ~as.numeric(.))%>%
      summarise(medianval=median(rOVERALL))
    
    baseline<-test[1,2]
    
    test <-test %>%
      mutate(percentagechange=round(medianval/as.numeric(baseline)*100))
    
  }else if(type=="predictedairconc"){
    test<-output %>% 
      filter(ID=="No Intervention")
    test
  }
}


# risk by route
b<-numberinfectedgraph(output, "extremelylow", "loginfrisk")
c<-numberinfectedgraph(output, "verylow", "loginfrisk")
d<-numberinfectedgraph(output, "low", "loginfrisk")
e<-numberinfectedgraph(output, "average", "loginfrisk")



library(ggpubr)
# Second row with box and dot plots
ggarrange(b, c,d,e, ncol = 2, nrow=2, labels = c("A","B", "C", "D")  )
}
