# compiler to speed up code
library(compiler)
COVIDinfectioncalculator<-cmpfun(
# The function to compute infection risk and route of transmission
COVIDinfectioncalculator<- function(ID,dt,DRk,ExtraExpVolStudy,Vts, gflow, gfhigh,distsalivavirusconc,SpeakontoSurf=NA,
                                    Roomheight,RoomairflowNFFF,Roomvolumemin,Roomvolumemax,
                                    RoomACHmin,RoomACHmax,Roomwindowsopen, 
                                    RoomUVCpurificationinroom,RoomUVCmaxflowrate,	RoomUVCeffmin,	RoomUVCeffmax,
                                    Roomwindspeedmin,Roomwindspeedmax,RoomsoaW,RoomsoaH,RoomsoaP,
                                    RoomNFw, RoomNFh, RoomNFd,
                                    InfStageofInfection, Infected, Infcoughrateperhourmin, Infnonsilenttime,
                                    Infcoughrateperhourmax,Infcoughrateperhourmode, InfCsprayprobmin, InfCsprayprobmax, InfCsprayprobmode,
                                    InfCexhaleprobmin,InfCexhaleprobmax,InfCexhaleprobmode,InfsurfacesNF, InfsurfacesFF, Infactivity, Infsalivastudy, InfsalivaChenshape, InfsalivaChenscale, InfsalivaIwasakimin, InfsalivaIwasakimax,
                                    InfEairTalkSmean,InfEairTalkSsd,Sufinger,Suface,Sueye,
                                    SuFFtimemin,SuFFtimemax,SuTmaxa,SuTmaxb,SuTmaxc,
                                    INACTIVaira,INACTIVairb,INACTIVairc,INACTIVsurfacea,INACTIVsurfaceb,
                                    INACTIVsurfacec,INACTIVskinmean,INACTIVskinsd,TRANSsurface.skinshape,TRANSsurface.skinscale,TRANSskin.abs=NA,
                                    CONTACTsurfaceNF.handa,CONTACTsurfaceNF.handb,CONTACTsurfaceNF.handc,CONTACTsurfaceFF.handa,CONTACTsurfaceFF.handb,
                                    CONTACTsurfaceFF.handc, CONTACTface.handsize,CONTACTface.handmu,SuTARGETmin,SuTARGETmax,
                                    SuCeyeprob,SuCSPRAYprobmin, SuCSPRAYprobmax,SuCSPRAYprobmode,SuCinhaleprobmin,
                                    SuCinhaleprobmax, SuCinhaleprobmode,SuChandtouchmin, SuChandtouchmax, SuChandtouchmode, 
                                    SuCfomiteprobmin, SuCfomiteprobmax, SuCfomiteprobmode,SuSPRAYprob, Su){ 
  
  
  #############################################################################################################################################
  # STAGE 1: SET UP THE VARIABLES
  #############################################################################################################################################
  
  ################### DEFINE Room VARIABLES ########################
  
  # room volume, m^3
  V<-runif(1, min=Roomvolumemin, max=Roomvolumemax)
  # W is the the width of the room
  W<-sqrt(V/Roomheight)
  # 1m is the NF distance; 3.7*2.4*3
  Vn<-RoomNFw*RoomNFh*RoomNFd
  # what is the percentage of the NF to the rest of the room?
  NFsizeperc=Vn/V
  # Create the betaNF variable
  betaNF<-NFsizeperc*Roomheight*W*RoomairflowNFFF
  # Air changes per hour
  ACH<-runif(1, min=RoomACHmin, max=RoomACHmax)
  # if windows open; recalculate the ACH rule of thumb equation from p40 here: https://www.who.int/water_sanitation_health/publications/natural_ventilation.pdf
  if(Roomwindowsopen=="Y"){
  ACH<-(0.65*(runif(1, Roomwindspeedmin, Roomwindspeedmax)*(RoomsoaW*RoomsoaH*RoomsoaP)*3600))/V
  } else{
  ACH<-ACH
  }
  # if UVC unit it the room then recalculate the ACH based on manufacturers specifications
  if(RoomUVCpurificationinroom=="Y"){
    ACH<-ACH+((RoomUVCmaxflowrate/V)*runif(1,RoomUVCeffmin, RoomUVCeffmax))
  } else{
    ACH<-ACH
  }
  
  ################### DEFINE BEHAVIOUR VARIABLES ########################
  
  # pSPRAY is the probability that the worker intercepts a droplet spray event
  pSPRAY<-rep(SuSPRAYprob,1)
  # Time spent in FF (in 10th of percentage)
  values<-seq(SuFFtimemin, SuFFtimemax, by=10)
  # Randomly use one of the values for time spend in the FF
  FFtime<-sample(values, 1)
  
  ### Tmax is the duration of the exposure period, minutes (Phan et al. (2019))
  #library(triangle) 
  Tmax<-triangle::rtriangle(1, a=SuTmaxa, b=SuTmaxb, c=SuTmaxc)

  ################### EMISSION VARIABLES ########################
   
  # Stage of Infection - here we are getting a weighting that will be applied to the emission variables
  # This is based on the Human Challenge Study data on the 'decay' in viral load over time from the PROTECT project
  # The stages correspond to time periods and then we've applied a distribution to possible values within that range
  if(InfStageofInfection=="Pre-peak"){
    InfStageofInfectionvalue<-rlogis(1, 0.62, 0.16)
    InfStageofInfectionvalue<-ifelse(InfStageofInfectionvalue>1, 1,InfStageofInfectionvalue)
    InfStageofInfectionvalue<-ifelse(InfStageofInfectionvalue<0, 0.62,InfStageofInfectionvalue)
  } else if (InfStageofInfection=="Around peak"){
    InfStageofInfectionvalue<-rcauchy(1, 0.92, 0.07)
    InfStageofInfectionvalue<-ifelse(InfStageofInfectionvalue>1, 1,InfStageofInfectionvalue)
    InfStageofInfectionvalue<-ifelse(InfStageofInfectionvalue<0, 0.92,InfStageofInfectionvalue)
  } else if (InfStageofInfection=="Peak"){
    InfStageofInfectionvalue<-1
  } else if (InfStageofInfection=="Post-peak"){
    InfStageofInfectionvalue<-rlogis(1, 0.67, 0.14)
    InfStageofInfectionvalue<-ifelse(InfStageofInfectionvalue>1, 1,InfStageofInfectionvalue)
    InfStageofInfectionvalue<-ifelse(InfStageofInfectionvalue<0, 0.67,InfStageofInfectionvalue)
  } else{
    InfStageofInfectionvalue<-1
  }
  
  # Cough rate
  Infcoughrateperhour<-triangle::rtriangle(1, a=Infcoughrateperhourmin, b=Infcoughrateperhourmax, c=Infcoughrateperhourmode)
  
  # Number of people in FF that are infected
  FFinfected<-Infected-1
  
  # COUGHrate is the rate of cough, unit is per minute, after dividing by 60
  # multiple by any controls on the coughs (i.e. mask)
  COUGHrate<-(Infcoughrateperhour/60)*(triangle::rtriangle(a=InfCsprayprobmin,b=InfCsprayprobmax, c=InfCsprayprobmode))*InfStageofInfectionvalue
  
  # CONCsaliva is the concentration of SARS-CoV-2 in saliva
  # unit is log10 infectious copies per mL
  # take to the power 10 to make units infectious virus per mL
  # there are two ways to get this variable either from the Chen or Iwasaki study
  if(Infsalivastudy=="Chen"){
  CONCsaliva<-(10^rweibull(1, shape=InfsalivaChenshape, scale=InfsalivaChenscale))
  CONCsaliva<-CONCsaliva*InfStageofInfectionvalue
  } else if(Infsalivastudy=="Iwasaki"){
  CONCsaliva<-(10^runif(1, min=InfsalivaIwasakimin, max=InfsalivaIwasakimax))
  }
  # The number of coughs during the exposure event
  Ncough<-round(Tmax*COUGHrate)
  # Adjust gene copies (unit of emission) to PFU (unit of dose-response)
  gf<-runif(1, gflow, gfhigh)	
  
  # We've assumed a constant virus emission rate, but that's not going to happen if the person is doing something other than resting-speaking
  # So we apply a behaviour modification factor which increases or decreases the emission rate based on behaviour
  # e.g. resting-breathing is 20% of the emission rate of resting-speaking
  # e.g. heavyexercise-speakingloudly is 43 times the emission rate of resting-speaking
  #	Based on figures from Buonnano et al. (2020a, b)
  
  if(Infactivity=="resting-breathing"){
    quantaexhalationrate=2/9.4
  } else if(Infactivity=="resting-speaking"){
    quantaexhalationrate=9.4/9.4
  } else if(Infactivity=="resting-speakingloudly"){
    quantaexhalationrate=60.5/9.4
  } else if(Infactivity=="standing-breathing"){
    quantaexhalationrate=2.3/9.4
  } else if(Infactivity=="standing-speaking"){
    quantaexhalationrate=11.4/9.4
  } else if(Infactivity=="standing-speakingloudly"){
    quantaexhalationrate=65.1/9.4
  } else if(Infactivity=="lightexercise-breathing"){
    quantaexhalationrate=5.6/9.4
  } else if(Infactivity=="lightexercise-speaking"){
    quantaexhalationrate=26.3/9.4
  } else if(Infactivity=="lightexercise-speakingloudly"){
    quantaexhalationrate=170/9.4
  } else if(Infactivity=="heavyexercise-breathing"){
    quantaexhalationrate=13.5/9.4
  } else if(Infactivity=="heavyexercise-speaking"){
    quantaexhalationrate=63.1/9.4
  } else if(Infactivity=="heavyexercise-speakingloudly"){
    quantaexhalationrate=408/9.4
  } else if(Infactivity=="singing"){
    quantaexhalationrate=970/9.4
  }
  
  # Virus emitted gene copies per minute with breathing/talking (distribution based on leung, Zhou and Ma)
  InfEairTalkS<-rlnorm(1, meanlog=InfEairTalkSmean, sdlog=InfEairTalkSsd)
  # This is where we apply the behaviour modification factor
  InfEairTalkSQA<-InfEairTalkS*quantaexhalationrate
  # This is where we apply the infection stage weighting
  InfEairTalkSQA<-InfEairTalkS*InfStageofInfectionvalue
  # Apply the emission rate control (e.g. ventilated headboard)
  EairTalkS<-InfEairTalkSQA*triangle::rtriangle(n=1, a=InfCexhaleprobmin, b=InfCexhaleprobmax, c=InfCexhaleprobmode)
  #Emission per minute from breathing/talking
  EairTalk<-EairTalkS

  ################### INACTIVATION VARIABLES #################
  
  # INACTIVair is the inactivation rate SARS-2 (Van Doremanlen, 2020) 
  # units per minute after dividing by 60
  INACTIVair<-triangle::rtriangle(n=1, a=INACTIVaira, b=INACTIVairb, c=INACTIVairc)/60
  # INACTIVsurface is the invactivation rate of SARS-2 on plastic (Van Doremanlen, 2020)
  # units per minute after dividing by 60
  INACTIVsurface<-triangle::rtriangle(n=1, a=INACTIVsurfacea, b=INACTIVsurfaceb, c=INACTIVsurfacec)/60
  # INACTIVskin is the inactvation rate of influenza on skin, units per minute
  INACTIVskin<-rnorm(1,mean=INACTIVskinmean, sd=INACTIVskinsd)/60
  # ensure the the normal distribution always returns a positive value
  while (length(INACTIVskin[INACTIVskin<=0])>=1){
    n<-length(INACTIVskin[INACTIVskin<=0])
    INACTIVskin[INACTIVskin<=0]<-rnorm(n,mean=INACTIVskinmean, sd=INACTIVskinsd)/60
  }
  
  ################### CONTACT TRANSFER VARIABLES #################
  
  # Calculate the susceptible surface area of eyes and face
  Aportals<-Suface+Sueye
  # TRANSsurface.skin is the effectiveness of transfer between  substrates and skin
  # unit is proportion (0,1]
  TRANSsurface.skin<-rweibull(1,TRANSsurface.skinshape, TRANSsurface.skinscale)
  # ensure that Webiull distribution returns a valuein the range of (0,1]
  while (length(TRANSsurface.skin[TRANSsurface.skin>1])>=1){
    n<-length(TRANSsurface.skin[TRANSsurface.skin>1])
    TRANSsurface.skin[TRANSsurface.skin>1]<-rweibull(n,TRANSsurface.skinshape, TRANSsurface.skinscale)
  }
  
  # TRANSskin is the effectiveness of transfer between two skin surfaces
  # unit is proprotion (0-1)
  if(is.na(TRANSskin.abs)){
  TRANSskin<-TRANSsurface.skin
  }else{
  TRANSskin<-TRANSskin.abs 
  }
  
  # CONTACTsurfaceNF.hand is the frequency of contact between hands and surfaces in the near-field 
  # from Phan et al. (2019), 
  # unit is touch per minute after dividing by 60
  CONTACTsurfaceNF.hand<-triangle::rtriangle(n=1, a=CONTACTsurfaceNF.handa, b=CONTACTsurfaceNF.handb, c=CONTACTsurfaceNF.handc)/60
  # CONTACTsurfaceFF.hand is the frequency of contact between hands and surfaces in the far-field 
  # from Phan et al. (2019) 
  # unit is touch per minute 
  CONTACTsurfaceFF.hand<-triangle::rtriangle(n=1, a=CONTACTsurfaceFF.handa, b=CONTACTsurfaceFF.handb, c=CONTACTsurfaceFF.handc)/60
  # CONTACTface.hand is the frequency of contact between hands and facial mucous membranes of worker
  # data is distribution of the number of contacts with mask observed by Phan et al. (2019)
  # divide by duration of exposure to get contact rate
  CONTACTface.hand<-(rnbinom(1, size=CONTACTface.handsize, mu=CONTACTface.handmu)*triangle::rtriangle(1, a=SuChandtouchmin, b=SuChandtouchmax, c=SuChandtouchmode))/Tmax
  # pTARGET is the proportion of particles that deposit on the facial musous membranes which reach receptors in the respiratory tract
  pTARGET<-runif(1,min=SuTARGETmin, max=SuTARGETmax)
  
  ################### SIZE AND COUNT DISTRIBUTION OF PARTICLES, BASED ON CHAO (2009) VARIABLES ########################
  
  # create the matrix that will hold the speaking data
  ChaoSpeak<-matrix(0, nrow=16, ncol=7)
  #column 1 is the lower end of the size range at 10 mm, Table 1
  # column 2 is the upper end of the size range at 10 mm, Table 1
  # column 3 is the average number of particles per person at 10 mm (50 coughs), Table 1
  # column 4 is the standard deviation of the particles per person at 10 mm (50 coughs), Table
  # column 5 is the mean particle volume in the size range
  # column 6 is the coefficient of variation
  # column 7 is the estimated number based on Duguid (Table 4)
  ChaoSpeak[1,1:4]<-c(2,4,  1.7, 1.62)
  ChaoSpeak[2,1:4]<-c(4,8,  26.8, 8.94)
  ChaoSpeak[3,1:4]<-c(8,16, 9.2, 4.67)
  ChaoSpeak[4,1:4]<-c(16,24, 4.8, 4.07)
  ChaoSpeak[5,1:4]<-c(24,32, 3.2, 2.36)
  ChaoSpeak[6,1:4]<-c(32,40, 1.6, 1.03)
  ChaoSpeak[7,1:4]<-c(40,50, 1.7, 0.90)
  ChaoSpeak[8,1:4]<-c(50,75, 1.8, 0.98)
  ChaoSpeak[9,1:4]<-c(75,100, 1.3, 0.65)
  ChaoSpeak[10,1:4]<-c(100,125, 1.7, 1.01)
  ChaoSpeak[11,1:4]<-c(125,150, 1.6, 1.03)
  ChaoSpeak[12,1:4]<-c(150,200, 1.7, 1.01)
  ChaoSpeak[13,1:4]<-c(200, 250, 1.5, 0.82)
  ChaoSpeak[14,1:4]<-c(250,500, 1.4, 0.50)
  ChaoSpeak[15,1:4]<-c(500,1000, 0.5, 0.82)
  ChaoSpeak[16,1:4]<-c(1000,2000, 0, 0)
  ChaoSpeak[,5]<-((4/3)*pi*(ChaoSpeak[,2]/2)^3+(4/3)*pi*(ChaoSpeak[,1]/2)^3)/2
  ChaoSpeak[1:15,6]<-ChaoSpeak[1:15,4]/ChaoSpeak[1:15,3]
  # here we are changing the study in which we derive the data on the distribution of the number of particles!
  if(ExtraExpVolStudy=="Duguid"){
    ChaoSpeak[,7]<-c(3, 50, 17, 9, 6, 3, 3, 3, 2,3,3,3,3,3,1,0)
  } else if (ExtraExpVolStudy=="LoudenandRoberts"){
    ChaoSpeak[,7]<-c(191,2972,1018,534,353,181,191,201,141,191,181,191,161,151,60,0)
   }else if(ExtraExpVolStudy=="Zhu"){
    ChaoSpeak[,7]<-c(3, 50, 17, 9, 6, 3, 3, 3, 2,3,3,3,3,3,1,0)
  # Makes quite a difference between Duguid/Zhu compared with Louden and Roberts
  }
  
  # create the matrix that will hold the coughing data
  ChaoCough<-matrix(0, nrow=16, ncol=7)
  #column 1 is the lower end of the size range at 10 mm, Table 1
  # column 2 is the upper end of the size range at 10 mm, Table 1
  # column 3 is the average number of particles per person at 10 mm (50 coughs), Table 1
  # column 4 is the standard deviation of the particles per person at 10 mm (50 coughs), Table
  # column 5 is the mean particle volume in the size range
  # column 6 is the coefficient of variation
  # column 7 is the estimated number based on Duguid (Table 4)
  ChaoCough[1,1:4]<-c(2,4,4.0, 3.46)
  ChaoCough[2,1:4]<-c(4,8, 55.0, 15.88)
  ChaoCough[3,1:4]<-c(8,16, 20.4, 15.44)
  ChaoCough[4,1:4]<-c(16,24, 6.7, 4.60)
  ChaoCough[5,1:4]<-c(24, 32, 2.5, 2.42)
  ChaoCough[6,1:4]<-c(32,40, 2.4, 2.37)
  ChaoCough[7,1:4]<-c(40,50, 2.0, 2.67)
  ChaoCough[8,1:4]<-c(50,75, 2.0, 1.41)
  ChaoCough[9,1:4]<-c(75,100, 1.4, 1.84)
  ChaoCough[10,1:4]<-c(100,125, 1.7, 1.77)
  ChaoCough[11,1:4]<-c(125,150, 1.6, 1.84)
  ChaoCough[12,1:4]<-c(150,200, 4.4, 2.80)
  ChaoCough[13,1:4]<-c(200, 250, 2.5, 1.84)
  ChaoCough[14,1:4]<-c(250,500, 2.1, 1.20)
  ChaoCough[15,1:4]<-c(500,1000, 1.4, 0.97)
  ChaoCough[16,1:4]<-c(1000,2000, 0, 0)
  ChaoCough[,5]<-((4/3)*pi*(ChaoCough[,2]/2)^3+(4/3)*pi*(ChaoCough[,1]/2)^3)/2
  ChaoCough[1:15,6]<-ChaoCough[1:15,4]/ChaoCough[1:15,3]
  if(ExtraExpVolStudy=="Duguid"){
  ChaoCough[,7]<-c(76, 1041, 386, 127, 47, 45, 38, 38, 27,32,30,83,47,40,27,0)
  } else if (ExtraExpVolStudy=="LoudenandRoberts"){
  ChaoCough[,7]<-c(39, 542, 201, 66,  25, 24, 20, 20, 14, 17,16,43,25,21,14,0)
  } else if(ExtraExpVolStudy=="Zhu"){
  ChaoCough[,7]<-c(67,924, 343, 113, 42, 40, 34, 34, 24, 29,27,74,42,35,24,0)
  # Makes quite a difference between Duguid compared with Louden and Roberts/Zhu
  }
  # Distribution of virus concentration across the 16 size bins   
  # 1 = same concentration in all bins, equal to concentration in the saliva
  if(distsalivavirusconc=="equal"){
  Dist.saliva<-rep(1,16)
  } else if(distsalivavirusconc=="lowernonresp"){
  Dist.saliva<-c(rep(1,3), rep(0.5,13))
  }

  #############################################################################################################################################
  # STAGE 2: SET UP THE CONTROLS
  #############################################################################################################################################
  
  # amount of virus that reaches the eyes
  Feye<-rep(SuCeyeprob,1)
  # amount of spray material that reaches facial mucous membranes
  Fspray<-rep(triangle::rtriangle(1, a=SuCSPRAYprobmin, b=SuCSPRAYprobmax, c=SuCSPRAYprobmode),1)
  # amount of airborne material that is inhaled 
  Finhale<-rep(triangle::rtriangle(1, a=SuCinhaleprobmin, b=SuCinhaleprobmax, c=SuCinhaleprobmode),1)
  # amount of fomite contact dose
  Ffomite<-rep(triangle::rtriangle(1, a=SuCfomiteprobmin, b=SuCfomiteprobmax, c=SuCfomiteprobmode),1)
  
  #############################################################################################################################################
  # STAGE 3: SET UP THE MARKOV CHAIN SIMULATION
  #############################################################################################################################################
  
  # The model has nine states in which the infectious agent may be located
  # 1 - room air in the near-field, near the infectious person
  # 2 - surfaces near the infectious person (touched by worker)
  # 3 - other surfaces in far-field (touched by worker)
  # 4 - the hands (fingertips) of the worker
  # 5 - the facial mucous membranes of the worker
  # 6 - the lower respiratory tract of the worker (inhale in near-field)
  # 7 - non-infectious virus (loss of viability)
  # 8 - exhausted from the room via ventilation (air exchange rate)
  # 9 - room air the far-field, away from the infectious person
  #10 - the lower respriatory tract of worker (inhale in far-field)
  
  # place holder variables for results
  EairCough<-rep(0,1)
  EsurfCough<-rep(0,1)
  rateEair<-rep(0,1)
  rateEsurface<-rep(0,1)
  result<-matrix(0, nrow=1, ncol=8)
  Nair<-rep(0,1)
  Nsurface<-rep(0,1)
  
  ## Virus removal from air by ventilation 
  # mechanical ventilation rooms virus from the near-field and far-field zones of air
  # divide by 60 to convert units from per hour to per minute
  lambda18<-ACH/60
  lambda98<-ACH/60
  
  ## Virus movement between two zones in air  
  # airflow is equal from near-field to far-field and far-field to near-field, 
  # to ensure no pressure difference
  lambda19<-betaNF/Vn
  lambda91<-betaNF/Vn
  
  # Virus loss by inactivation (die-off) 
  lambda27<-INACTIVsurface
  lambda37<-INACTIVsurface
  lambda47<-INACTIVskin
  lambda17<-INACTIVair
  lambda97<-INACTIVair
  
  ## Transfer rates with efficiency 
  # exchange between near-field surface and hand, include area ratio 
  # hand is smaller than surface 
  lambda24<-(Sufinger/InfsurfacesNF)*TRANSsurface.skin*CONTACTsurfaceNF.hand
  # exchange between hand and near-field surface 
  lambda42<-TRANSsurface.skin*CONTACTsurfaceNF.hand
  # exchange between one finger (divide Sufinger by five) and facial mucous membranes 
  lambda45<-(Sufinger/Aportals)*CONTACTface.hand*TRANSskin
  # exchange between far-field surface and hand
  lambda34<-(Sufinger/InfsurfacesFF)*TRANSsurface.skin*CONTACTsurfaceFF.hand	
  # exchange between hand and far-field surface
  lambda43<-TRANSsurface.skin*CONTACTsurfaceFF.hand
  
  ## inhalation rate 
  # breathing rate 0.020 m^3 per minute
  # fraction ofroom volume inhaled 
  #near-field-zone
  lambda16<-0.020/Vn
  #far-field zone
  lambda910<-0.020/(V-Vn)
  
  ## Virus deposition from air onto surfaces 
  # m/s, Roomheight = 3 m
  # deposition from air in near-field to touched surfaces in the near-field
  lambda12<-(Vts/Roomheight*60)*(InfsurfacesNF/(0.5*W*W*100*100))
  # deposition fom air in far-field to touched surfaces in the far-field
  lambda93<-(Vts/Roomheight*60)*(InfsurfacesFF/(0.5*W*W*100*100))
  
  ## Virus resuspension from surfaces to air
  lambda21<-0   
  lambda39<-0    
  
  ################### NEAR FIELD TRANSITITION MATRIX ###################
  # with inhalation in the near-field zone, no contact with far-field zones
  
  #calculate the total first-order rate constants for leaving the zone of interest
  lambda10<-lambda12+lambda16+lambda17+lambda18+lambda19   
  lambda20<-lambda24+lambda27+lambda21
  lambda30<-lambda37+lambda39
  lambda40<-lambda42+lambda45+lambda47
  lambda90<-lambda91+lambda93+lambda97+lambda98     
  
  # make a placehold for the one-step transition probability matrix
  P<-matrix(0,nrow=10,ncol=10)
  
  P[1,1]<-exp(-lambda10*dt)
  P[1,2]<-lambda12/lambda10*(1-P[1,1])
  P[1,6]<-lambda16/lambda10*(1-P[1,1])
  P[1,7]<-lambda17/lambda10*(1-P[1,1])
  P[1,8]<-lambda18/lambda10*(1-P[1,1])
  P[1,9]<-lambda19/lambda10*(1-P[1,1]) 
  P[2,2]<-exp(-lambda20*dt)
  P[2,1]<-lambda21/lambda20*(1-P[2,2])
  P[2,4]<-lambda24/lambda20*(1-P[2,2])
  P[2,7]<-lambda27/lambda20*(1-P[2,2])
  P[3,3]<-exp(-lambda30*dt)
  P[3,7]<-lambda37/lambda30*(1-P[3,3])
  P[3,9]<-lambda39/lambda30*(1-P[3,3])
  P[4,4]<-exp(-lambda40*dt)
  P[4,2]<-lambda42/lambda40*(1-P[4,4])
  P[4,5]<-lambda45/lambda40*(1-P[4,4])
  P[4,7]<-lambda47/lambda40*(1-P[4,4])
  P[9,9]<-exp(-lambda90*dt)                              
  P[9,1]<-lambda91/lambda90*(1-P[9,9])
  P[9,3]<-lambda93/lambda90*(1-P[9,9])           
  P[9,7]<-lambda97/lambda90*(1-P[9,9])        
  P[9,8]<-lambda98/lambda90*(1-P[9,9])        
  
  # absorbing states - virus does not leave these states
  P[5,5]<-1
  P[6,6]<-1
  P[7,7]<-1
  P[8,8]<-1
  P[10,10]<-1
  
  ################### FAR FIELD TRANSITITION MATRIX ###################
  # HCW in the far-field zone, so no inhalation or contact in the near-field zone	
  
  #calculate the total first-order rate constants for leaving the zone of interest
  lambda10<-lambda12+lambda17+lambda18+lambda19   
  lambda20<-lambda27+lambda21
  lambda30<-lambda34+lambda37+lambda39
  lambda40<-lambda43+lambda45+lambda47
  lambda90<-lambda91+lambda93+lambda97+lambda98+lambda910 
  
  # make a placehold for the one-step transition probability matrix
  Pn<-matrix(0,nrow=10,ncol=10)
  
  Pn[1,1]<-exp(-lambda10*dt)
  Pn[1,2]<-lambda12/lambda10*(1-Pn[1,1])
  Pn[1,7]<-lambda17/lambda10*(1-Pn[1,1])
  Pn[1,8]<-lambda18/lambda10*(1-Pn[1,1])
  Pn[1,9]<-lambda19/lambda10*(1-Pn[1,1]) 
  Pn[2,2]<-exp(-lambda20*dt)
  Pn[2,1]<-lambda21/lambda20*(1-Pn[2,2])
  Pn[2,7]<-lambda27/lambda20*(1-Pn[2,2])
  Pn[3,3]<-exp(-lambda30*dt)
  Pn[3,4]<-lambda34/lambda30*(1-Pn[3,3])
  Pn[3,7]<-lambda37/lambda30*(1-Pn[3,3])
  Pn[3,9]<-lambda39/lambda30*(1-Pn[3,3])
  Pn[4,4]<-exp(-lambda40*dt)
  Pn[4,3]<-lambda43/lambda40*(1-Pn[4,4])
  Pn[4,5]<-lambda45/lambda40*(1-Pn[4,4])
  Pn[4,7]<-lambda47/lambda40*(1-Pn[4,4])
  Pn[9,9]<-exp(-lambda90*dt)                       
  Pn[9,1]<-lambda91/lambda90*(1-Pn[9,9])
  Pn[9,3]<-lambda93/lambda90*(1-Pn[9,9])           
  Pn[9,7]<-lambda97/lambda90*(1-Pn[9,9])        
  Pn[9,8]<-lambda98/lambda90*(1-Pn[9,9])    
  Pn[9,10]<-lambda910/lambda90*(1-Pn[9,9])    
  
  # absorbing states - virus does not leave these states
  Pn[5,5]<-1
  Pn[6,6]<-1
  Pn[7,7]<-1
  Pn[8,8]<-1
  Pn[10,10]<-1
  
  #############################################################################################################################################
  # STAGE 4: DEFINE VIRUS EMISSION CHARACTERISTICS 
  #############################################################################################################################################

  # Pathogens in COUGH particles
  if(Ncough!=0){
    # number of pathogens in particle bin
    n.paths.cough.particle<-matrix(0, nrow=16, ncol=Ncough)
    # number of particles
    n.cough.particle<-matrix(0, nrow=16, ncol=Ncough)
    for (n in 1:Ncough){
      for (i in 1:16){
        #sample the number of particles in each particle size bin 
         
        n.cough.particle[i,n]<-rpois(1,ChaoCough[i,7])
        
        
        #number of pathogens is the particle size bin is number particles x 
        #                                             volume of particles x 
        #                                             volume unit correction (um^3 to cm^3) x 
        #                                             concentration in saliva x 
        #                                             concentration adjustment
        n.paths.cough.particle[i,n]<-n.cough.particle[i,n]*ChaoCough[i,5]*(10^(-12))*CONCsaliva*Dist.saliva[i]	
      }
    }
    #emission rate to air and surfaces (pathogens/minute)
    EairCough<-sum(sum(n.paths.cough.particle[1:3,]))/Tmax
    EsurfCough<-sum(sum(n.paths.cough.particle[4:16,]))/Tmax
  }
  if(Ncough==0){
    EairCough<-0
    EsurfCough<-0
  }
  # Pathogens in BREATHING particles (onto surfaces)
  # 8 below is 8 minutes, to count to 1-100 10 times.
  nn<-round(Tmax)
  # number of pathogens in particle bin
  n.paths.speak.particle<-matrix(0, nrow=16, ncol=nn)
  # number of particles
  n.speak.particle<-matrix(0, nrow=16, ncol=nn)
  
  for (n in 1:nn){
    for (i in 1:16){
      #sample the number of particles in each particle size bin 
      n.speak.particle[i,n]<-rpois(1,ChaoSpeak[i,7])
      
      
      #number of pathogens is the particle size bin is number particles x 
      #                                             volume of particles x 
      #                                             volume unit correction (um^3 to cm^3) x 
      #                                             concentration in saliva x 
      #                                             concentration adjustment
      n.paths.speak.particle[i,n]<-n.speak.particle[i,n]*ChaoSpeak[i,5]*(10^(-12))*CONCsaliva*Dist.saliva[i]	
    }
  }
  # speaking emission rate to air and surfaces (pathogens/minute)
  EsurfSpeak<-(sum(sum(n.paths.speak.particle[4:16,]))/(Tmax*Infnonsilenttime)*quantaexhalationrate)
  # Pathogens in BREATHING/TALKING particles
  #continuous emission into air and surfaces in near field (PFU per min)
  rateEair<-(EairCough+EairTalk)/gf
  if(SpeakontoSurf=="Y"){
  rateEsurface<-(EsurfCough+EsurfSpeak)/gf
  } else{
  rateEsurface<-(EsurfCough)/gf
  }
  #continuous emission into air and surfaces in far field (PFU per min)
  rateEair2<-((EairCough+EairTalk)*FFinfected)/gf
  if(SpeakontoSurf=="Y"){
  rateEsurface2<-((EsurfCough+EsurfSpeak)*FFinfected)/gf
  } else{
  rateEsurface2<-((EsurfCough)*FFinfected)/gf
  }
  
  #############################################################################################################################################
  # STAGE 5: INITIAL COMPARTMENT CONDITIONS - equated with steady state
  #############################################################################################################################################

  # emission is into the air in the NF and onto surfaces in the NF
  Nair<-(rateEair)/(lambda12+lambda17+lambda18+lambda19)  
  Nsurface<-rateEsurface/(lambda27)
  
  # emission is into the air in the FF and onto surfaces in the FF
  Nair2<-(rateEair2)/(lambda93+lambda97+lambda98+lambda91)  
  Nsurface2<-rateEsurface2/(lambda37)
  
  #############################################################################################################################################
  # STAGE 6: TRACK DOSE TO DIFFERENT LOCATIONS 
  #############################################################################################################################################
  
  trackP<-matrix(0, nrow=(Tmax/dt), ncol=4)
  #P[1,6] = NF air to lungs
  #P[1,5] = NF air to mucous membranes
  #P[2,5] = NF substrates to mucous membranes
  #P[1,10] = NF to lower respiratory tract (FF inhalation)
  
  trackP[1,]<-c(P[1,6], P[1,5], P[2,5], P[1,10])
  
  trackFF<-matrix(0, nrow=(Tmax/dt), ncol=4)
  #P[9,6] = FF air to lungs
  #P[9,5] = FF air to membranes
  #P[3,5] = FF substrates to mucous membranes
  #P[9,10] = FF air to the lower respriatory tract of worker (inhale in far-field)
  
  trackFF[1,]<-c(P[9,6], P[9,5], P[3,5], P[9,10])
  
  #############################################################################################################################################
  # STAGE 7: COMPUTE INHALATION AND CONTACT DOSE
  #############################################################################################################################################
  
  # Create the binary variable for which transisition matrix to use in the matrix multiplication
  if(FFtime == 100){
    choice<-sample(c(0,0,0,0,0,0,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 90){
    choice<-sample(c(1,0,0,0,0,0,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 80){
    choice<-sample(c(1,1,0,0,0,0,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 70){
    choice<-sample(c(1,1,1,0,0,0,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 60){
    choice<-sample(c(1,1,1,1,0,0,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 50){
    choice<-sample(c(1,1,1,1,1,0,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 40){
    choice<-sample(c(1,1,1,1,1,1,0,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 30){
    choice<-sample(c(1,1,1,1,1,1,1,0,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 20){
    choice<-sample(c(1,1,1,1,1,1,1,1,0,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 10){
    choice<-sample(c(1,1,1,1,1,1,1,1,1,0),Tmax/dt, replace=TRUE)
  } else if(FFtime == 0){
    choice<-sample(c(1,1,1,1,1,1,1,1,1,1),Tmax/dt, replace=TRUE)
  }
  
########################################### SIMULATE MARKOV CHAIN ############################################
# note, this is the bit that is slow 

  Ptemp<-P
  # create boolean for choice
  condition<-as.logical(choice)
  
  #library(foreach)
  #library(doParallel)
  
  #numCores <- detectCores()
  
  # start from 2 because 1 is the intial concentrations
  #cl <- parallel::makeCluster(numCores)
  #doParallel::registerDoParallel(cl)
  # , .combine=rbind) %dopar%
  
   for (t in 2:(Tmax/dt)){
    if (condition[t]){
      Ptemp<-Ptemp%*%P
    } else{
      Ptemp<-Ptemp%*%Pn
    }
    trackP[t,]<-c(Ptemp[1,6], Ptemp[1,5], Ptemp[2,5],Ptemp[1,10])
    trackFF[t,]<-c(Ptemp[9,6], Ptemp[9,5], Ptemp[3,5],Ptemp[9,10])
   }
   
   #parallel::stopCluster(cl)
   
#################################### TRACKING HOW MUCH VIRUS ENDS UP WHERE ########################################
  
  nsteps<-length(trackP[,1])
  # total dose to the lung is from emission at each time step into near-field air, and surfaces
  # plus dose resulting from initial conditions in each of these three zones
  doseLUNGi<-sum(trackP[,1]*rateEair*dt)+
                 trackP[nsteps,1]*Nair
  
  doseLUNGFFi<-sum(trackP[,4]*rateEair*dt)+
                   trackP[nsteps,4]*Nair
  
  doseFACEi<-#sum(trackP[,2]*rateEair*dt)+
             sum(trackP[,3]*rateEsurface*dt)+
                 #trackP[nsteps,2]*Nair+
                 trackP[nsteps,3]*Nsurface
  doseSPRAYi<- sum(trackP[,2]*rateEair*dt)+
              trackP[nsteps,2]*Nair
  
  # if there is more than 1 infected in the room then add total dose to the lung is from emission at each time step into far-field air, and surfaces
  # plus dose resulting from initial conditions in each of these three zones
  if(Infected>1){
  doseLUNGi2<-sum(trackFF[,1]*rateEair2*dt)+
              trackFF[nsteps,1]*Nair2
  doseLUNGFFi2<-sum(trackFF[,4]*rateEair2*dt)+
                trackFF[nsteps,4]*Nair2
  doseFACEi2<-#sum(trackFF[,2]*rateEair2*dt)+
              sum(trackFF[,3]*rateEsurface2*dt)+
              #trackFF[nsteps,2]*Nair2+
              trackFF[nsteps,3]*Nsurface
  
  doseSPAYi2 <- sum(trackFF[,2]*rateEair2*dt)+
                trackFF[nsteps,2]*Nair2
  # combine the dose recieved in the near field and the far field
  doseLUNGi<-doseLUNGi+doseLUNGi2
  doseLUNGFFi<-doseLUNGFFi + doseLUNGFFi2
  doseFACEi<-doseFACEi + doseFACEi2
  doseSPRAYi<-doseSPRAYi + doseSPRAYi2
  }
  
  # apply the effect of respirator
  doseLUNG<-doseLUNGi*Finhale
  doseLUNGFF<-doseLUNGFFi*Finhale
  # apply the effect of fomite cleaning
  doseFACE<-Ffomite*doseFACEi*(Fspray*pTARGET*(Suface/Aportals)+Feye*pTARGET*(Sueye/Aportals))
  doseSPRAYi<-Ffomite*doseSPRAYi*(Fspray*pTARGET*(Suface/Aportals)+Feye*pTARGET*(Sueye/Aportals))
  
  
  #############################################################################################################################################
  # STAGE 8: COMPUTE INHALATION AND CONTACT INFECTION RISK
  #############################################################################################################################################
  
  #for exponential dose-response model
  rFACE<-1-exp(-doseFACE/DRk)
  rLUNGNF<-1-exp(-(doseLUNG)/DRk)
  rLUNGFF<-1-exp(-(doseLUNGFF)/DRk)
  
  #############################################################################################################################################
  # STAGE 9: COMPUTE SPRAY DOSE
  #############################################################################################################################################
  
  #fraction of the cone surface area that represents a facial portal/mucous membrane
  probthin <- Aportals/(3.8*10^3)   
  #fraction of the cone volume that is inhaled (0.0005 m^3 per breath, 0.079 m^3 cone volume)
  probinsp <- 0.0005/(0.079)   
  if(Ncough>0){
    doseS <-matrix(0, nrow=16, ncol=Ncough)
    doseI <-matrix(0, nrow=16, ncol=Ncough)
    for (n in 1:Ncough){
      for (i in 10:16){
        
        # if there are no particles in the size bin
        if (n.cough.particle[i,n] == 0) {
          doseS[i,n]<-0
          }
        # if there are particles in the size bin
        if (n.cough.particle[i,n]!=0) {
          # number of pathogens in particles that land on face 
          doseS[i,n]<-n.paths.cough.particle[i,n]*probthin               
        }
      }
      
      # now the spray inhalation
      for (i in 1:9){
        # if there are no particles in the size bin
        if (n.cough.particle[i,n] == 0) {
          doseI[i,n]<-0
          }
        # if there are particles in the size bin
        if (n.cough.particle[i,n]!=0) {
          # number of pathogens in particles that are inspired
          doseI[i,n]<-n.paths.cough.particle[i,n]*probinsp                         
        }	
        
        
      }
    }
  }
  
  if(Ncough==0){
    doseS<-0
    doseI<-0}
  
  # apply the exposure reduction from a respirator/surgical mask and eye protection
  doseSPRAY<-sum(sum(doseS))*(Fspray*Suface/Aportals+Feye*Sueye/Aportals)
  doseINSP<-sum(sum(doseI))*(Finhale)
  
  #############################################################################################################################################
  # STAGE 10: COMPUTE SPRAY INFECTION RISK
  #############################################################################################################################################
  
  # calculate conditional (on proportion of particles that deposit on the facial mucous membranes which reach receptors in the respiratory tract) probability 
  CONDPROBINFSPRAY<-1-exp(-(doseSPRAYi+((doseSPRAY*pTARGET)*doseINSP))/DRk)
  
  # calculate the unconditional probability (conditional multiplied by probability of HCW intercepting cough)
  UNCONDPROBINFSPRAY <- pSPRAY*CONDPROBINFSPRAY
  rSPRAY <- 1 - ((1 - UNCONDPROBINFSPRAY)^Ncough)
  # Make sure if there is no coughs then there is no cough spray risk
  if(Ncough==0){
    doseSPRAY<-0
    rSPRAY<-0
  }
  
  #############################################################################################################################################
  # STAGE 11: COMPUTE OVERALLL INFECTION RISK AND GENERATE OUTPUTS
  #############################################################################################################################################
  
  # COMPUTED USING INCLUSION-EXCLUSION FORMULA
  rOVERALL<-
    rFACE+rLUNGNF+rLUNGFF+rSPRAY-
    rFACE*rLUNGNF+rFACE*rLUNGFF+rFACE*rSPRAY+rLUNGNF*rLUNGFF+rLUNGNF*rSPRAY+rLUNGFF*rSPRAY+
    rFACE*rLUNGNF*rLUNGFF+rFACE*rLUNGNF*rSPRAY+rFACE*rLUNGFF*rSPRAY+rLUNGNF*rLUNGFF*rSPRAY-
    rFACE*rLUNGNF*rLUNGFF*rSPRAY
  
  # calculate how many became infected by multiplying the overall risk by how many are susceptible
  numberinfected<-Su*rOVERALL
  
  # combine all the outputs and add them to the original dataframe
  result<-cbind(
    V,W,Vn,NFsizeperc, betaNF, ACH, pSPRAY, FFtime, 
                Tmax, Infcoughrateperhour,Ncough, FFinfected, COUGHrate, 
                CONCsaliva, gf, Infactivity, InfEairTalkS,EairTalkS,InfEairTalkSQA,  INACTIVair, 
                INACTIVsurface, INACTIVskin, Aportals, TRANSsurface.skin, TRANSskin,
                CONTACTsurfaceNF.hand, CONTACTsurfaceFF.hand, CONTACTface.hand, pTARGET,
                ExtraExpVolStudy, distsalivavirusconc, Feye, Fspray, Finhale, Ffomite, 
                rateEair, rateEsurface, rateEair2, rateEsurface2,nsteps,  probthin, probinsp,
                Nsurface, Nsurface2, Nair, Nair2, doseFACE, doseLUNG, doseLUNGFF, doseSPRAY,rFACE, rSPRAY, rLUNGNF, rLUNGFF, rOVERALL, numberinfected)
  result<-data.frame(result)
  return(result)
}
)
