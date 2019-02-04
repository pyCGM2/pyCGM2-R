


#### NEW ####
consistencyPlot<-function(framesTable, iContext , iLabel,iAxis, 
                              iTitle="",yLabel="Deg", legendPosition = "none",
                              ylimits=NULL,
                              colorFactor=NULL,facetFactor=NULL,linetypeFactor=NULL){

  
    gatherFramesTbl = framesTable %>% gather(Frame, Values,  Frame0:Frame100) 
    
    iData = filter(gatherFramesTbl, 
                   Label == iLabel & Axis == iAxis  & Context ==iContext)  
    
   
  
  fig = ggplot() + 
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    ggtitle(iTitle)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition) 
  
  iData$inter <- interaction(iData$Index) # !!!! I spent lot of time to find that
  #iData$inter <- interaction(iData$Id,iData$Label,iData$Context,iData$Axis,iData$ComparisonFactor, iData$Cycle) # !!!! I spent lot of time to find that
  
  if ((!is.null(colorFactor)) &&   (!is.null(linetypeFactor))) { 
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = colorFactor,
                                     linetype = linetypeFactor,
                                     group = "inter"),
                          size=0.5)
  } else if ((!is.null(colorFactor)) &&   (is.null(linetypeFactor))){
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = colorFactor,
                                     group = "inter"),
                          size=0.5)
  } else if ((is.null(colorFactor)) &&   !(is.null(linetypeFactor))){
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     linetype = linetypeFactor,
                                     group = "inter"),
                          size=0.5)
  }  else {
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = "Context",
                                     group = "inter"),
                          size=0.5)
    
  }
  
  
  if (!is.null(facetFactor)){ fig =  fig+facet_grid(paste0(".~", facetFactor))}
  
  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}
  
  
  
  fig
  
  return(fig)
  
}





descriptivePlot<-function(framesTableDS, iContext , iLabel,iAxis, 
                          iTitle="",yLabel="Deg", legendPosition = "none",
                          colorFactor=NULL,facetFactor=NULL,linetypeFactor=NULL,
                          ylimits=NULL,
                          lineWidth=0.5){
  
  gatherFramesTbl = gather_FrameValues_DescritiveStats( framesTableDS)    
  
  iData = filter(gatherFramesTbl, 
                 Stats == "mean" & 
                   Label == iLabel & Axis == iAxis & Context == iContext )  
  
    
  
  fig = ggplot() +
    ggtitle(iTitle)+
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition) 


  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}
  

  #fig = fig + geom_line(data=iData,
  #                      aes(x=Frame,y=Values,
  #                          color = Id,
  #                          group=interaction(Id,Label,Context,Axis,ComparisonFactor)),
  #                      size=0.5)+facet_grid(.~Id)
  
  #iData$inter <- interaction(iData$Id,iData$Label,iData$Context,iData$Axis,iData$ComparisonFactor) # !!!! I spent lot of time to find that    
  iData$inter <- interaction(iData$Index) # !!!! I spent lot of time to find that
  
  if ((!is.null(colorFactor)) &&   (!is.null(linetypeFactor))) { 
  
      fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = colorFactor,
                                     linetype = linetypeFactor,
                                     group = "inter"),
                          size=lineWidth)
  } else if ((!is.null(colorFactor)) &&   (is.null(linetypeFactor))){
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = colorFactor,
                                     group = "inter"),
                          size=lineWidth)
  } else if ((is.null(colorFactor)) &&   !(is.null(linetypeFactor))){
  
  fig = fig + geom_line(data=iData,  
                        aes_string(x="Frame",y="Values",
                                   linetype = linetypeFactor,
                                   group = "inter"),
                        size=lineWidth)
  } else {
    
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = "Context",
                                     group = "inter"),
                          size=lineWidth)
    
    if (iContext=="Left"){
      fig = fig + scale_color_manual(values=c("red"))
    } else if (iContext=="Right"){
      fig = fig + scale_color_manual(values=c("blue"))
      }
    
  }
  

  
  if (!is.null(facetFactor)){ fig =  fig+facet_grid(paste0(".~", facetFactor))}
    

  return(fig)
  
}

consistencyPlot_bothContext<-function(framesTable,  iLabelLeft,iAxisLeft, iLabelRight,iAxisRight, 
                                      iTitle="",yLabel="Deg", legendPosition = "none",
                                      ylimits=NULL){
  
  #f = consistencyPlot_bothContext(kinematicTable_both,subjects,  "LHipAngles","X", "RHipAngles","X",iComparisonFactor=c("OP01_S01","OP01_S02"), 
  #iTitle="",yLabel="Deg", legendPosition = "none",
  #ylimits=NULL)
  #f


  gatherFramesTbl = framesTable %>% gather(Frame, Values,  Frame0:Frame100) 
    
  iData_L = filter(gatherFramesTbl, 
                 Label == iLabelLeft & Axis == iAxisLeft)  
    
  iData_R = filter(gatherFramesTbl, 
                     Label == iLabelRight & Axis == iAxisRight)
    
  
  iData = bind_rows(iData_L, iData_R)
  
  fig = ggplot() + 
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    ggtitle(iTitle)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition) 
  
  
  
  fig = fig + geom_line(data=iData,
                        aes(x=Frame,y=Values,
                            color = Context,
                            group=interaction(Index)),
                        size=0.5)

  
  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}
  
  
  
  fig
  
  return(fig)

  
}



descriptivePlot_bothContext<-function(framesTableDS,  iLabelLeft,iAxisLeft, iLabelRight,iAxisRight, 
                                      iTitle="",yLabel="Deg", legendPosition = "none",
                                      ylimits=NULL){
  
  #f = descriptivePlot_bothContext(descriptiveKinematics_both,"All", "LHipAngles","X", "RHipAngles","X",
  #                                iComparisonFactor=c("OP01_S01","OP01_S02"), 
  #                                iTitle="",yLabel="Deg", legendPosition = "none",ylimits=NULL)
  
  # peut afficher differentes conditions et sujets mais ne les distingue pas par couleurs ou styles de ligne
  
  
  gatherFramesTbl = gather_FrameValues_DescritiveStats( framesTableDS)    
  
  
  iData_L = filter(gatherFramesTbl, 
                   Stats == "mean" & 
                     Label == iLabelLeft & Axis == iAxisLeft & Context == "Left" )  
  
  iData_R = filter(gatherFramesTbl, 
                   Stats == "mean" & 
                     Label == iLabelRight & Axis == iAxisRight & Context == "Right")  
    
  

  iData = bind_rows(iData_L, iData_R)
  
  
  fig = ggplot() + 
    scale_x_discrete(name="Time Normalized")+
    ylab(yLabel)+#scale_y_continuous(name=yLabel)+
    ggtitle(iTitle)+
    theme(panel.grid.minor = element_blank(),
          axis.title.x =      element_text(size = 10),
          axis.text.x  = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title.y =  element_text(size = 10),
          axis.text.y  = element_text(size=8),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10,face="plain"),
          plot.title = element_text(family="Times", face="plain", size=10),
          legend.position=legendPosition) 
  
  
  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}
  
  if (!is.null(iComparisonFactor)){
    fig = fig + geom_line(data=iData,
                          aes(x=Frame,y=Values,
                              color = Context,
                              group=interaction(Index)),
                          size=0.5)
  } else {
    
    fig = fig + geom_line(data=iData,
                          aes(x=Frame,y=Values,
                              color = Context,
                              group=interaction(Index)),
                          size=0.5)
    
  }
  
  
  
  return(fig)
  
}



#--- new figure add-on----
addGaitDescriptiveEventsLines<-function(fig,phaseTableDS,iContext,
                                        colorFactor=NULL,linetypeFactor=NULL ){

  gaitEventsTableFilt =  filter(phaseTableDS, Context ==iContext)

  
  gaitEvents =gather_DescritiveStats(gaitEventsTableFilt,"ComparisonFactor",c("stancePhase","doubleStance1","doubleStance2"))
    
  if ((!is.null(colorFactor)) &&   (!is.null(linetypeFactor))) { 
    fig = fig + geom_vline( data = filter(gaitEvents,Factor == "stancePhase"),
                            aes_string(xintercept="Mean",
                                color=colorFactor,
                                linetype = linetypeFactor),show_guide = FALSE)

  } else   if (!(is.null(colorFactor)) &&   (is.null(linetypeFactor))) { 
    fig = fig + geom_vline( data = filter(gaitEvents,Factor == "stancePhase"),
                            aes_string(xintercept="Mean",
                                       color=colorFactor),show_guide = FALSE)

  } else  if ((is.null(colorFactor)) &&   !(is.null(linetypeFactor))) { 
    
    fig = fig + geom_vline( data = filter(gaitEvents,Factor == "stancePhase"),
                            aes_string(xintercept="Mean",
                                       linetype=linetypeFactor),show_guide = FALSE)
  } else {
    
    fig = fig + geom_vline( data = filter(gaitEvents,Factor == "stancePhase"),
                            aes_string(xintercept="Mean",
                                       color="Context"),show_guide = FALSE)
    
  }
  
  return(fig)
}



addGaitDescriptiveEventsLines_bothContext<-function(fig,phaseTableDS){
  

  gaitEvents =gather_DescritiveStats(phaseTableDS,"ComparisonFactor",c("stancePhase","doubleStance1","doubleStance2"))
  
  
  
  fig = fig + geom_vline( data = filter(gaitEvents,Factor == "stancePhase"),
                          aes(xintercept="Mean",
                              color=Context),show_guide = FALSE)
  

  
  return(fig)
}


normative_ribbon <- function(data) {
  # programming as a new geom ( see https://rpubs.com/hadley/97970)
  list(
    geom_ribbon(data = data,
                aes(ymin = Min, ymax = Max, x= Frame,group=interaction(Label,Axis)),
                fill = "grey70",alpha = 0.4)
    )
}


std_ribbon <- function(dataDf) {
  # programming as a new geom ( see https://rpubs.com/hadley/97970)
  
  data = getStdCorridorLimits(dataDf)
  
  list(
    geom_ribbon(data = data,
                aes(ymin = Min, ymax = Max,fill = ComparisonFactor, x= Frame,group=interaction(ComparisonFactor,Label,Axis)),
                alpha = 0.4)
  )
}

# ribbon <- function(data) {
#   # programming as a new geom ( see https://rpubs.com/hadley/97970)
#   list(
#     geom_ribbon(data = data,
#                 aes(ymin = Min, ymax = Max, x= Frame,group=interaction(Label,Axis)),
#                 fill = "grey70",alpha = 0.4)
#   )
# }



### NEW PLOT PANEL  ####
descriptiveKinematicGaitPanel<- function(descDf,descEvents, iContext,
                                         colorFactor=NULL, linetypeFactor=NULL,
                                         normativeData=NULL,stdCorridor=FALSE,
                                         manualLineType=NULL,manualSizeType=NULL){
  
  
  

  # trace uniquement UN context  
  
  if (iContext== "Left"){
    prefixe = "L"
  } else if (iContext== "Right"){
    prefixe = "R"
  } else if (iContext== "Overall"){
    prefixe = ""}
  
  # trace uni
  Pelvis_X = descriptivePlot(descDf,  iContext , paste0(prefixe,"PelvisAngles"),"X", 
                             iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(0,60),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Pelvis_Y = descriptivePlot(descDf,  iContext , paste0(prefixe,"PelvisAngles"),"Y", 
                             iTitle="Pelvic obliquity",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Pelvis_Z = descriptivePlot(descDf,  iContext , paste0(prefixe,"PelvisAngles"),"Z", 
                             iTitle="Pelvis rotation",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Hip_X = descriptivePlot(descDf,  iContext , paste0(prefixe,"HipAngles"),"X", 
                          iTitle="Hip flexion",yLabel="Deg", legendPosition="none",ylimits=c(-20,70),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Hip_Y = descriptivePlot(descDf,  iContext , paste0(prefixe,"HipAngles"),"Y", 
                          iTitle="Hip Abd",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Hip_Z = descriptivePlot(descDf,  iContext , paste0(prefixe,"HipAngles"),"Z", 
                          iTitle="Hip rot",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  
  Knee_X = descriptivePlot(descDf,  iContext , paste0(prefixe,"KneeAngles"),"X", 
                           iTitle="Knee flexion",yLabel="Deg", legendPosition="none",ylimits=c(-15,75),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Knee_Y = descriptivePlot(descDf,  iContext , paste0(prefixe,"KneeAngles"),"Y", 
                           iTitle="Knee Abd",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Knee_Z = descriptivePlot(descDf,  iContext , paste0(prefixe,"KneeAngles"),"Z", 
                           iTitle="Knee rot",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  
  Ankle_X = descriptivePlot(descDf,  iContext , paste0(prefixe,"AnkleAngles"),"X", 
                            iTitle="Ankle flexion",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Ankle_Y = descriptivePlot(descDf,  iContext , paste0(prefixe,"AnkleAngles"),"Y", 
                            iTitle="Ankle eversion",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  FootProgress_Z = descriptivePlot(descDf,  iContext , paste0(prefixe,"FootProgressAngles"),"Z", 
                                   iTitle="Foot progression",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                                   colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  if (!(is.null(descEvents))){
    Pelvis_X=addGaitDescriptiveEventsLines(Pelvis_X,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Pelvis_Y=addGaitDescriptiveEventsLines(Pelvis_Y,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Pelvis_Z=addGaitDescriptiveEventsLines(Pelvis_Z,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Hip_X=addGaitDescriptiveEventsLines(Hip_X,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Hip_Y=addGaitDescriptiveEventsLines(Hip_Y,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Hip_Z=addGaitDescriptiveEventsLines(Hip_Z,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Knee_X=addGaitDescriptiveEventsLines(Knee_X,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Knee_Y=addGaitDescriptiveEventsLines(Knee_Y,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Knee_Z=addGaitDescriptiveEventsLines(Knee_Z,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Ankle_X=addGaitDescriptiveEventsLines(Ankle_X,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Ankle_Y=addGaitDescriptiveEventsLines(Ankle_Y,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    FootProgress_Z=addGaitDescriptiveEventsLines(FootProgress_Z,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
  
  }
  
  
  if (!(is.null(normativeData))){
    Pelvis_X = Pelvis_X+normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "X"))  
    Pelvis_Y = Pelvis_Y+normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Y"))  
    Pelvis_Z = Pelvis_Z+normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Z"))  

    Hip_X = Hip_X+normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "X"))  
    Hip_Y = Hip_Y+normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Y"))  
    Hip_Z = Hip_Z+normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Z"))  
    
    Knee_X = Knee_X+normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "X"))  
    Knee_Y = Knee_Y+normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Y"))  
    Knee_Z = Knee_Z+normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Z"))  
    
    Ankle_X = Ankle_X+normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "X"))  
    Ankle_Y = Ankle_Y+normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "Y"))  
    FootProgress_Z = FootProgress_Z+normative_ribbon(filter(normativeData,Label=="FootProgressAngles" & Axis == "Z"))
  }
  
  if (stdCorridor){  
    
    data = getStdCorridorLimits(descDf)
    
    # Pelvis_X = Pelvis_X +
    #   geom_ribbon(data = filter(data,Label=="PelvisAngles" & Axis == "X"),
    #               aes(ymin = Min, ymax = Max,fill = ComparisonFactor, x= Frame,group=interaction(ComparisonFactor,Label,Axis)),
    #               alpha = 0.4)
  
    Pelvis_X = Pelvis_X + std_ribbon(filter(descDf,Label==paste0(prefixe,"PelvisAngles") & Axis == "X"))
    Pelvis_Y = Pelvis_Y + std_ribbon(filter(descDf,Label==paste0(prefixe,"PelvisAngles") & Axis == "Y"))
    Pelvis_Z = Pelvis_Z + std_ribbon(filter(descDf,Label==paste0(prefixe,"PelvisAngles") & Axis == "Z"))
     
    Hip_X = Hip_X + std_ribbon(filter(descDf,Label==paste0(prefixe,"HipAngles") & Axis == "X"))
    Hip_Y = Hip_Y + std_ribbon(filter(descDf,Label==paste0(prefixe,"HipAngles") & Axis == "Y"))
    Hip_Z = Hip_Z + std_ribbon(filter(descDf,Label==paste0(prefixe,"HipAngles") & Axis == "Z"))
     
    Knee_X = Knee_X + std_ribbon(filter(descDf,Label==paste0(prefixe,"KneeAngles") & Axis == "X"))
    Knee_Y = Knee_Y + std_ribbon(filter(descDf,Label==paste0(prefixe,"KneeAngles") & Axis == "Y"))
    Knee_Z = Knee_Z + std_ribbon(filter(descDf,Label==paste0(prefixe,"KneeAngles") & Axis == "Z"))
     
    Ankle_X = Ankle_X + std_ribbon(filter(descDf,Label==paste0(prefixe,"AnkleAngles") & Axis == "X"))
    Ankle_Y = Ankle_Y + std_ribbon(filter(descDf,Label==paste0(prefixe,"AnkleAngles") & Axis == "Y"))
    FootProgress_Z = FootProgress_Z + std_ribbon(filter(descDf,Label==paste0(prefixe,"FootProgressAngles") & Axis == "Z"))

  }
  
  if (!(is.null(manualLineType))){
    Pelvis_X = Pelvis_X + scale_linetype_manual(values=manualLineType)
    Pelvis_Y = Pelvis_Y + scale_linetype_manual(values=manualLineType)
    Pelvis_Z = Pelvis_Z + scale_linetype_manual(values=manualLineType)
    
    Hip_X = Hip_X + scale_linetype_manual(values=manualLineType)
    Hip_Y = Hip_Y + scale_linetype_manual(values=manualLineType)
    Hip_Z = Hip_Z + scale_linetype_manual(values=manualLineType)
    
    Knee_X = Knee_X + scale_linetype_manual(values=manualLineType)
    Knee_Y = Knee_Y + scale_linetype_manual(values=manualLineType)
    Knee_Z = Knee_Z + scale_linetype_manual(values=manualLineType)
    
    Ankle_X = Ankle_X + scale_linetype_manual(values=manualLineType)
    Ankle_Y = Ankle_Y + scale_linetype_manual(values=manualLineType)
    FootProgress_Z = FootProgress_Z + scale_linetype_manual(values=manualLineType)
    
  }
  
  if (!(is.null(manualSizeType))){
    Pelvis_X = Pelvis_X + scale_size_manual(values=manualSizeType)
    Pelvis_Y = Pelvis_Y + scale_size_manual(values=manualSizeType)
    Pelvis_Z = Pelvis_Z + scale_size_manual(values=manualSizeType)
    
    Hip_X = Hip_X + scale_size_manual(values=manualSizeType)
    Hip_Y = Hip_Y + scale_size_manual(values=manualSizeType)
    Hip_Z = Hip_Z + scale_size_manual(values=manualSizeType)
    
    Knee_X = Knee_X + scale_size_manual(values=manualSizeType)
    Knee_Y = Knee_Y + scale_size_manual(values=manualSizeType)
    Knee_Z = Knee_Z + scale_size_manual(values=manualSizeType)
    
    Ankle_X = Ankle_X + scale_size_manual(values=manualSizeType)
    Ankle_Y = Ankle_Y + scale_size_manual(values=manualSizeType)
    FootProgress_Z = FootProgress_Z + scale_size_manual(values=manualSizeType)
    
  }
  

            
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)
  
  
  legend_shared <- get_legend(Pelvis_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))  
  
  fig = plot_grid(p1, p2,p3, p4,
                  legend_shared,
                  nrow = 5,rel_heights = c(1,1,1,1, .2))  
  
  fig
  
  return(fig)
  
  
}

descriptiveKinematicGaitPanel_both<- function(descDf,descEvents=NULL,normativeData=NULL){
  # 
  
    
  Pelvis_X = descriptivePlot_bothContext(descDf, "LPelvisAngles","X", "RPelvisAngles","X",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(0,60))  
  
  Pelvis_Y = descriptivePlot_bothContext(descDf, "LPelvisAngles","Y", "RPelvisAngles","Y",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  
  Pelvis_Z = descriptivePlot_bothContext(descDf, "LPelvisAngles","Z", "RPelvisAngles","Z",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  
  
  Hip_X = descriptivePlot_bothContext(descDf, "LHipAngles","X", "RHipAngles","X",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-20,70))  
  
  Hip_Y = descriptivePlot_bothContext(descDf, "LHipAngles","Y", "RHipAngles","Y",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  
  Hip_Z = descriptivePlot_bothContext(descDf, "LHipAngles","Z", "RHipAngles","Z",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  
  
  
  Knee_X = descriptivePlot_bothContext(descDf, "LKneeAngles","X", "RKneeAngles","X",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-15,75))  
  
  Knee_Y = descriptivePlot_bothContext(descDf, "LKneeAngles","Y", "RKneeAngles","Y",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  
  Knee_Z = descriptivePlot_bothContext(descDf, "LKneeAngles","Z", "RKneeAngles","Z",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  

  Ankle_X = descriptivePlot_bothContext(descDf, "LAnkleAngles","X", "RAnkleAngles","X",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  
  
  Ankle_Y = descriptivePlot_bothContext(descDf, "LAnkleAngles","Y", "RAnkleAngles","Y",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  FootProgress_Z = descriptivePlot_bothContext(descDf, "LFootProgressAngles","Z", "RFootProgressAngles","Z",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="none",ylimits=c(-30,30))  

  if (!(is.null(descEvents))){
    Pelvis_X=addGaitDescriptiveEventsLines_bothContext(Pelvis_X,descEvents)
    Pelvis_Y=addGaitDescriptiveEventsLines_bothContext(Pelvis_Y,descEvents)
    Pelvis_Z=addGaitDescriptiveEventsLines_bothContext(Pelvis_Z,descEvents)
    
    Hip_X=addGaitDescriptiveEventsLines_bothContext(Hip_X,descEvents)
    Hip_Y=addGaitDescriptiveEventsLines_bothContext(Hip_Y,descEvents)
    Hip_Z=addGaitDescriptiveEventsLines_bothContext(Hip_Z,descEvents)
    
    Knee_X=addGaitDescriptiveEventsLines_bothContext(Knee_X,descEvents)
    Knee_Y=addGaitDescriptiveEventsLines_bothContext(Knee_Y,descEvents)
    Knee_Z=addGaitDescriptiveEventsLines_bothContext(Knee_Z,descEvents)
    
    Ankle_X=addGaitDescriptiveEventsLines_bothContext(Ankle_X,descEvents)
    Ankle_Y=addGaitDescriptiveEventsLines_bothContext(Ankle_Y,descEvents)
    FootProgress_Z=addGaitDescriptiveEventsLines_bothContext(FootProgress_Z,descEvents)
  }

  if (!(is.null(normativeData))){
    Pelvis_X = Pelvis_X+normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "X"))  
    Pelvis_Y = Pelvis_X+normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Y"))  
    Pelvis_Z = Pelvis_X+normative_ribbon(filter(normativeData,Label=="PelvisAngles" & Axis == "Z"))  
    
    Hip_X = Hip_X+normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "X"))  
    Hip_Y = Hip_Y+normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Y"))  
    Hip_Z = Hip_Z+normative_ribbon(filter(normativeData,Label=="HipAngles" & Axis == "Z"))  
    
    Knee_X = Knee_X+normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "X"))  
    Knee_Y = Knee_Y+normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Y"))  
    Knee_Z = Knee_Z+normative_ribbon(filter(normativeData,Label=="KneeAngles" & Axis == "Z"))  
    
    Ankle_X = Ankle_X+normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "X"))  
    Ankle_Y = Ankle_Y+normative_ribbon(filter(normativeData,Label=="AnkleAngles" & Axis == "Y"))  
    FootProgress_Z = FootProgress_Z+normative_ribbon(filter(normativeData,Label=="FootProgressAngles" & Axis == "Z"))
  }
  
  
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)
  
  
    
  legend_shared <- get_legend(Pelvis_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))  
  
  fig = plot_grid(p1, p2,p3, p4,
                  legend_shared,
                  nrow = 5,rel_heights = c(1,1,1,1, .2))  
  
  fig
  
  return(fig)
  
}




descriptiveKineticGaitPanel<- function(descDf,descEvents, iContext,
                                         colorFactor=NULL, linetypeFactor=NULL,
                                       normativeData=NULL){
  
  
  
  # trace uniquement UN context  
  
  if (iContext== "Left"){
    prefixe = "L"
  } else if (iContext== "Right"){
    prefixe = "R"
  } else if (iContext== "Overall"){
    prefixe = ""}
  

  Hip_X = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"X", 
                             iTitle="Hip extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-2.0,3.0),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Hip_Y = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"Y", 
                          iTitle="Hip abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,2.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Hip_Z = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"Z", 
                          iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Hip_Power = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipPower"),"Z", 
                          iTitle="Hip power",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  

  Knee_X = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"X", 
                          iTitle="Knee extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Knee_Y = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"Y", 
                          iTitle="Knee abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Knee_Z = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"Z", 
                          iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Knee_Power = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneePower"),"Z", 
                              iTitle="Knee power",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0),
                              colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  
  
  Ankle_X = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"X", 
                           iTitle="Ankle extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,3.0),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Ankle_Y = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"Y", 
                           iTitle="Ankle abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Ankle_Z = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"Z", 
                           iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Ankle_Power = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnklePower"),"Z", 
                               iTitle="Ankle power",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-2.0,5.0),
                               colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  
  if (!(is.null(descEvents))){
    Hip_X=addGaitDescriptiveEventsLines(Hip_X,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Hip_Y=addGaitDescriptiveEventsLines(Hip_Y,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Hip_Z=addGaitDescriptiveEventsLines(Hip_Z,descEvents,iContext,
                                           colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Hip_Power=addGaitDescriptiveEventsLines(Hip_Power,descEvents,iContext,
                                        colorFactor=colorFactor, linetypeFactor=linetypeFactor  )

    
    Knee_X=addGaitDescriptiveEventsLines(Knee_X,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Knee_Y=addGaitDescriptiveEventsLines(Knee_Y,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Knee_Z=addGaitDescriptiveEventsLines(Knee_Z,descEvents,iContext,
                                         colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Knee_Power=addGaitDescriptiveEventsLines(Knee_Power,descEvents,iContext,
                                            colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    

    Ankle_X=addGaitDescriptiveEventsLines(Ankle_X,descEvents,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    Ankle_Y=addGaitDescriptiveEventsLines(Ankle_Y,descEvents,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Ankle_Z=addGaitDescriptiveEventsLines(Ankle_Z,descEvents,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
    
    Ankle_Power=addGaitDescriptiveEventsLines(Ankle_Power,descEvents,iContext,
                                          colorFactor=colorFactor, linetypeFactor=linetypeFactor  )
    
  }
  if (!(is.null(normativeData))){

    Hip_X = Hip_X+normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "X"))  
    Hip_Y = Hip_Y+normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Y"))  
    Hip_Z = Hip_Z+normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Z"))  
    Hip_Power = Hip_Power+normative_ribbon(filter(normativeData,Label=="HipPower" & Axis == "Z"))  
    
        
    Knee_X = Knee_X+normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "X"))  
    Knee_Y = Knee_Y+normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Y"))  
    Knee_Z = Knee_Z+normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Z"))  
    Knee_Power = Knee_Power+normative_ribbon(filter(normativeData,Label=="KneePower" & Axis == "Z"))  
  
    Ankle_X = Ankle_X+normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "X"))  
    Ankle_Y = Ankle_Y+normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Y"))  
    Ankle_Z = Ankle_Z+normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Z"))  
    Ankle_Power = Ankle_Power+normative_ribbon(filter(normativeData,Label=="AnklePower" & Axis == "Z"))  
    
  
    }
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_power,ncol=4)
  
  legend_shared <- get_legend(Hip_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))
  
  fig = plot_grid(p1,p2,p3,legend_shared, 
                  nrow = 4, rel_heights = c(1,1,1,.2))
  
  fig
  
  return(fig)
  
    
  
}


descriptiveKineticGaitPanel_both<- function(descDf,descEvents=NULL,
                                            normativeData=NULL){
  # 
  
  
  Hip_X = descriptivePlot_bothContext(descDf, "LHipMoment","X", "RHipMoment","X",
                                         
                                         iTitle="Hip extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-2.0,3.0))  
  
  Hip_Y = descriptivePlot_bothContext(descDf, "LHipMoment","Y", "RHipMoment","Y",
                                         
                                         iTitle="Hip abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,2.0))  
  
  Hip_Z = descriptivePlot_bothContext(descDf, "LHipMoment","Z", "RHipMoment","Z",
                                         
                                         iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))  
  
  Hip_Power = descriptivePlot_bothContext(descDf, "LHipMoment","Z", "RHipMoment","Z",
                                      
                                      iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0))  

  
  Knee_X = descriptivePlot_bothContext(descDf, "LKneeMoment","X", "RKneeMoment","X",
                                      
                                      iTitle="Knee extensor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0))  
                                      
  Knee_Y = descriptivePlot_bothContext(descDf, "LKneeMoment","Y", "RKneeMoment","Y",
                                      
                                      iTitle="Knee abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-1.0,1.0))  
  
  Knee_Z = descriptivePlot_bothContext(descDf, "LKneeMoment","Z", "RKneeMoment","Z",
                                      
                                      iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))  
  
  Knee_Power = descriptivePlot_bothContext(descDf, "LKneeMoment","Z", "RKneeMoment","Z",
                                          
                                          iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-3.0,3.0))  
                                      
  Ankle_X = descriptivePlot_bothContext(descDf, "LAnkleMoment","X", "RAnkleMoment","X",
                                      
  iTitle="Ankle extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,3.0))  
  
  Ankle_Y = descriptivePlot_bothContext(descDf, "LAnkleMoment","Y", "RAnkleMoment","Y",
                                      
                                      iTitle="Ankle abductor moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))  
  
  Ankle_Z = descriptivePlot_bothContext(descDf, "LAnkleMoment","Z", "RAnkleMoment","Z",
                                      
                                      iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-0.5,0.5))  
  
  Ankle_Power = descriptivePlot_bothContext(descDf, "LAnkleMoment","Z", "RAnkleMoment","Z",
                                          
                                          iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="none",ylimits=c(-5.0,5.0))  
  
  
  if (!(is.null(descEvents))){
    
    Hip_X=addGaitDescriptiveEventsLines_bothContext(Hip_X,descEvents)
    Hip_Y=addGaitDescriptiveEventsLines_bothContext(Hip_Y,descEvents)
    Hip_Z=addGaitDescriptiveEventsLines_bothContext(Hip_Z,descEvents)
    Hip_Power=addGaitDescriptiveEventsLines_bothContext(Hip_Power,descEvents)
    
    Knee_X=addGaitDescriptiveEventsLines_bothContext(Knee_X,descEvents)
    Knee_Y=addGaitDescriptiveEventsLines_bothContext(Knee_Y,descEvents)
    Knee_Z=addGaitDescriptiveEventsLines_bothContext(Knee_Z,descEvents)
    Knee_Power=addGaitDescriptiveEventsLines_bothContext(Knee_Power,descEvents)
    
    Ankle_X=addGaitDescriptiveEventsLines_bothContext(Ankle_X,descEvents)
    Ankle_Y=addGaitDescriptiveEventsLines_bothContext(Ankle_Y,descEvents)
    Ankle_Z=addGaitDescriptiveEventsLines_bothContext(Ankle_Z,descEvents)
    Ankle_Power=addGaitDescriptiveEventsLines_bothContext(Ankle_Power,descEvents)
    
  }
  
  if (!(is.null(normativeData))){
    
    Hip_X = Hip_X+normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "X"))  
    Hip_Y = Hip_Y+normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Y"))  
    Hip_Z = Hip_Z+normative_ribbon(filter(normativeData,Label=="HipMoment" & Axis == "Z"))  
    Hip_Power = Hip_Power+normative_ribbon(filter(normativeData,Label=="HipPower" & Axis == "Z"))  
    
    
    Knee_X = Knee_X+normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "X"))  
    Knee_Y = Knee_Y+normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Y"))  
    Knee_Z = Knee_Z+normative_ribbon(filter(normativeData,Label=="KneeMoment" & Axis == "Z"))  
    Knee_Power = Knee_Power+normative_ribbon(filter(normativeData,Label=="KneePower" & Axis == "Z"))  
    
    Ankle_X = Ankle_X+normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "X"))  
    Ankle_Y = Ankle_Y+normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Y"))  
    Ankle_Z = Ankle_Z+normative_ribbon(filter(normativeData,Label=="AnkleMoment" & Axis == "Z"))  
    Ankle_Power = Ankle_Power+normative_ribbon(filter(normativeData,Label=="AnklePower" & Axis == "Z"))  
    
    
  }
  
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_Power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_Power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_Power,ncol=4)
  
  legend_shared <- get_legend(Hip_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))
  
  fig = plot_grid(p1,p2,p3,legend_shared, 
                  nrow = 4, rel_heights = c(1,1,1,.2))
  
  fig
  
  return(fig)
  
}
  
  

