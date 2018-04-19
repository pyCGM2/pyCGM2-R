


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
    scale_y_continuous(name=yLabel)+
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
                          ylimits=NULL){
  
  gatherFramesTbl = gather_FrameValues_DescritiveStats( framesTableDS)    
  
  iData = filter(gatherFramesTbl, 
                 Stats == "mean" & 
                   Label == iLabel & Axis == iAxis & Context == iContext )  
  
    
  
  fig = ggplot() + 
    scale_x_discrete(name="Time Normalized")+
    scale_y_continuous(name=yLabel)+
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
  } else {
    
    
    fig = fig + geom_line(data=iData,  
                          aes_string(x="Frame",y="Values",
                                     color = "Context",
                                     group = "inter"),
                          size=0.5)
    
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
    scale_y_continuous(name=yLabel)+
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
    scale_y_continuous(name=yLabel)+
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




### NEW PLOT PANEL  ####
descriptiveKinematicGaitPanel<- function(descDf,descEvents, iContext,
                                         colorFactor=NULL, linetypeFactor=NULL){
  
  

  # trace uniquement UN context  
  
  if (iContext== "Left"){
    prefixe = "L"
  } else if (iContext== "Right"){
    prefixe = "R"
  } else if (iContext== "Overall"){
    prefixe = ""}
  
  # trace uni
  Pelvis_X = descriptivePlot(descDf,  iContext , paste0(prefixe,"PelvisAngles"),"X", 
                             iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(0,60),
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
                            iTitle="Knee flexion",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  Ankle_Y = descriptivePlot(descDf,  iContext , paste0(prefixe,"AnkleAngles"),"Y", 
                            iTitle="Knee Abd",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
                            colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  FootProgress_Z = descriptivePlot(descDf,  iContext , paste0(prefixe,"FootProgressAngles"),"Z", 
                                   iTitle="Knee rot",yLabel="Deg", legendPosition="none",ylimits=c(-30,30),
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

descriptiveKinematicGaitPanel_both<- function(descDf,descEvents){
  # 
  
    
  Pelvis_X = descriptivePlot_bothContext(descDf, "LPelvisAngles","X", "RPelvisAngles","X",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(0,60))  
  
  Pelvis_Y = descriptivePlot_bothContext(descDf, "LPelvisAngles","Y", "RPelvisAngles","Y",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  Pelvis_Z = descriptivePlot_bothContext(descDf, "LPelvisAngles","Z", "RPelvisAngles","Z",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  
  Hip_X = descriptivePlot_bothContext(descDf, "LHipAngles","X", "RHipAngles","X",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-20,70))  
  
  Hip_Y = descriptivePlot_bothContext(descDf, "LHipAngles","Y", "RHipAngles","Y",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  Hip_Z = descriptivePlot_bothContext(descDf, "LHipAngles","Z", "RHipAngles","Z",
                                         iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  
  
  Knee_X = descriptivePlot_bothContext(descDf, "LKneeAngles","X", "RKneeAngles","X",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-15,75))  
  
  Knee_Y = descriptivePlot_bothContext(descDf, "LKneeAngles","Y", "RKneeAngles","Y",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  Knee_Z = descriptivePlot_bothContext(descDf, "LKneeAngles","Z", "RKneeAngles","Z",
                                      iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  

  Ankle_X = descriptivePlot_bothContext(descDf, "LAnkleAngles","X", "RAnkleAngles","X",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  Ankle_Y = descriptivePlot_bothContext(descDf, "LAnkleAngles","Y", "RAnkleAngles","Y",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  
  
  FootProgress_Z = descriptivePlot_bothContext(descDf, "LFootProgressAngles","Z", "RFootProgressAngles","Z",
                                       iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(-30,30))  

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
                                         colorFactor=NULL, linetypeFactor=NULL){
  
  
  
  # trace uniquement UN context  
  
  if (iContext== "Left"){
    prefixe = "L"
  } else if (iContext== "Right"){
    prefixe = "R"
  } else if (iContext== "Overall"){
    prefixe = ""}
  

  Hip_X = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"X", 
                             iTitle="Hip extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-2.0,3.0),
                             colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Hip_Y = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"Y", 
                          iTitle="Hip abductor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,2.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)

  Hip_Z = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipMoment"),"Z", 
                          iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Hip_Power = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"HipPower"),"Z", 
                          iTitle="Hip power",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-3.0,3.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  

  Knee_X = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"X", 
                          iTitle="Knee extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,1.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Knee_Y = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"Y", 
                          iTitle="Knee abductor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,1.0),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Knee_Z = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneeMoment"),"Z", 
                          iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5),
                          colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Knee_Power = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"KneePower"),"Z", 
                              iTitle="Knee power",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-3.0,3.0),
                              colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  
  
  Ankle_X = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"X", 
                           iTitle="Ankle extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,3.0),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Ankle_Y = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"Y", 
                           iTitle="Ankle abductor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Ankle_Z = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnkleMoment"),"Z", 
                           iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5),
                           colorFactor = colorFactor,linetypeFactor = linetypeFactor, facetFactor = NULL)
  
  Ankle_Power = descriptivePlot(descDf, iSubjects, "Overall" , paste0(prefixe,"AnklePower"),"Z", 
                               iTitle="Ankle power",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-2.0,5.0),
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
  
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_power,ncol=4)
  
  legend_shared <- get_legend(Hip_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))
  
  fig = plot_grid(p1,p2,p3,legend_shared, 
                  nrow = 4, rel_heights = c(1,1,1,.2))
  
  fig
  
  return(fig)
  
    
  
}


descriptiveKineticGaitPanel_both<- function(descDf,descEvents=NULL){
  # 
  
  
  Hip_X = descriptivePlot_bothContext(descDf, "LHipMoment","X", "RHipMoment","X",
                                         
                                         iTitle="Hip extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-2.0,3.0))  
  
  Hip_Y = descriptivePlot_bothContext(descDf, "LHipMoment","Y", "RHipMoment","Y",
                                         
                                         iTitle="Hip abductor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,2.0))  
  
  Hip_Z = descriptivePlot_bothContext(descDf, "LHipMoment","Z", "RHipMoment","Z",
                                         
                                         iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5))  
  
  Hip_Power = descriptivePlot_bothContext(descDf, "LHipMoment","Z", "RHipMoment","Z",
                                      
                                      iTitle="Hip rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-3.0,3.0))  

  
  Knee_X = descriptivePlot_bothContext(descDf, "LKneeMoment","X", "RKneeMoment","X",
                                      
                                      iTitle="Knee extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,1.0))  
                                      
  Knee_Y = descriptivePlot_bothContext(descDf, "LKneeMoment","Y", "RKneeMoment","Y",
                                      
                                      iTitle="Knee abductor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,1.0))  
  
  Knee_Z = descriptivePlot_bothContext(descDf, "LKneeMoment","Z", "RKneeMoment","Z",
                                      
                                      iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5))  
  
  Knee_Power = descriptivePlot_bothContext(descDf, "LKneeMoment","Z", "RKneeMoment","Z",
                                          
                                          iTitle="Knee rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-3.0,3.0))  
                                      
  Ankle_X = descriptivePlot_bothContext(descDf, "LAnkleMoment","X", "RAnkleMoment","X",
                                      
  iTitle="Ankle extensor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-1.0,3.0))  
  
  Ankle_Y = descriptivePlot_bothContext(descDf, "LAnkleMoment","Y", "RAnkleMoment","Y",
                                      
                                      iTitle="Ankle abductor moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5))  
  
  Ankle_Z = descriptivePlot_bothContext(descDf, "LAnkleMoment","Z", "RAnkleMoment","Z",
                                      
                                      iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-0.5,0.5))  
  
  Ankle_Power = descriptivePlot_bothContext(descDf, "LAnkleMoment","Z", "RAnkleMoment","Z",
                                          
                                          iTitle="Ankle rotator moment",yLabel="N.m.kg-1", legendPosition="top",ylimits=c(-5.0,5.0))  
  
  
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
  
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_Power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_Power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_Power,ncol=4)
  
  legend_shared <- get_legend(Hip_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))
  
  fig = plot_grid(p1,p2,p3,legend_shared, 
                  nrow = 4, rel_heights = c(1,1,1,.2))
  
  fig
  
  return(fig)
  
}
  
  

### OLD ####
gaitPlot<-function(gatherFramesDescTbl,iContext, iLabel,iAxis,iTitle="",yLabel="Deg",
                   scaleToMeter=1,
                   ylimits=NULL,
                   iGatherPhaseDescTable=NULL,
                   blackPhaseColor=FALSE,
                   greyPalette = NULL,
                   horizontalLines=NULL){


  iData = filter(gatherFramesDescTbl, Label %in% iLabel & Axis ==iAxis & Context %in% iContext)  
  
  fig = ggplot() + 
        xlab("Time Normalized")+
        ylab(yLabel)+
        ggtitle(iTitle)+
        theme(panel.grid.minor = element_blank(),
              axis.title.x =      element_text(size = 10),
              axis.text.x  = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.title.y =  element_text(size = 10),
              axis.text.y  = element_text(size=8),
              legend.text = element_text(size = 8),
              legend.title=element_blank(),
              plot.title = element_text(family="Times", face="plain", size=10),
              legend.position='None') 
  
  if (all(iContext == c("Left","Right"))){
    fig = fig + geom_line(data=iData,
                          aes(x=Frame,y=MeanValues/scaleToMeter,
                              color= Context,
                              linetype=ComparisonFactor,
                              group=interaction(ComparisonFactor,Label,Context,Axis)),
                          size=0.5)
    fig = fig  + scale_color_manual(values=c("red", "blue"))
    
  } else {
    fig =  fig +geom_line(data=iData,
              aes(x=Frame,y=MeanValues/scaleToMeter,
                  color=ComparisonFactor,
                  group=interaction(ComparisonFactor,Label,Context,Axis)),
              size=1)
    
  }
  
  if (!is.null(iGatherPhaseDescTable)){
    
    if (iContext == "Both"){
      
      fig = fig + geom_vline( data = filter(iGatherPhaseDescTable,Factor == "stancePhase"),
                              aes(xintercept=Mean,
                                  color = Context,
                                  linetype=ComparisonFactor,
                                  group=interaction(ComparisonFactor,Context)))
      
      
    } else if (iContext == "Overall") {
      
      if (!blackPhaseColor){
        fig = fig + geom_vline( data = filter(iGatherPhaseDescTable,Factor == "stancePhase"),
                                aes(xintercept=Mean,
                                    color=ComparisonFactor))
      } else {
        fig = fig + geom_vline( data = filter(iGatherPhaseDescTable,Factor == "stancePhase"),
                                aes(xintercept=Mean), color= "black")
      }  
      
    } else {
      fig = fig + geom_vline( data = filter(iGatherPhaseDescTable,Factor == "stancePhase"),
                              aes(xintercept=Mean,
                                  color=ComparisonFactor,
                                  group=interaction(ComparisonFactor,Context)))
        
    }
    
    
  }

  
  if (!is.null(ylimits)){ fig = fig + ylim(ylimits[1],ylimits[2])}  
  if (!is.null(horizontalLines)){ 
    
    for (i in horizontalLines){
      fig = fig + geom_hline(yintercept=i)
    }
  }

  
  fig  

  
  return(fig)
  
}


kinematicGaitPanel<- function(gatherFramesDescTbl,iContext, iGatherPhaseDescTable=NULL,blackPhaseColorFlag=FALSE,greyPalette=FALSE){
  
  
  if (iContext == "Overall") {
  
    Pelvis_X = gaitPlot(gatherFramesDescTbl,iContext,c("PelvisAngles"),"X", iTitle="Pelvic tilt",ylimits = c(0,60),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Y = gaitPlot(gatherFramesDescTbl,iContext,c("PelvisAngles"),"Y",iTitle="Pelvic obliquity", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Z = gaitPlot(gatherFramesDescTbl,iContext,c("PelvisAngles"),"Z",iTitle="Pelvic rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext, c("HipAngles"),"X",iTitle="Hip flexion",ylimits = c(-20,70),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c("HipAngles"),"Y",iTitle="Hip adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c("HipAngles"),"Z",iTitle="Hip rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c("KneeAngles"),"X",iTitle="Knee flexion",ylimits = c(-15,75), iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c("KneeAngles"),"Y",iTitle="Knee adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c("KneeAngles"),"Z",iTitle="Knee rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleAngles"),"X",iTitle="Ankle dorsiflexion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleAngles"),"Y",iTitle="Ankle eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    FootProgress_Z = gaitPlot(gatherFramesDescTbl,iContext,c("FootProgressAngles" ),"Z",iTitle="Foot progression", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
  
    } else if (iContext == "Both"){ 
  
      Pelvis_X = gaitPlot(gatherFramesDescTbl,iContext,c("LPelvisAngles","RPelvisAngles"),"X", iTitle="Pelvic tilt",ylimits = c(0,60),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Pelvis_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LPelvisAngles","RPelvisAngles"),"Y",iTitle="Pelvic obliquity", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Pelvis_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LPelvisAngles","RPelvisAngles"),"Z",iTitle="Pelvic rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      
      Hip_X = gaitPlot(gatherFramesDescTbl,iContext, c("LHipAngles","RHipAngles"),"X",iTitle="Hip flexion",ylimits = c(-20,70),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LHipAngles","RHipAngles"),"Y",iTitle="Hip adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LHipAngles","RHipAngles"),"Z",iTitle="Hip rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      
      Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeAngles","RKneeAngles"),"X",iTitle="Knee flexion",ylimits = c(-15,75),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeAngles","RKneeAngles"),"Y",iTitle="Knee adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeAngles","RKneeAngles"),"Z",iTitle="Knee rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      
      Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleAngles","RAnkleAngles"),"X",iTitle="Ankle dorsiflexion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleAngles","RAnkleAngles"),"Y",iTitle="Ankle eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
      FootProgress_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LFootProgressAngles","RFootProgressAngles" ),"Z",iTitle="Foot progression", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    

  }  else {
    
    if (iContext == "Left"){LabelPrefix = "L"}
    else if (iContext == "Right"){LabelPrefix = "R"}
  
    Pelvis_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"PelvisAngles",sep="")),"X",iContext, iTitle="Pelvic tilt",ylimits = c(0,60),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"PelvisAngles",sep="")),"Y",iContext,iTitle="Pelvic obliquity", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"PelvisAngles",sep="")),"Z",iContext,iTitle="Pelvic rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipAngles",sep="")),"X",iContext,iTitle="Hip flexion",ylimits = c(-20,70),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipAngles",sep="")),"Y",iContext,iTitle="Hip adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipAngles",sep="")),"Z",iContext,iTitle="Hip rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeAngles",sep="")),"X",iContext,iTitle="Knee flexion",ylimits = c(-15,75),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeAngles",sep="")),"Y",iContext,iTitle="Knee adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeAngles",sep="")),"Z",iContext,iTitle="Knee rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,c(paste(LabelPrefix,"AnkleAngles",sep="")),"X",iContext,iTitle="Ankle dorsiflexion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_Y = gaitPlot(gatherFramesDescTbl,c(paste(LabelPrefix,"AnkleAngles",sep="")),"Y",iContext,iTitle="Ankle eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    FootProgress_Z = gaitPlot(gatherFramesDescTbl,c(paste(LabelPrefix,"FootProgressAngles",sep="") ),"Z",iContext,iTitle="Foot progression", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
  }
  
  
  if (greyPalette){
    Pelvis_X = Pelvis_X+scale_colour_grey()
    Pelvis_Y = Pelvis_Y+scale_colour_grey()
    Pelvis_Z = Pelvis_Z+scale_colour_grey()
    Hip_X = Hip_X+scale_colour_grey()
    Hip_Y = Hip_Y+scale_colour_grey()
    Hip_Z = Hip_Z+scale_colour_grey()
    Knee_X = Knee_X+scale_colour_grey()
    Knee_Y = Knee_Y+scale_colour_grey()
    Knee_Z = Knee_Z+scale_colour_grey()
    Ankle_X = Ankle_X+scale_colour_grey()
    Ankle_Y = Ankle_Y+scale_colour_grey()
    FootProgress_Z = FootProgress_Z+scale_colour_grey()
  }
  
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)
  

  legend_shared <- get_legend(Pelvis_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))
#                                theme(legend.position="center"))
  
  fig = plot_grid(p1, p2,p3, p4,
            legend_shared,
            nrow = 5,rel_heights = c(1,1,1,1, .2))  
  
  fig
  
  return(fig)
  
  }


kineticGaitPanel<- function(gatherFramesDescTbl,iContext,scaleToMeter=1000,iGatherPhaseDescTable=NULL,blackPhaseColorFlag=FALSE,greyPalette=FALSE){
  
  
  if (iContext == "Overall") {
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext,c("HipMoment"),"X",iTitle="Hip extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-2.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c("HipMoment"),"Y",iTitle="Hip abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 2.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c("HipMoment"),"Z",iTitle="Hip rotation moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_power = gaitPlot(gatherFramesDescTbl,iContext,c("HipPower"),"Z",iTitle="Hip power",yLabel="W.kg-1",ylimits = c(-3.0, 3.0), iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag) 
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c("KneeMoment"),"X",iTitle="Knee extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 1.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c("KneeMoment"),"Y",iTitle="Knee abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 1.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c("KneeMoment"),"Z",iTitle="Knee rotation moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_power = gaitPlot(gatherFramesDescTbl,iContext,c("KneePower"),"Z",iTitle="Knee power",yLabel="W.kg-1",ylimits = c(-3.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag) 
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleMoment"),"X",iTitle="Ankle extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleMoment"),"Y",iTitle="Ankle  evertor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Ankle_Z = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleMoment"),"Z",iTitle="Ankle abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_power = gaitPlot(gatherFramesDescTbl,iContext,c("AnklePower"),"Z",iTitle="Ankle power",yLabel="W.kg-1",ylimits = c(-2.0, 5.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    
    
  } else if (iContext == "Both"){ 
    
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext,c("LHipMoment","RHipMoment"),"X",iTitle="Hip extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-2.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LHipMoment","RHipMoment"),"Y",iTitle="Hip abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 2.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LHipMoment","RHipMoment"),"Z",iTitle="Hip rotation moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_power = gaitPlot(gatherFramesDescTbl,iContext,c("LHipPower","RHipPower"),"Z",iTitle="Hip power",yLabel="W.kg-1",ylimits = c(-3.0, 3.0), iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag) 
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeMoment","RKneeMoment"),"X",iTitle="Knee extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 1.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeMoment","RKneeMoment"),"Y",iTitle="Knee abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 1.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeMoment","RKneeMoment"),"Z",iTitle="Knee rotation moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_power = gaitPlot(gatherFramesDescTbl,iContext,c("LKneePower","RKneePower"),"Z",iTitle="Knee power",yLabel="W.kg-1",ylimits = c(-3.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag) 
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleMoment","RAnkleMoment"),"X",iTitle="Ankle extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleMoment","RAnkleMoment"),"Y",iTitle="Ankle  evertor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Ankle_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleMoment","RAnkleMoment"),"Z",iTitle="Ankle abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_power = gaitPlot(gatherFramesDescTbl,iContext,c("LAnklePower","RAnklePower"),"Z",iTitle="Ankle power",yLabel="W.kg-1",ylimits = c(-2.0, 5.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    

  } else {
    
    if (iContext == "Left"){
      LabelPrefix = "L"
    }
    else if (iContext == "Right"){LabelPrefix = "R"}
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipMoment",sep="")),"X",iContext,iTitle="Hip extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-2.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipMoment",sep="")),"Y",iContext,iTitle="Hip abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 2.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipMoment",sep="")),"Z",iContext,iTitle="Hip rotation moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_power = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipPower",sep="")),"Z",iContext,iTitle="Hip power",yLabel="W.kg-1",ylimits = c(-3.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag) 
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeMoment",sep="")),"X",iContext,iTitle="Knee extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 1.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeMoment",sep="")),"Y",iContext,iTitle="Knee abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 1.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeMoment",sep="")),"Z",iContext,iTitle="Knee rotation moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_power = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneePower",sep="")),"Z",iContext,iTitle="Knee power",yLabel="W.kg-1",ylimits = c(-3.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag) 
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"AnkleMoment",sep="")),"X",iContext,iTitle="Ankle extensor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-1.0, 3.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"AnkleMoment",sep="")),"Y",iContext,iTitle="Ankle evertor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    Ankle_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"AnkleMoment",sep="")),"Z",iContext,iTitle="Ankle abductor moment",yLabel="N.m.kg-1",scaleToMeter=scaleToMeter,ylimits = c(-0.5, 0.5),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_power = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"AnklePower",sep="")),"Z",iContext,iTitle="Ankle power",yLabel="W.kg-1",ylimits = c(-2.0, 5.0),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)  
    
  }

  if (greyPalette){
    Hip_X = Hip_X+scale_colour_grey()
    Hip_Y = Hip_Y+scale_colour_grey()
    Hip_Z = Hip_Z+scale_colour_grey()
    Hip_power = Hip_power+scale_colour_grey()
    Knee_X = Knee_X+scale_colour_grey()
    Knee_Y = Knee_Y+scale_colour_grey()
    Knee_Z = Knee_Z+scale_colour_grey()
    Knee_power = Knee_power+scale_colour_grey()
    Ankle_X = Ankle_X+scale_colour_grey()
    Ankle_Y = Ankle_Y+scale_colour_grey()
    Ankle_Z = Ankle_Z+scale_colour_grey()
    Ankle_power = Ankle_power+scale_colour_grey()
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



kinematicGaitPanel2<- function(gatherFramesDescTbl,iContext, iGatherPhaseDescTable=NULL,blackPhaseColorFlag=FALSE,greyPalette=FALSE){
  
  
  if (iContext == "Overall") {
    
    Pelvis_X = gaitPlot(gatherFramesDescTbl,iContext,c("PelvisAngles"),"X", iTitle="Pelvic tilt",ylimits = c(0,60),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Y = gaitPlot(gatherFramesDescTbl,iContext,c("PelvisAngles"),"Y",iTitle="Pelvic obliquity", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Z = gaitPlot(gatherFramesDescTbl,iContext,c("PelvisAngles"),"Z",iTitle="Pelvic rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext, c("HipAngles"),"X",iTitle="Hip flexion",ylimits = c(-20,70),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c("HipAngles"),"Y",iTitle="Hip adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c("HipAngles"),"Z",iTitle="Hip rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c("KneeAngles"),"X",iTitle="Knee flexion",ylimits = c(-15,75), iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c("KneeAngles"),"Y",iTitle="Knee adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c("KneeAngles"),"Z",iTitle="Knee rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleAngles"),"X",iTitle="Ankle dorsiflexion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c("AnkleAngles"),"Y",iTitle="Ankle eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    FootProgress_Z = gaitPlot(gatherFramesDescTbl,iContext,c("FootProgressAngles" ),"Z",iTitle="Foot progression", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)

    ForeFoot_X = gaitPlot(gatherFramesDescTbl,iContext,c("ForeFootAngles"),"X",iTitle="ForeFoot dorsiflexion", ylimits =c(-50,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    ForeFoot_Y = gaitPlot(gatherFramesDescTbl,iContext,c("ForeFootAngles"),"Y",iTitle="ForeFoot eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    ForeFoot_Z = gaitPlot(gatherFramesDescTbl,iContext,c("ForeFootAngles" ),"Z",iTitle="ForeFoot abduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)    
    
        
  } else if (iContext == "Both"){ 
    
    Pelvis_X = gaitPlot(gatherFramesDescTbl,iContext,c("LPelvisAngles","RPelvisAngles"),"X", iTitle="Pelvic tilt",ylimits = c(0,60),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LPelvisAngles","RPelvisAngles"),"Y",iTitle="Pelvic obliquity", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LPelvisAngles","RPelvisAngles"),"Z",iTitle="Pelvic rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext, c("LHipAngles","RHipAngles"),"X",iTitle="Hip flexion",ylimits = c(-20,70),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LHipAngles","RHipAngles"),"Y",iTitle="Hip adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LHipAngles","RHipAngles"),"Z",iTitle="Hip rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeAngles","RKneeAngles"),"X",iTitle="Knee flexion",ylimits = c(-15,75),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeAngles","RKneeAngles"),"Y",iTitle="Knee adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LKneeAngles","RKneeAngles"),"Z",iTitle="Knee rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleAngles","RAnkleAngles"),"X",iTitle="Ankle dorsiflexion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LAnkleAngles","RAnkleAngles"),"Y",iTitle="Ankle eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    FootProgress_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LFootProgressAngles","RFootProgressAngles" ),"Z",iTitle="Foot progression", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    ForeFoot_X = gaitPlot(gatherFramesDescTbl,iContext,c("LForeFootAngles","RForeFootAngles"),"X",iTitle="ForeFoot dorsiflexion", ylimits =c(-50,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    ForeFoot_Y = gaitPlot(gatherFramesDescTbl,iContext,c("LForeFootAngles","RForeFootAngles"),"Y",iTitle="ForeFoot eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    ForeFoot_Z = gaitPlot(gatherFramesDescTbl,iContext,c("LForeFootAngles","RForeFootAngles"),"Z",iTitle="ForeFoot abduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)    
    
    
        
  }  else {
    
    if (iContext == "Left"){LabelPrefix = "L"}
    else if (iContext == "Right"){LabelPrefix = "R"}
    
    Pelvis_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"PelvisAngles",sep="")),"X",iContext, iTitle="Pelvic tilt",ylimits = c(0,60),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"PelvisAngles",sep="")),"Y",iContext,iTitle="Pelvic obliquity", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Pelvis_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"PelvisAngles",sep="")),"Z",iContext,iTitle="Pelvic rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Hip_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipAngles",sep="")),"X",iContext,iTitle="Hip flexion",ylimits = c(-20,70),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipAngles",sep="")),"Y",iContext,iTitle="Hip adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Hip_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"HipAngles",sep="")),"Z",iContext,iTitle="Hip rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Knee_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeAngles",sep="")),"X",iContext,iTitle="Knee flexion",ylimits = c(-15,75),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeAngles",sep="")),"Y",iContext,iTitle="Knee adduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Knee_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste(LabelPrefix,"KneeAngles",sep="")),"Z",iContext,iTitle="Knee rotation", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    
    Ankle_X = gaitPlot(gatherFramesDescTbl,c(paste(LabelPrefix,"AnkleAngles",sep="")),"X",iContext,iTitle="Ankle dorsiflexion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    Ankle_Y = gaitPlot(gatherFramesDescTbl,c(paste(LabelPrefix,"AnkleAngles",sep="")),"Y",iContext,iTitle="Ankle eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    FootProgress_Z = gaitPlot(gatherFramesDescTbl,c(paste(LabelPrefix,"FootProgressAngles",sep="") ),"Z",iContext,iTitle="Foot progression", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)

    ForeFoot_X = gaitPlot(gatherFramesDescTbl,iContext,c(paste0(LabelPrefix,"ForeFootAngles")),"X",iTitle="ForeFoot dorsiflexion", ylimits =c(-50,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    ForeFoot_Y = gaitPlot(gatherFramesDescTbl,iContext,c(paste0(LabelPrefix,"ForeFootAngles")),"Y",iTitle="ForeFoot eversion", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)
    ForeFoot_Z = gaitPlot(gatherFramesDescTbl,iContext,c(paste0(LabelPrefix,"ForeFootAngles" )),"Z",iTitle="ForeFoot abduction", ylimits =c(-30,30),iGatherPhaseDescTable=iGatherPhaseDescTable,blackPhaseColor=blackPhaseColorFlag)    
    
    
    
      }
  
  
  if (greyPalette){
    Pelvis_X = Pelvis_X+scale_colour_grey()
    Pelvis_Y = Pelvis_Y+scale_colour_grey()
    Pelvis_Z = Pelvis_Z+scale_colour_grey()
    Hip_X = Hip_X+scale_colour_grey()
    Hip_Y = Hip_Y+scale_colour_grey()
    Hip_Z = Hip_Z+scale_colour_grey()
    Knee_X = Knee_X+scale_colour_grey()
    Knee_Y = Knee_Y+scale_colour_grey()
    Knee_Z = Knee_Z+scale_colour_grey()
    Ankle_X = Ankle_X+scale_colour_grey()
    Ankle_Y = Ankle_Y+scale_colour_grey()
    FootProgress_Z = FootProgress_Z+scale_colour_grey()

    ForeFoot_X = ForeFoot_X+scale_colour_grey()
    ForeFoot_Y = ForeFoot_Y+scale_colour_grey()
    ForeFoot_Z = ForeFoot_Z+scale_colour_grey()
    
      }
  
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)
  p5 = plot_grid(ForeFoot_X, ForeFoot_Y,ForeFoot_Z,ncol=3)
  
  legend_shared <- get_legend(Pelvis_X +  theme(legend.position=c(0.3,0.8),legend.direction = "horizontal"))
  #                                theme(legend.position="center"))
  
  fig = plot_grid(p1, p2,p3, p4,p5,
                  legend_shared,
                  nrow = 6,rel_heights = c(1,1,1,1,1, .2))  
  
  fig
  
  return(fig)
  
}




### OLD ####
plot_onCompararison_bothContexts<- function(tableStatsFull,iLabel,iAxis,iTitle="",yLabel="Deg"){
  

  
  data_X = filter(tableStatsFull, Label %in% iLabel & Axis ==iAxis)  
  fig = ggplot() + geom_line(data=data_X,
                             aes(x=Frame,y=MeanValues,
                                 color=ComparisonFactor,linetype=Context,
                                 group=interaction(ComparisonFactor,Label,Context,Axis)),
                             size=0.5)+
    scale_x_discrete(name="Time Normalized")+
    scale_y_continuous(name=yLabel)+
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
          legend.position='None')
  fig
  return(fig)
}

plot_onCompararison_oneContext<- function(tableStatsFull,iLabel,iAxis,iContext, iTitle="",yLabel="Deg",legendPos="None"){
  
  

  data_X = filter(tableStatsFull, Label %in% iLabel & Axis ==iAxis & Context ==iContext)  
  fig = ggplot() + geom_line(data=data_X,
                             aes(x=Frame,y=MeanValues,
                                 color=ComparisonFactor,
                                 group=interaction(ComparisonFactor,Label,Context,Axis)),
                             size=1)+
    scale_x_discrete(name="Time Normalized")+
    scale_y_continuous(name=yLabel)+
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
          legend.position=legendPos)
  fig
  
  return(fig)
}

plot_onCompararison_overall<- function(gatherFramesTbl,iGlobalLabel,iAxis, iTitle="",yLabel="Deg", iStancePhase=0){
  
  
  
  data_X = filter(gatherFramesTbl, GlobalLabel %in% iGlobalLabel & Axis ==iAxis)  
  fig = ggplot() + geom_line(data=data_X,
                             aes(x=Frame,y=MeanValues,
                                 color=ComparisonFactor,
                                 group=interaction(ComparisonFactor,GlobalLabel,Axis)),
                             size=0.5)+
    scale_x_discrete(name="Time Normalized")+
    scale_y_continuous(name=yLabel)+
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
          legend.position='None')
  
  if (iStancePhase != 0){fig = fig + geom_vline(xintercept=iStancePhase)}
  
  
  
  return(fig)
}

# KINEMATIC PLOTS -----------------------------------------------------------


kinematicGaitPlots_onComparison_bothContexts<- function(tableStatsFull){
  
  
  Pelvis_X = plot_onCompararison_bothContexts(tableStatsFull,c("LPelvisAngles","RPelvisAngles"),"X",iTitle="Pelvic tilt")
  Pelvis_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LPelvisAngles","RPelvisAngles"),"Y",iTitle="Pelvic obliquity")
  Pelvis_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LPelvisAngles","RPelvisAngles"),"Z",iTitle="Pelvic rotation")
  
  Hip_X = plot_onCompararison_bothContexts(tableStatsFull,c("LHipAngles","RHipAngles"),"X",iTitle="Hip flexion")
  Hip_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LHipAngles","RHipAngles"),"Y",iTitle="Hip adduction")
  Hip_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LHipAngles","RHipAngles"),"Z",iTitle="Hip rotation")
  
  Knee_X = plot_onCompararison_bothContexts(tableStatsFull,c("LKneeAngles","RKneeAngles"),"X",iTitle="Knee flexion")
  Knee_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LKneeAngles","RKneeAngles"),"Y",iTitle="Knee adduction")
  Knee_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LKneeAngles","RKneeAngles"),"Z",iTitle="Knee rotation")
  
  Ankle_X = plot_onCompararison_bothContexts(tableStatsFull,c("LAnkleAngles","RAnkleAngles"),"X",iTitle="Ankle dorsiflexion")
  Ankle_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LAnkleAngles","RAnkleAngles"),"Y",iTitle="Ankle eversion")
  FootProgress_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LFootProgressAngles","RFootProgressAngles"),"Z",iTitle="Foot progression")
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Z,FootProgress_Z,ncol=3)
  
  legend_shared <- get_legend(Pelvis_X + theme(legend.position="bottom"))
  
  plot_grid(p1, p2,p3, p4,
            legend_shared,
            nrow = 5,rel_heights = c(1,1,1,1, .2))  

}


kinematicGaitPlots_onComparison_oneContext<- function(tableStatsFull,iContext){
  
  if (iContext == "Left"){
    LabelPrefix = "L"
  }
  else if (iContext == "Right"){LabelPrefix = "R"}
  
    
  
  
  Pelvis_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"PelvisAngles",sep="")),"X",iContext, iTitle="Pelvic tilt")
  Pelvis_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"PelvisAngles",sep="")),"Y",iContext,iTitle="Pelvic obliquity")
  Pelvis_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"PelvisAngles",sep="")),"Z",iContext,iTitle="Pelvic rotation")
  
  Hip_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipAngles",sep="")),"X",iContext,iTitle="Hip flexion")
  Hip_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipAngles",sep="")),"Y",iContext,iTitle="Hip adduction")
  Hip_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipAngles",sep="")),"Z",iContext,iTitle="Hip rotation")
  
  Knee_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneeAngles",sep="")),"X",iContext,iTitle="Knee flexion")
  Knee_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneeAngles",sep="")),"Y",iContext,iTitle="Knee adduction")
  Knee_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneeAngles",sep="")),"Z",iContext,iTitle="Knee rotation")
  
  Ankle_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"AnkleAngles",sep="")),"X",iContext,iTitle="Ankle dorsiflexion")
  Ankle_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"AnkleAngles",sep="")),"Y",iContext,iTitle="Ankle eversion")
  FootProgress_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"FootProgressAngles",sep="") ),"Z",iContext,iTitle="Foot progression")
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)
  
  legend_shared <- get_legend(Pelvis_X + theme(legend.position="bottom"))
  
  plot_grid(p1, p2,p3, p4,
            legend_shared,
            nrow = 5,rel_heights = c(1,1,1,1, .2))  
}

kinematicGaitPlots_onComparison_overall<- function(gatherFrameTbl,iStancePhase=0){
  

  Pelvis_X = plot_onCompararison_overall(gatherFrameTbl,c("PelvisAngles"),"X", iTitle="Pelvic tilt",iStancePhase=iStancePhase)
  Pelvis_Y = plot_onCompararison_overall(gatherFrameTbl,c("PelvisAngles"),"Y",iTitle="Pelvic obliquity",iStancePhase=iStancePhase)
  Pelvis_Z = plot_onCompararison_overall(gatherFrameTbl,c("PelvisAngles"),"Z",iTitle="Pelvic rotation",iStancePhase=iStancePhase)
  
  Hip_X = plot_onCompararison_overall(gatherFrameTbl,c("HipAngles"),"X",iTitle="Hip flexion",iStancePhase=iStancePhase)
  Hip_Y = plot_onCompararison_overall(gatherFrameTbl,c("HipAngles"),"Y",iTitle="Hip adduction",iStancePhase=iStancePhase)
  Hip_Z = plot_onCompararison_overall(gatherFrameTbl,c("HipAngles"),"Z",iTitle="Hip rotation",iStancePhase=iStancePhase)
  
  Knee_X = plot_onCompararison_overall(gatherFrameTbl,c("KneeAngles"),"X",iTitle="Knee flexion",iStancePhase=iStancePhase)
  Knee_Y = plot_onCompararison_overall(gatherFrameTbl,c("KneeAngles"),"Y",iTitle="Knee adduction",iStancePhase=iStancePhase)
  Knee_Z = plot_onCompararison_overall(gatherFrameTbl,c("KneeAngles"),"Z",iTitle="Knee rotation",iStancePhase=iStancePhase)
  
  Ankle_X = plot_onCompararison_overall(gatherFrameTbl,c("AnkleAngles"),"X",iTitle="Ankle dorsiflexion",iStancePhase=iStancePhase)
  Ankle_Y = plot_onCompararison_overall(gatherFrameTbl,c("AnkleAngles"),"Y",iTitle="Ankle eversion",iStancePhase=iStancePhase)
  FootProgress_Z = plot_onCompararison_overall(gatherFrameTbl,c("FootProgressAngles" ),"Z",iTitle="Foot progression",iStancePhase=iStancePhase)
  
  p1 = plot_grid(Pelvis_X, Pelvis_Y,Pelvis_Z,ncol=3)
  p2 = plot_grid(Hip_X, Hip_Y,Hip_Z,ncol=3)
  p3 = plot_grid(Knee_X, Knee_Y,Knee_Z,ncol=3)
  p4 = plot_grid(Ankle_X, Ankle_Y,FootProgress_Z,ncol=3)
  
  legend_shared <- get_legend(Pelvis_X + theme(legend.position="bottom"))
  
  plot_grid(p1, p2,p3, p4,
            legend_shared,
            nrow = 5,rel_heights = c(1,1,1,1, .2))  
}

# KINETIC PLOTS -----------------------------------------------------------


  
kineticGaitPlots_onComparison_bothContexts<- function(tableStatsFull){
  
  
  Hip_X = plot_onCompararison_bothContexts(tableStatsFull,c("LHipMoment","RHipMoment"),"X",iTitle="Hip extensor moment",yLabel="N.mm.kg-1")  
  Hip_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LHipMoment","RHipMoment"),"Y",iTitle="Hip abductor moment",yLabel="N.mm.kg-1")  
  Hip_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LHipMoment","RHipMoment"),"Z",iTitle="Hip rotation moment",yLabel="N.mm.kg-1")
  Hip_power = plot_onCompararison_bothContexts(tableStatsFull,c("LHipPower","RHipPower"),"Z",iTitle="Hip power",yLabel="W.kg-1") 
  
  Knee_X = plot_onCompararison_bothContexts(tableStatsFull,c("LKneeMoment","RKneeMoment"),"X",iTitle="Knee extensor moment",yLabel="N.mm.kg-1")  
  Knee_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LKneeMoment","RKneeMoment"),"Y",iTitle="Knee abductor moment",yLabel="N.mm.kg-1")  
  Knee_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LKneeMoment","RKneeMoment"),"Z",iTitle="Knee rotation moment",yLabel="N.mm.kg-1")
  Knee_power = plot_onCompararison_bothContexts(tableStatsFull,c("LKneePower","RKneePower"),"Z",iTitle="Knee power",yLabel="W.kg-1") 
  
  Ankle_X = plot_onCompararison_bothContexts(tableStatsFull,c("LAnkleMoment","RAnkleMoment"),"X",iTitle="Ankle extensor moment",yLabel="N.mm.kg-1")  
  Ankle_Y = plot_onCompararison_bothContexts(tableStatsFull,c("LAnkleMoment","RAnkleMoment"),"Y",iTitle="Ankle evertor moment",yLabel="N.mm.kg-1")  
  Ankle_Z = plot_onCompararison_bothContexts(tableStatsFull,c("LAnkleMoment","RAnkleMoment"),"Z",iTitle="Ankle abductor moment",yLabel="N.mm.kg-1")
  Ankle_power = plot_onCompararison_bothContexts(tableStatsFull,c("LAnklePower","RAnklePower"),"Z",iTitle="Ankle power",yLabel="W.kg-1")  
  
  
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_power,ncol=4)
  
  #p1
  
  
  legend_shared <- get_legend(Hip_X + theme(legend.position="bottom"))
  # 
  plot_grid(p1,p2,p3,legend_shared, 
            nrow = 4, rel_heights = c(1,1,1,.2))
  
  
  
} 

kineticGaitPlots_onComparison_oneContext<- function(tableStatsFull,iContext){
  
  if (iContext == "Left"){
    LabelPrefix = "L"
  }
  else if (iContext == "Right"){LabelPrefix = "R"}
  
  
  Hip_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipMoment",sep="")),"X",iContext,iTitle="Hip extensor moment",yLabel="N.mm.kg-1")  
  Hip_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipMoment",sep="")),"Y",iContext,iTitle="Hip abductor moment",yLabel="N.mm.kg-1")  
  Hip_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipMoment",sep="")),"Z",iContext,iTitle="Hip rotation moment",yLabel="N.mm.kg-1")
  Hip_power = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"HipPower",sep="")),"Z",iContext,iTitle="Hip power",yLabel="W.kg-1") 
  
  Knee_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneeMoment",sep="")),"X",iContext,iTitle="Knee extensor moment",yLabel="N.mm.kg-1")  
  Knee_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneeMoment",sep="")),"Y",iContext,iTitle="Knee abductor moment",yLabel="N.mm.kg-1")  
  Knee_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneeMoment",sep="")),"Z",iContext,iTitle="Knee rotation moment",yLabel="N.mm.kg-1")
  Knee_power = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"KneePower",sep="")),"Z",iContext,iTitle="Knee power",yLabel="W.kg-1") 
  
  Ankle_X = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"AnkleMoment",sep="")),"X",iContext,iTitle="Ankle extensor moment",yLabel="N.mm.kg-1")  
  Ankle_Y = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"AnkleMoment",sep="")),"Y",iContext,iTitle="Ankle evertor moment",yLabel="N.mm.kg-1")  
  Ankle_Z = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"AnkleMoment",sep="")),"Z",iContext,iTitle="Ankle abductor moment",yLabel="N.mm.kg-1")
  Ankle_power = plot_onCompararison_oneContext(tableStatsFull,c(paste(LabelPrefix,"AnklePower",sep="")),"Z",iContext,iTitle="Ankle power",yLabel="W.kg-1")  
  
  
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_power,ncol=4)
  
  #p1
  
  
  legend_shared <- get_legend(Hip_X + theme(legend.position="bottom"))
  # 
  plot_grid(p1,p2,p3,legend_shared, 
            nrow = 4, rel_heights = c(1,1,1,.2))
  
}

kineticGaitPlots_onComparison_overall<- function(tableStatsFull){
  

  Hip_X = plot_onCompararison_overall(tableStatsFull,c("HipMoment"),"X",iTitle="Hip extensor moment",yLabel="N.mm.kg-1")  
  Hip_Y = plot_onCompararison_overall(tableStatsFull,c("HipMoment"),"Y",iTitle="Hip abductor moment",yLabel="N.mm.kg-1")  
  Hip_Z = plot_onCompararison_overall(tableStatsFull,c("HipMoment"),"Z",iTitle="Hip rotation moment",yLabel="N.mm.kg-1")
  Hip_power = plot_onCompararison_overall(tableStatsFull,c("HipPower"),"Z",iTitle="Hip power",yLabel="W.kg-1") 
  
  Knee_X = plot_onCompararison_overall(tableStatsFull,c("KneeMoment"),"X",iTitle="Knee extensor moment",yLabel="N.mm.kg-1")  
  Knee_Y = plot_onCompararison_overall(tableStatsFull,c("KneeMoment"),"Y",iTitle="Knee abductor moment",yLabel="N.mm.kg-1")  
  Knee_Z = plot_onCompararison_overall(tableStatsFull,c("KneeMoment"),"Z",iTitle="Knee rotation moment",yLabel="N.mm.kg-1")
  Knee_power = plot_onCompararison_overall(tableStatsFull,c("KneePower"),"Z",iTitle="Knee power",yLabel="W.kg-1") 
  
  Ankle_X = plot_onCompararison_overall(tableStatsFull,c("AnkleMoment"),"X",iTitle="Ankle extensor moment",yLabel="N.mm.kg-1")  
  Ankle_Y = plot_onCompararison_overall(tableStatsFull,c("AnkleMoment"),"Y",iTitle="Ankle  evertor moment",yLabel="N.mm.kg-1")  
  Ankle_Z = plot_onCompararison_overall(tableStatsFull,c("AnkleMoment"),"Z",iTitle="Ankle abductor moment",yLabel="N.mm.kg-1")
  Ankle_power = plot_onCompararison_overall(tableStatsFull,c("AnklePower"),"Z",iTitle="Ankle power",yLabel="W.kg-1")  
  
  
  
  p1 = plot_grid(Hip_X, Hip_Y,Hip_Z,Hip_power,ncol=4)
  p2 = plot_grid(Knee_X, Knee_Y,Knee_Z,Knee_power,ncol=4)
  p3 = plot_grid(Ankle_X, Ankle_Y,Ankle_Z,Ankle_power,ncol=4)
  
  #p1
  
  
  legend_shared <- get_legend(Hip_X + theme(legend.position="bottom"))
  # 
  plot_grid(p1,p2,p3,legend_shared, 
            nrow = 4, rel_heights = c(1,1,1,.2))
  
}
