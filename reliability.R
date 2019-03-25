ComputeSessionAverage<-function(df){
  outDf = df %>%
  group_by(Participant,Assessor,Session,Label,Axis,Context,Frames)%>%
  summarise(Avg = mean(Value,na.rm = TRUE))
  return(outDf)
}



withinSubjectStandardDeviation <-function(sadf,Anova=FALSE){
  # RIchard spreadsheet has been working with the sdtandard devation. 
  # uou might have a sligh difference with the anova.
  

  if (!Anova){ 
    frames = sadf %>%
    group_by(Participant,Label,Axis,Context,Frames)%>%
    summarize(Sem = sd(Avg))
  } else {
    frames = sadf %>%
      group_by(Participant,Label,Axis,Context,Frames)%>%
      do(anova = aov(Avg ~ Assessor, data=.))%>%
      mutate(Sem = sigma(anova))
  }
  
  
  framesAvg = frames %>%
    group_by(Participant,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem,na.rm = TRUE))

  
  return(list(atFrame=frames, overall = framesAvg))
}



withinOperatorStandardDeviation<-function(sadf,Anova=FALSE){
  # It s also the SEM for individual Assessor
  
  if (!Anova){ 

    frames = sadf %>%
      group_by(Participant,Assessor,Label,Axis,Context,Frames)%>%
      summarise(Sem = sd(Avg,na.rm = TRUE))
  } else  {
    
    frames = sadf %>%
      group_by(Participant,Assessor,Label,Axis,Context,Frames)%>%
      summarise(Sem = sd(Avg))
  }
  
  framesAvg = frames %>%
    group_by(Participant,Assessor,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem,na.rm = TRUE))
  
  
  return(list(atFrame=frames, overall = framesAvg))
  
}


Sem_byAssessor<-function(sadf,Anova=TRUE){
  
  if (Anova){
    frames = sadf %>%
      group_by(Assessor,Label,Axis,Context,Frames)%>%
      do(anova = aov(Avg ~ Participant, data=.))%>%
      mutate(Sem = sigma(anova,na.rm = TRUE))
    
    framesAvg = frames %>%
      group_by(Assessor,Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))
    
  
  } else {
    wod = withinOperatorStandardDeviation(sadf)
    colnames(wod$atFrame)[colnames(wod$atFrame)=="Sem"] <- "Sem_wod"
  
    #SEM by assessor
    frames = wod$atFrame %>%
      group_by(Assessor,Label,Axis,Context,Frames)%>%
      summarise(Sem = sqrt(sum(Sem_wod^2)/length(Sem_wod)))
    
    framesAvg = frames %>%
      group_by(Assessor,Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))
    
  }
  return(list(atFrame=frames, overall = framesAvg))

}


Sem_allAssessors<-function(sadf, Anova=TRUE){
  
  if (Anova){
    
    
    frames = sadf %>%
      group_by(Label,Axis,Context,Frames)%>%
      do(anova = aov(Avg ~ Participant, data=.))%>%
      mutate(Sem = sigma(anova,na.rm = TRUE))
    
    framesAvg = frames %>%
      group_by(Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))
  
    } else {
    
    wsd = withinSubjectStandardDeviation(sadf)
    colnames(wsd$atFrame)[colnames(wsd$atFrame)=="Sem"] <- "Sem_wsd"
    
    #all Ass
    frames = wsd$atFrame %>%
      group_by(Label,Axis,Context,Frames)%>%
      summarise(Sem = sqrt(sum(Sem_wsd^2)/length(Sem_wsd)))
    
    
    framesAvg = frames %>%
      group_by(Label,Axis,Context)%>%
      summarise(SemAvg = mean(Sem,na.rm = TRUE))
    }
  

  return(list(atFrame=frames, overall = framesAvg))
  
}



betweenAssessors<-function(sadf){
  
  assessor_frames = sadf %>%
    group_by(Assessor,Label,Axis,Context,Frames)%>%
    summarise(Average = mean(Avg,na.rm = TRUE))
  
  
  assessor_framesAvg = assessor_frames %>%
    group_by(Assessor,Label,Axis,Context)%>%
    summarise(FrameAverage = mean(Average,na.rm = TRUE))
  
  
  
  allAssessors_frames = assessor_frames %>%
    group_by(Frames,Label,Axis,Context)%>%
    summarise(All = mean(Average), Agreement = max(Average)-min(Average))
  
  
  allAssessors_framesAvg = allAssessors_frames %>%
    group_by(Label,Axis,Context)%>%
    summarise(AllAverage = mean(All), FrameAgreement = mean(Agreement))
  

  return(list(byAssessor_atFrame=assessor_frames, 
              byAssessor_overall = assessor_framesAvg,
              allAssessors_atFrame=allAssessors_frames, 
              allAssessors_overall = allAssessors_framesAvg))
}


betweenAssessorsReport<-function(badfs){

  byAssessor = badfs$byAssessor_overall
  allAssessors = badfs$allAssessors_overall


  byAssessorJoin = left_join(byAssessor,allAssessors, by=c("Label","Axis","Context"))
  
  
  byAssessorJoin = byAssessorJoin%>%
    rowwise()%>%
    mutate(Case = ifelse(FrameAverage>AllAverage, "greater", "lesser"),
           AbsoluteDifference = FrameAverage - AllAverage
           )
  
  return (byAssessorJoin)
  
  
}

withinAssessorReport<-function(sembyAssessordf,nAssesor,nParticipant){
  
  ic_upper = sqrt(((nAssesor-1)*nParticipant)/(qchisq(0.05, df=(nAssesor-1)*nParticipant))) 
  
  ##  within Assessor
  overallDf = sembyAssessordf$overall
  
  overallDf$IC_up = overallDf$SemAvg *ic_upper 
  

  return (overallDf)
  

}

accrossAssessorReport<-function(semAllAssessordf,nAssesor,nParticipant){
  
  ic_upper = sqrt(((nAssesor-1)*nParticipant)/(qchisq(0.05, df=(nAssesor-1)*nParticipant))) 
  
  ##  within Assessor
  overallDf = semAllAssessordf$overall
  
  overallDf$IC_up = overallDf$SemAvg *ic_upper 
  
  
  return (overallDf)
  
  
}



