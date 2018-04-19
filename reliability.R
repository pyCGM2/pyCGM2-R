ComputeSessionAverage<-function(df){
  outDf = df %>%
  group_by(Participant,Assessor,Session,Label,Axis,Context,Frames)%>%
  summarise(Avg = mean(Value))
  return(outDf)
}

withinSubjectDeviation <-function(sadf){
  
  fitws_frames = sadf %>%
    group_by(Participant,Label,Axis,Context,Frames)%>%
    do(anova = aov(Avg ~ Assessor, data=.))%>%
    mutate(Sem = sigma(anova))
  
  
  fitws_framesAvg = fitws_frames %>%
    group_by(Participant,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem))
  
  return(list(atFrame=fitws_frames, overall = fitws_framesAvg))
}



withinOperatorDeviation<-function(sadf){
  
  #---SEM for individual Assessor---
  
  
  frames = sadf %>%
    group_by(Participant,Assessor,Label,Axis,Context,Frames)%>%
    summarise(Sem = sd(Avg))
  
  framesAvg = frames %>%
    group_by(Participant,Assessor,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem))
  
  
  return(list(atFrame=frames, overall = framesAvg))
  
}


Sem_byAssessor<-function(sadf){
  
  frames = sadf %>%
    group_by(Assessor,Label,Axis,Context,Frames)%>%
    do(anova = aov(Avg ~ Participant, data=.))%>%
    mutate(Sem = sigma(anova))
  
  framesAvg = frames %>%
    group_by(Assessor,Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem))
  
  return(list(atFrame=frames, overall = framesAvg))
  
}


Sem_allAssessors<-function(sadf){
  
  frames = sadf %>%
    group_by(Label,Axis,Context,Frames)%>%
    do(anova = aov(Avg ~ Participant, data=.))%>%
    mutate(Sem = sigma(anova))
  
  allSem = frames %>%
    group_by(Label,Axis,Context)%>%
    summarise(SemAvg = mean(Sem))
  

  return(list(atFrame=frames, overall = allSem))
  
}


betweenAssessors<-function(sadf){
  
  assessor_frames = sadf %>%
    group_by(Assessor,Label,Axis,Context,Frames)%>%
    summarise(Average = mean(Avg))
  
  
  assessor_framesAvg = assessor_frames %>%
    group_by(Assessor,Label,Axis,Context)%>%
    summarise(FrameAverage = mean(Average))
  
  
  
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

