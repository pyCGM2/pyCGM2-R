
#' add new col CycleCum, cumulating cycle of both context 
addCumulatedCycleindex<-function(table, groupByList){
  
  nbCycles = table%>%
    filter(Context =="Left")%>%
    group_by_at(vars(groupByList)) %>%  
    summarise(ncycles = max(Cycle)+1) 
  
  
  tmp = left_join(table,nbCycles)
  
  outTable = tmp %>% 
    rowwise() %>% 
    mutate(CycleCum  = ifelse(Context == "Left",Cycle,Cycle + ncycles))
  
  return (outTable)
}



constructOverallTable<- function(table){
  
  if ("CycleCum" %in% names(table)) {

    
    table$Label = substring(table$Label,2,  nchar(table$Label))
    table$Context = "Overall"
    table$Cycle = table$CycleCum 
  
     return (table)
  }else{ stop("need to run function addCumulatedCycleindex before ") }
}


homegeizeCycles<-function(table,groupByList){
  
  cycleCumtable = addCumulatedCycleindex(table,groupByList)
  
  
  tmp =  cycleCumtable %>%
    group_by_at(vars(groupByList)) %>%  
    summarise(maxCycles = max(CycleCum))
  
  minMax = min (tmp$maxCycles)
  
  outTable = filter (cycleCumtable, CycleCum <= minMax)  
  
  return (outTable)    
  
}

#' Number of cycles on both Context
getNumberOfCycles<-function(table,label,axis){
  
  outTable =  table%>%
    group_by_("Id","Operator","Session","Context") %>%
    filter( Label==label & Axis == axis  )%>%
    summarize(ncycles = length(Label))  
  
  return(outTable)
}


#### Descriptive stats NEW ####


descritiveStats<- function(table,factors,ComparativeLabel,bySubjectFlag=TRUE){
  
    if (bySubjectFlag) 
    {
      # mean
      mean=table%>%
        group_by_(ComparativeLabel,"Id","Context") %>%  
        select(factors)%>% 
        summarise_all(funs(mean)) 
      
      sd=table%>%
        group_by_(ComparativeLabel,"Id","Context") %>%  
        select(factors)%>% 
        summarise_all(funs(sd)) 
    } else {
      # mean
      mean=table%>%
        group_by_(ComparativeLabel,"Context") %>%  
        select(factors)%>% 
        summarise_all(funs(mean)) 
      
      sd=table%>%
        group_by_(ComparativeLabel,"Context") %>%  
        select(factors)%>% 
        summarise_all(funs(sd)) 
      
    }
  
  mean$Stats ="mean"
  sd$Stats ="std"
  
  out = bind_rows(mean, sd)
  if (!(bySubjectFlag)) {
    out$Id = "All"
  }  
  
  out$Index = seq(1,nrow(out))
  return( out) 
  
  
}

gather_DescritiveStats<- function(DS,ComparativeLabel,factors){
  
  # gather_DescritiveStats(kinematic_phasesTbl,"ComparisonFactor",c("stancePhase","doubleStance1","doubleStance2"))  

  
  
  meanGather = DS  %>%
    filter(Stats == "mean")%>%
    gather_("Factor", "Mean",  factors)

  
  sdGather = DS  %>%
    filter(Stats == "std")%>%
    gather_("Factor", "Sd",  factors)

  
  meanGather$Stats=NULL
  sdGather$Stats=NULL
  final = left_join(meanGather,sdGather, by=c("Id",ComparativeLabel,"Context","Factor") )
    
  return(final)
}





#### FRAMES processing  ####  
FrameValues_descritiveStats<- function(table,ComparativeLabel,bySubjectFlag=TRUE){
  
  if (bySubjectFlag) {
    groupByList = c(ComparativeLabel,"Id","Context","Label","Axis")
  } else{
    groupByList = c(ComparativeLabel,"Context","Label","Axis")
  } 

  
  meanTrace=table%>%
    group_by_at(vars(groupByList)) %>%  
    select(Frame0:Frame100)%>% 
    summarise_all(funs(mean)) 
  
  sdTrace=table%>%
    group_by_at(vars(groupByList)) %>% 
    select(Frame0:Frame100)%>% 
    summarise_all(funs(sd))

  
  meanTrace$Stats ="mean"
  sdTrace$Stats ="std"
  
  out = bind_rows(meanTrace, sdTrace)
  if (!(bySubjectFlag)) {
    out$Id = "All"
  }  
  
  out$Index = seq(1,nrow(out))

  return( out) 
}

  


gather_FrameValues_DescritiveStats<- function(framesDS){
  
  
  meanTraceGather = framesDS  %>%
    filter(Stats == "mean")%>%
    gather(Frame, Values,  Frame0:Frame100)
  
  sdTraceGather = framesDS  %>%
    filter(Stats == "std")%>%
    gather(Frame, Values,  Frame0:Frame100)

  final = bind_rows(meanTraceGather,sdTraceGather)
  
  
  return(final)
  
}


#----GAIT-----

descriptiveGait<- function(kinematicTable,bySubjectFlag=TRUE){ 

  descFrames = FrameValues_descritiveStats(kinematicTable,"ComparisonFactor",bySubjectFlag = bySubjectFlag) # frames
  descEvents = descritiveStats(kinematicTable,c("stancePhase","doubleStance1","doubleStance2"),"ComparisonFactor",bySubjectFlag =bySubjectFlag) #gait events

  #out= left_join(descriptiveKinematics, descriptivegaitEvents, by=c("Id","Context","ComparisonFactor","Stats"))
  
  return (list(Frames = descFrames, Events = descEvents ))
}
  




