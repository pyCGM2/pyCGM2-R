library(Metrics)   
library(irr)   

### --- New ----

# metrics 

# low level



metrics_onFrames<- function(table,metricsFunction,factor1,factor2,comparisonLabel=NULL){
  
  apply_metrics<-function(df,metricsFunction ,factor1,factor2){
    
    b1 = which( colnames(wide_Table)==paste(factor1,"_Frame0",sep="" ))
    e1 = which( colnames(wide_Table)==paste(factor1,"_Frame100",sep="" ))
    
    b2 = which( colnames(wide_Table)==paste(factor2,"_Frame0",sep="" ))
    e2 = which( colnames(wide_Table)==paste(factor2,"_Frame100",sep="" ))
    
    valueA = as.numeric(df[b1:e1])
    valueB = as.numeric(df[b2:e2])  
    
    res =  eval(parse(text= paste0("Metrics::",metricsFunction,"(valueA,valueB)"))) 
    #res = Metrics::rmse(valueA,valueB)
    return (res)
  }
  
  
  if ("Stats" %in% names(table)){
    table =  filter(table, Stats == "mean")
  }
  
  
  wide_Table = table %>%
    select(ComparisonFactor,Id,Label,Axis,Cycle,Context,Frame0:Frame100)%>%
    gather(Frames, Value, Frame0:Frame100)%>%
    unite(temp, ComparisonFactor, Frames) %>%
    spread(temp, Value)
  wide_Table$Index=seq(1,nrow(wide_Table))
  
  #ta =apply(wide_Table,1, processRms,indexA_begin,indexA_end,indexB_begin,indexB_end)
  
  out = wide_Table%>%
    rowwise()%>%
    do(data.frame(metricsFunction = apply_metrics(., metricsFunction,factor1,factor2)))%>%
    bind_cols(wide_Table%>%
                select(Id,Label,Axis,Cycle,Context))
  
  if (is.null(comparisonLabel)){comparisonLabel = paste0(factor1,"_",factor2)} 
  out["ComparisonLabel"] = comparisonLabel
  
  names(out)[names(out) == "metricsFunction"] <- metricsFunction  
  
  return(out)
}




metrics_onFrames_local <- function(metricsFunction,table,Label,Context,Axes,factor1,factor2,comparisonLabel=NULL){
  
  # metrics_onFramesOutputs("rmse",kinematicTable,"LKneeAngles","Left",c("X","Y","Z"),"Rigid","THIsta")

  out = data.frame()
  
  if (is.null(comparisonLabel)){comparisonLabel = paste0(factor1,"_",factor2)}
  
  for (Axis in Axes){
    
    table1 = filter(table,ComparisonFactor == factor1 & Label == Label & Axis == Axis[1] & Context == Context)
    table2 = filter(table,ComparisonFactor == factor2 & Label == Label & Axis == Axis[1] & Context == Context)
    
    d1 = select(table1,starts_with("Frame0"): ends_with("Frame100"))
    d2 = select(table2,starts_with("Frame0"): ends_with("Frame100"))
    
    if (nrow(d1) != nrow(d2))
      stop("not the same number of cycle")
    
    value=c()
    for (i in 1:nrow(d1))
      value[i] =  eval(parse(text= paste0("Metrics::",metricsFunction,"(d1[i,],d2[i,])"))) 
    
    out = bind_rows(out,data.frame("Label" = Label, "Axis" = Axis, "Context" = Context,
                                   "comparison" = comparisonLabel,
                                   metricsFunction =  value))
  }
  
  names(out)[names(out) == "metricsFunction"] <- metricsFunction
  return (out)
  
}


MeanAbsoluteVariability <- function(descritiveTable){
 
  # compute in respect to Mantovani2016 formulae
  
  gds = gather_FrameValues_DescritiveStats(descritiveTable)
  gds = filter(gds,Stats == "mean")
  
  mavTable = gds %>%
      group_by(Id,Label,Context,Axis,Frame)%>%
      summarize(dif = max(Values)-min(Values))%>%
      group_by(Id,Label,Context,Axis)%>%
      summarize(MAV = mean(dif))

  return (mavTable)
}


MeanAbsoluteVariability_intra <- function(table){
  # compute mav for each comparison factor
  
  wide_Table = table %>%
    select(ComparisonFactor,Id,Label,Axis,Cycle,Context,Frame0:Frame100)%>%
    gather(Frames, Value, Frame0:Frame100)
  

  out = wide_Table%>%
    group_by(ComparisonFactor,Id,Label,Context,Axis,Frames)%>%
    summarise(diff = max(Value)-min(Value))%>%
    group_by(ComparisonFactor,Label,Context,Axis)%>%
    summarize(MAV = mean(diff))

  return (out) 
}




MinDetectableChange <- function(table, factors){

  #mdc = MinDetectableChange(kinematicTable,c("Frame0","Frame001") )
  # compute in respect to Mantovani2016 formulae

  gkt = table  %>%
    select("Id","Label","Context","Axis","Cycle","ComparisonFactor",factors)%>%
    gather_("Factor", "Values",  factors)%>%
    mutate(Factor = paste0("Sem_",Factor))%>%
    group_by(Label,Axis,Context,Factor)%>%
    do(anova = aov(Values ~ ComparisonFactor, data=.))%>%
    mutate(Sem = 1.96* sqrt(2)*sigma(anova))%>%
    select(-anova)%>%
    spread(Factor,Sem)

  return(gkt)
  
}



metrics_onCycleDiscreteValue<- function(table,metricsFunction,DiscreteLabel,factor1,factor2,comparisonLabel=NULL){
  
  apply_metrics<-function(df,metricsFunction ,factor1,factor2){
     
     valueA = as.numeric(df[factor1])
     valueB = as.numeric(df[factor2])  
     
     res =  eval(parse(text= paste0("Metrics::",metricsFunction,"(valueA,valueB)"))) 
     #res = Metrics::rmse(valueA,valueB)
     return (res)
   }
   
   
   if ("Stats" %in% names(table)){
     table =  filter(table, Stats == "mean")
   }
   
   
   wide_Table = table %>%
     select("ComparisonFactor","Id","Label","Axis","Cycle","Context",DiscreteLabel)%>%
     gather_("Method", "Value", DiscreteLabel)%>%
     unite_("temp", "ComparisonFactor", DiscreteLabel) %>%
     spread(temp, Value)
   wide_Table$Index=seq(1,nrow(wide_Table))
   
   
  out = wide_Table%>%
     group_by(Id,Label,Context,Axis,Cycle)%>%
     do(data.frame(metricsFunction = apply_metrics(., metricsFunction,factor1,factor2)))
  
  if (is.null(comparisonLabel)){comparisonLabel = paste0(factor1,"_",factor2)} 
    out["ComparisonLabel"] = comparisonLabel
   
   names(out)[names(out) == "metricsFunction"] <- metricsFunction  
  
  return(out)
}