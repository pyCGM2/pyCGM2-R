
rms <- function(valueA,valueB){
  diff = (valueA - valueB)^2
  value = sqrt(sum(diff))/sqrt(length(diff))  
  return (  value)
}


processRms <- function(x,b,e,bb,ee) {
  diff = (as.numeric(x[b:e]) - as.numeric(x[bb:ee]))^2
  value = sqrt(sum(diff))/sqrt(length(diff))  
  return (  value)
}

processRmsNoZeros <- function(x,b,e,bb,ee) {

  valueA = x[b:e]
  
  valueB = x[bb:ee]
  
  if (length(which(valueB == 0)) == 101 ){
    value = 0
  }
  else if(length(which(is.na(valueB)))==101){
    value = NA
  }
  else{
    if (any(valueB ==0)) {
      valueA2 = valueA[-which(valueB ==0)[]]
      valueB2 = valueB[-which(valueB ==0)[]]
      diff = (as.numeric(valueA2) - as.numeric(valueB2))^2
    }
    else{
      diff = (as.numeric(valueA) - as.numeric(valueB))^2
    }
    value = sqrt(sum(diff))/sqrt(length(diff))
  }
  return (  value)
}
  
  
# APPLY RMS------------------------------------------------





applyRms_onConsecutiveCols <- function(wide_Table,columnName1,columnName1_begin, columnName1_end, 
                     columnName2,columnName2_begin, columnName2_end,
                     removeZeros=TRUE){
  # wide_TableHJC2 = applyRms(wide_TableHJC,"Davis","X","Z","US","X","Z")
  
  
  indexA_begin = which( colnames(wide_Table)==paste(columnName1,"_", columnName1_begin,sep="" ))
  indexA_end = which( colnames(wide_Table)==paste(columnName1,"_", columnName1_end,sep="" ))
  
  indexB_begin = which( colnames(wide_Table)==paste(columnName2,"_", columnName2_begin,sep="" ))
  indexB_end = which( colnames(wide_Table)==paste(columnName2,"_", columnName2_end,sep="" ))
  
  if (removeZeros){
    rms_wideTable = apply(wide_Table,1, processRmsNoZeros,indexA_begin,indexA_end,indexB_begin,indexB_end)
    wide_Table$Rms = rms_wideTable
  }
  else{
    rms_wideTable =apply(wide_Table,1, processRms,indexA_begin,indexA_end,indexB_begin,indexB_end)
    wide_Table$Rms = rms_wideTable
  }
  
  return(wide_Table)
  
}

# 
apply_max <- function(table,swingPhase){

  endStance = 100-round(swingPhase)  
  indexEndStance = which( grepl(as.character(endStance),colnames(table)))
  indexEnd = which( colnames(table) == "Frame100")
  

  return(wide_Table)
  
}







# APPLY RMS on Frames------------------------------------------------

applyRms_onFrames <- function(wide_Table,columnName1,columnName2,removeZeros=TRUE){
  
      
  indexA_begin = which( colnames(wide_Table)==paste(columnName1,"_Frame0",sep="" ))
  indexA_end = which( colnames(wide_Table)==paste(columnName1,"_Frame100",sep="" ))

  indexB_begin = which( colnames(wide_Table)==paste(columnName2,"_Frame0",sep="" ))
  indexB_end = which( colnames(wide_Table)==paste(columnName2,"_Frame100",sep="" ))
  
  if (removeZeros){
    rms_wideTable = apply(wide_Table,1, processRmsNoZeros,indexA_begin,indexA_end,indexB_begin,indexB_end)
    wide_Table$Rms = rms_wideTable
  }
  else{
    rms_wideTable =apply(wide_Table,1, processRms,indexA_begin,indexA_end,indexB_begin,indexB_end)
    wide_Table$Rms = rms_wideTable
  }
    
   return(wide_Table)
   
}

