library(readxl)

loadNormativeDataSet <- function(fullXlsFile, sheet){
  
  data = read_excel(fullXlsFile, sheet = sheet ,col_names = TRUE)

  return (  data )
}



constructTableFromXls <- function(fullXlsFiles, sheet){
  
  # construit une dataframe a partir d une feuille de plusieurs ficheirs excel
  table = data.frame()
  for (xlsfile in fullXlsFiles){
    data = read_excel(xlsfile, sheet = sheet ,col_names = TRUE)
    table = rbind(table,data)
  }
  
  table$Index = seq(1,nrow(table))
  return (  table )
}



constructGPSTableFromXls <- function(fullXlsFiles){
  
  gpsTable = data.frame()
  
  
  for (xlsfile in fullXlsFiles){
    
    
    data = read_excel(xlsfile, sheet = "descriptive GPS ",col_names = TRUE)
    gpsTable = rbind(gpsTable,data)
    
  }
  
  gpsTable$Index = seq(1,nrow(gpsTable))
  return (gpsTable)
}


