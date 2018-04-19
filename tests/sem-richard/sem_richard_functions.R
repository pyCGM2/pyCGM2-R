library(lme4)   
library(tidyr)
library(grid)
library(dplyr)
library(readxl)
library(ggplot2)


source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\reliability.R")

setwd("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R")

### Complex ######

myResults = "./tests/sem-richard/moreComplex.xlsx"
data=read_excel(myResults)



#---- average session------ 


sadf = ComputeSessionAverage(data)


wsd = withinSubjectDeviation(sadf)

wod = withinOperatorDeviation(sadf)

semAss = Sem_byAssessor(sadf)

semAll = Sem_allAssessors(sadf)

ba = betweenAssessors(sadf)

comparisonBA = betweenAssessorsReport(ba)

tes = withinAssessorReport(semAss,3,10)

