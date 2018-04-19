library(lme4)   
library(tidyr)
library(grid)
library(dplyr)
library(readxl)
library(ggplot2)



setwd("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R")

source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\varianceEstimation.R")



## user-specified parameters-------------------------
formstr="~(1|SubjectName)+(1|AssessorName)+(1|SubjectName:AssessorName)+(1|SubjectName:AssessorName:SessionName)"

######################

myResults = "./tests/varianceEstimation/myResults.xls"

myDATA=read_excel(myResults)



# LAnkles
angles = filter(myDATA, Label=="LAnkleAngles" & "X" == Axis & Context=="Left")

sds =   VarianceEstCore(angles,formStr)
plotVariance(sds)
globalVar = getGlobalVariance(sds)

globalSdExpected = c(9.7, 0.6, 0.0, 3.5, 2.4)
if (all(globalVar$globalSD == globalSdExpected)){ print ("OK")}else{"Fail !!"}

# LHip
angles = filter(myDATA, Label=="LHipAngles" & "X" == Axis & Context=="Left")

sds =   VarianceEstCore(angles,formStr)
plotVariance(sds)
globalVar = getGlobalVariance(sds)

globalSdExpected = c(9.1, 2.4, 0, 7.5, 1.9)
if (all(globalVar$globalSD == globalSdExpected)){ print ("OK")}else{"Fail !!"}





