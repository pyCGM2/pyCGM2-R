library(ggplot2)   # plotting library
library(dplyr) # il faut oublier plyr pour dplyr
library(tidyr)
library(cowplot)
library(readxl)
library(lme4)   

rm(list = ls(all = TRUE)) # clear workspace

source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\io.R")
source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\dataProcessing.R")
source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\rPlots.R")
source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\numeric.R")

source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\reliability.R")
source("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\varianceEstimation.R")

windowsFonts(Times=windowsFont("TT Times New Roman"))


#---- DATA-------

kinematicTable = read.csv(".\\dataset\\kinematicReliability.csv")
kinematicTable = mutate(kinematicTable, ComparisonFactor = paste0(Operator,"_",Session)) # combinaison of two factors labelling session conditions 

#---- Consistency plot-------

## all subjects and all modalities of comparisonFactor  

data= kinematicTable
figOp1 = consistencyPlot(data,
                         "Left" , "LHipAngles","Z", 
                         iTitle="",yLabel="Deg", legendPosition="top",ylimits=c(-30,30),
                         colorFactor = NULL,linetypeFactor = NULL, facetFactor = NULL)
figOp1

# in case you want to work with a specific operator, filter your data with dplyr::filter   
data= filter(kinematicTable,Operator=="OP01")
figOp1 = consistencyPlot(data,
                         "Left" , "LHipAngles","Z", 
                         iTitle="",yLabel="Deg", legendPosition="top",ylimits=c(-30,30),
                         colorFactor = NULL,linetypeFactor = NULL, facetFactor = NULL)
figOp1


#---- descriptive plot-------
# IMPORTANT : you need to compute descriptive data first
descriptiveKinematics = descriptiveGait(kinematicTable,bySubjectFlag = FALSE) # return a list ( frameVcalues, events) of  all subject descriptive stats 
frameValues = descriptiveKinematics$Frames
eventsValues = descriptiveKinematics$Events


descriptiveKinematics = descriptiveGait(kinematicTable,bySubjectFlag = FALSE) # return a list ( frameVcalues, events) of  all subject descriptive stats 

## all subjects and all modalities of comparisonFactor 
Pelvis_X = descriptivePlot(descriptiveKinematics$Frames, "Left" , "LPelvisAngles","X", 
                           iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(0,60),
                           colorFactor = NULL,linetypeFactor = NULL, facetFactor = NULL)

Pelvis_X


#---- descriptive gait plot-------
Pelvis_X = descriptivePlot(frameValues, "Left" , "LPelvisAngles","X", 
                           iTitle="Pelvic tilt",yLabel="Deg", legendPosition="top",ylimits=c(0,60),
                           colorFactor = NULL,linetypeFactor = NULL, facetFactor = NULL)
Pelvis_X=addGaitDescriptiveEventsLines(Pelvis_X,eventsValues,"Overall",
                                       colorFactor=NULL, linetypeFactor=NULL  )

Pelvis_X


#---- descriptive gait Panel-------
fig=descriptiveKinematicGaitPanel(frameValues,eventsValues,
                                  "Left", colorFactor=NULL, linetypeFactor=NULL)

fig
