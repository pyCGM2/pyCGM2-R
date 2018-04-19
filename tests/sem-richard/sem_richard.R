library(lme4)   
library(tidyr)
library(grid)
library(dplyr)
library(readxl)
library(ggplot2)


setwd("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R")

### simple ######

myResults = "./tests/sem-richard/richard.xlsx"
myDATA=read_excel(myResults)

# method 1
fit <- lm(Value ~ Person, data=myDATA)
sigma(fit)

# method 2
fit2 <- aov(Value ~ Person, data=myDATA)
summary(fit2)
sigma(fit2)


### Complex ######

myResults2 = "./tests/sem-richard/moreComplex.xlsx"
myDATA2=read_excel(myResults2)

myDATA_frame0 = filter(myDATA2, Frames == "Frames0")


#---- average session------ 

#   frames 0
sessionAverage = myDATA_frame0 %>%
  group_by(Participant,Assessor,Session)%>%
  summarise(avg = mean(Value))

#   allFrames
sessionAverage_frames = myDATA2 %>%
  group_by(Participant,Assessor,Session,Frames)%>%
  summarise(avg = mean(Value))
sessionAverage_frames



#---SEM for individual Assessor---


#   frames 0
semIndAssessor = sessionAverage %>%
  group_by(Participant,Assessor)%>%
  summarise(stddev = sd(avg))

#   allFrames
data = sessionAverage_frames %>%
  group_by(Participant,Assessor,Frames)


semIndAssessor_frames2 = sessionAverage_frames %>%
  group_by(Participant,Assessor,Frames)%>%
  do(anova = aov(avg ~ Session, data=.))%>%
  mutate(Sem = sigma(anova))
semIndAssessor_frames2



semIndAssessor_frames = sessionAverage_frames %>%
  group_by(Participant,Assessor,Frames)%>%
  summarise(sem = sd(avg))
semIndAssessor_frames

semIndAssessor_framesAvg = semIndAssessor_frames %>%
  group_by(Participant,Assessor)%>%
  summarise(FrameSem = mean(sem))
semIndAssessor_framesAvg

#-----sem--------


#   frames 0
fitSem = aov(avg ~ Participant, data=filter(sessionAverage, Assessor == "Op1"))
summary(fitSem)
sigma(fitSem)

#   allFrames
fitSem_frames = sessionAverage_frames %>%
  group_by(Assessor,Frames)%>%
  do(anova = aov(avg ~ Participant, data=.))%>%
  mutate(sem = sigma(anova))
fitSem_frames

fitSem_framesAvg = fitSem_frames %>%
  group_by(Assessor)%>%
  summarise(FrameSem = mean(sem))
fitSem_framesAvg


# ----sem all----

#   frames 0
fitSemAll = aov(avg ~ Participant, sessionAverage)
summary(fitSemAll)
sigma(fitSemAll)

#   frames
fitSemAll_frames = sessionAverage_frames %>%
  group_by(Frames)%>%
  do(anova = aov(avg ~ Participant, data=.))%>%
  mutate(sem = sigma(anova))


allSem = mean(fitSemAll_frames$sem)



  

#----Within subject standard deviations----

#   frame0
fitws = aov(avg ~ Assessor, data=filter(sessionAverage, Participant == "P01"))
summary(fitws)
sigma(fitws)

#   frames
fitws_frames = sessionAverage_frames %>%
  group_by(Participant,Frames)%>%
  do(anova = aov(avg ~ Assessor, data=.))%>%
  mutate(Sem = sigma(anova))
fitws_frames

fitws_framesAvg = fitws_frames %>%
  group_by(Participant)%>%
  summarise(SemFrameAvg = mean(Sem))

fitws_framesAvg = rename(fitws_framesAvg, Sem=SemFrameAvg)
fitws_framesAvg


# ------between subject------


#   frame0
bws = sessionAverage %>%
  group_by(Assessor)%>%
  summarise(grandAvg = mean(avg))
bws

bwsAll = bws %>%
  summarise(AllAvg = mean(grandAvg), agreement = max(grandAvg)-min(grandAvg))
bwsAll

#   frames
bws_frames = sessionAverage_frames %>%
  group_by(Assessor,Frames)%>%
  summarise(average = mean(avg))
bws_frames


bwsAll_frames = bws_frames %>%
  group_by(Frames)%>%
  summarise(AllAverage = mean(average), max = max(average), min = min(average) ,agreement = max(average)-min(average))
bwsAll_frames


bws_framesAvg = bws_frames %>%
  group_by(Assessor)%>%
  summarise(FrameAverage = mean(average))
bws_framesAvg


bwsAll_framesAvg = bwsAll_frames %>%
  summarise(FrameAllAverage = mean(AllAverage), FrameAgreement = mean(agreement))
bwsAll_framesAvg

bws_framesAvg$All = bwsAll_framesAvg$FrameAllAverage
bws_framesAvg$Agreement = bwsAll_framesAvg$FrameAgreement

bwsAll_framesAvg

# report
nAssesor= 3
nParticipant = 10
ic_upper = sqrt(((nAssesor-1)*nParticipant)/(qchisq(0.05, df=(nAssesor-1)*nParticipant))) 

##  within Assessor 
fitSem_framesAvg$IC = fitSem_framesAvg$FrameSem *ic_upper 







