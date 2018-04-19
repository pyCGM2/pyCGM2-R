
library(lme4)   
library(latticeExtra) 
library(tidyr)
library(grid)
library(readxl)

setwd("C:\\Users\\HLS501\\Documents\\Programming\\API\\pyCGM2\\pyCGM2-R\\pyCGM2-R\\KolethVariance")

## user-specified parameters-------------------------
specfile="ExperimentDesign.xlsx"
formstr="~(1|SubjectName)+(1|AssessorName)+(1|SubjectName:AssessorName)+(1|SubjectName:AssessorName:SessionName)"
######################

estVcomp=function(savePlot=FALSE,saveSD=FALSE){
  require(lme4)
  require(latticeExtra)
  require(grid)
  require(readxl)
  require(tidyr)
  
  param=read_excel(specfile,2)
  design=read_excel(specfile,3)

  datafile=param[[3]][1]
  sheetName=excel_sheets(datafile)
  expectedSheetName=paste(param$Trajectory,param$Dim,sep="_")
  if(!all(sheetName%in%expectedSheetName))
    stop(paste0("Sheet names in ",basename(datafile)," does not match those listed in the Parameters sheet."))
  

  ## rename variance components.
  vcompNames=c(rev(gsub("1 \\| ","",attr(terms(as.formula(paste0("1",formstr))),"term.labels"))),"Residual")
  print("The current variance components names are:")
  print(vcompNames)
  toChange=readline("Do you want to change it? (y/n)")
  if(toChange=="y"){
    newVcompNames=rep(NA,length(vcompNames))
    for(i in 1:length(vcompNames))
      newVcompNames[i]=readline(paste0("Please enter new name for component ",vcompNames[i],":"))
    vcompNames=newVcompNames
  }
  
  ## analysis.
  sdData=plots=list()
  for(p in sheetName){
    print(paste0("Analyzing ",p,"."))
    data=read_excel(datafile,sheet=p)
    if(nrow(data)!=nrow(design))
      stop(paste0("Number of rows in `",p,"` sheet does not match that as specified in the `Experiment design` sheet."))
    angleNames=names(data)
    Ynames=paste0("X",angleNames)
    names(data)=Ynames
    D=data.frame(design,data)
    for(s in unique(D$SideName)){
      print(paste("Analyzing",s,"side."))
      X=subset(D,SideName==s)
      V=lapply(1:ncol(data),function(j){
        form=as.formula(paste0(Ynames[j],formstr))
        lmm=lmer(form,data=X,control=lmerControl())
        out=as.data.frame(VarCorr(lmm))[,c("grp","sdcor")]
        names(out)[2]=angleNames[j]
        out$grp=factor(vcompNames,levels=vcompNames[c((length(vcompNames)-1):1,length(vcompNames))])
        out
      })
      pV=Reduce(merge,V)
      browser()
      pV=tidyr::gather(pV,t,sd,-grp,convert=T)
      pV$sd=round(pV$sd,1)
      plots[[s]][[p]]=xyplot(sd~t|grp,data=pV,type="l",main=paste(p,s,sep="-"),layout=c(5,1),
                        scales=list(y=list(rot=0)),axis=axis.grid,ylab="Standard deviation (deg)")
      print(plots[[s]][[p]])
      pV$var=p
      pV$side=s
      sdData[[s]][[p]]=tidyr::spread(pV,t,sd)
    }

  }
  
  ## generate final plot.
  pD=gather(do.call(rbind,lapply(sdData,function(ll)do.call(rbind,ll))),t,sd,-grp,-var,-side,convert=T)
 
  
  finalP=useOuterStrips(xyplot(sd~t|grp+var,data=pD,type="l",as.table=TRUE,lwd=2,group=side,
                                auto.key=list(columns=2,points=FALSE,lines=TRUE),
                                panel=function(x,y,...){
                                  panel.xyplot(x,y,...)
                                  grid.text(x=grid::unit(0.3,"npc"),y=grid::unit(0.9,"npc"),label=paste0("sd=",round(sqrt(mean(y^2)),1)))
                                },
                                lattice.options=list(layout.widths=list(strip.left=list(x=10))),
                                scales=list(y=list(rot=0)),axis=axis.grid,ylab="Standard deviation (deg)"),
                         strip.left=strip.custom(horizontal=T))
  print(finalP)
  
  ## save the plot to pdf-------------
  if(savePlot){
    pdf(file="vcomp_plot.pdf")
    print(finalP)
    dev.off()
    shell.exec("vcomp_plot.pdf")
  }

    ## save the data----------------------
  if(saveSD)write.csv(pD,file="sdData.csv",row.names=FALSE)
}

estVcomp(savePlot = FALSE,saveSD = FALSE)
