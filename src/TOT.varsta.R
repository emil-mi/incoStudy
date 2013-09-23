rm(list=ls(all.names=T))
source('src/common.R')
source('src/all-data.R')

suppressWarnings(rm(IU,AMB,TOT))

COMMON$Tip<-factor(COMMON$Tip,labels=c("TOT","AMB"))
COMMON$Psi<-ifelse(COMMON$Psi=="DEPRESIE",T,F)
COMMON<-rename(COMMON,replace=c('Psi'='Depresie','Bronsita_cronica'='Bronsita'))

ddply(COMMON,.(Bronsita,Diabet,Depresie),str)
COMMON<-adply(
  COMMON,1,
  function(row) {
    c(com=c("Bronsita","Diabet","Depresie","NA")[
      with(row,c(Bronsita,Diabet,Depresie,!any(Bronsita,Diabet,Depresie)))
           ])
  }
)
COMMON$com<-as.factor(COMMON$com)
COMMON<-subset(COMMON,select=-c(Bronsita,Diabet,Depresie))

set.seed(1234)
data<-rbind(
  data.frame(Varsta=rkde(500,kde(COMMON$Varsta[COMMON$Tip=='AMB'],positive=T)),Tip='AMB'),
  data.frame(Varsta=rkde(500,kde(COMMON$Varsta[COMMON$Tip=='TOT'],positive=T)),Tip='TOT'),
  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanF),Tip='RO'))
data<-with(data,data[Varsta>=27 & Varsta<=77,])

wilcox.test(data$Varsta[data$Tip=='RO'],data$Varsta[data$Tip=='AMB'],mu=-1.942)
wilcox.test(data$Varsta[data$Tip=='RO'],data$Varsta[data$Tip=='TOT'],mu=-1.942)
