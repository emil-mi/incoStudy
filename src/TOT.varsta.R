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

local({
  densityplot(~Varsta|Tip,COMMON,bw='SJ')
  qqmath(~Varsta|Tip,COMMON,type=c('b','r','g'))
  
  distribution<-function(probs) {
    quantile(rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanF)-2,probs)
  }
  
  xyplot(y~x|tip,scales=list(y='free'), data.frame(x=seq(0,1,0.01),
                        y=c(
                          qnorm(seq(0,1,0.01)),
                          distribution(seq(0,1,0.01))),
                        tip=c(rep('n',101),rep('e',101))),
                        type=c('l','g'))
  
  set.seed(1234)
  varste<-rbind(
    COMMON[,c("Varsta","Tip")],
    data.frame(Varsta=sample(rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanF)-2,90000,T),Tip='RO')
  )

  range.min<-with(varste,min(Varsta[Tip %in% c('AMB','TOT')]))
  range.max<-with(varste,max(Varsta[Tip %in% c('AMB','TOT')]))
  varste<-varste[varste$Varsta>=0.8*range.min & varste$Varsta<=range.max,]
  
  dp1<-asTheEconomist(
    densityplot(
      ~Varsta,
      varste,
      groups=Tip,
      auto.key=T,
      plot.points=F,main='',ylab=''),
    xlab='Vârsta'
  )
  
  dp2<-asTheEconomist( bwplot(
    Varsta~Tip,
    data=COMMON,
    ylab='Vârsta',
    xlab='Grup')
  )
  
  #sterg etichetele de la scala y. Nu am gasit un alt mod pana acum
  dp1$yscale.components.old<-dp1$yscale.components
  dp1$yscale.components<-function(...) { 
    ret<-dp1$yscale.components.old(...)
    ret$left$labels$labels=NULL
    ret
  }
  
  png(filename="doc/img/totVarstaGrup.png",width=540,height=540,bg=graph.bg)
  print(dp1,split = c(1, 1, 1, 2))
  print(dp2,split = c(1, 2, 1, 2),newpage=FALSE)
  dev.off()
  
  dp3<-styleGraph(qqmath(
    ~Varsta,COMMON,type=c('b','r'),distribution=distribution,group=Tip,
    main="",auto.key=T))
  png(filename="doc/img/totVarstaQQ.png",width=540,height=540,bg=graph.bg)
  print(dp3)
  dev.off()
  
})

set.seed(1234)
data<-rbind(
  data.frame(Varsta=rkde(500,kde(COMMON$Varsta[COMMON$Tip=='AMB'],positive=T)),Tip='AMB'),
  data.frame(Varsta=rkde(500,kde(COMMON$Varsta[COMMON$Tip=='TOT'],positive=T)),Tip='TOT'),
  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanF),Tip='RO'))

data.amb<-with(data,data[Varsta>=min(Varsta[Tip=='AMB']) & Varsta<=max(Varsta[Tip=='AMB']),])
data.tot<-with(data,data[Varsta>=min(Varsta[Tip=='TOT']) & Varsta<=max(Varsta[Tip=='TOT']),])

t.test(data.amb$Varsta[data$Tip=='RO'],data.amb$Varsta[data$Tip=='AMB'],var.equal=T)
t.test(data.amb$Varsta[data$Tip=='RO'],data.amb$Varsta[data$Tip=='AMB'],var.equal=F)

wilcox.test(data.amb$Varsta[data$Tip=='RO'],data.amb$Varsta[data$Tip=='AMB'],mu=-2)
wilcox.test(data.tot$Varsta[data$Tip=='RO'],data.tot$Varsta[data$Tip=='TOT'],mu=-2)
