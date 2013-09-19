rm(list=ls())
source('src/common.R')
source('src/all-data.R')

rm(AMB,COMMON,TOT,VarstaRO2010)
IU_WIDE<-rename(IU[,c(1:2,5,7,11:24)],replace=c("Tip"="sex"))
IU_WIDE<-cbind(IU_WIDE[,1:4],
      group=c(rep("IUE",25),rep("IUI",25)),
      bi.0=as.integer(IU$Nasteri>=0),
      bi.1=as.integer(IU$Nasteri>=1),
      bi.2=as.integer(IU$Nasteri>=2),
      bi.3=as.integer(IU$Nasteri>=3),
      IU_WIDE[,5:ncol(IU_WIDE)])
rm(IU)
IU_WIDE<-melt(IU_WIDE,id.vars=1:9)
IU_WIDE<-with(IU_WIDE,cbind(IU_WIDE[,1:9],
      colsplit(IU_WIDE$variable,"\\.",c("Variable","Trt")),
      value
     ))
IU_WIDE$Trt<-factor(IU_WIDE$Trt,level=c("PRE","POST"))
IU_WIDE$Variable<-as.factor(IU_WIDE$Variable)
IU.I2D<-subset(IU_WIDE,Variable=='I2D')
rownames(IU.I2D)<-1:nrow(IU.I2D)

likertScale.4<-c("Deloc","Ușor","Moderat","Mult")
likertScale.7<-c("Încântat","Mulțumit","Oarecum satisfăcut",
                 "Indiferent","Nesatisfăcut","Nefericit","Groaznic")
likertScale.7b<-c("Mult mai bine",
                  "Mai bine",
                  "Ceva mai bine",
                  "Nici o schimbare",
                  "Ceva mai rău",
                  "Mai rău",
                  "Mult mai rău") 

IU.CEII <- subset(IU_WIDE,subset=Variable=='CEII')
IU.CEII <- within(IU.CEII,value <- round(value/7))
IU.CEII.LIKERT <-as.data.frame(xtabs(~factor(value,0:3,likertScale.4)+Trt,IU.CEII))
colnames(IU.CEII.LIKERT)[1]<-"Level"
IU.CEII.LIKERT<-dcast(IU.CEII.LIKERT,Trt~Level,value.var='Freq')

IU.CVDSU <- subset(IU_WIDE,subset=Variable=='CVDSU')
IU.CVDSU.LIKERT <-as.data.frame(xtabs(~factor(value,1:7,likertScale.7)+Trt,IU.CVDSU))
colnames(IU.CVDSU.LIKERT)[1]<-"Level"
IU.CVDSU.LIKERT<-dcast(IU.CVDSU.LIKERT,Trt~Level,value.var='Freq')

levelsMap<-c(1,2,2,3,3,4,5,5,6,6,7)
IU.VAS <- subset(IU_WIDE,subset=Variable=='VAS')
IU.VAS.LIKERT <-as.data.frame(xtabs( ~ factor( levelsMap[value+1], 1:7, likertScale.7)+Trt,IU.VAS))
colnames(IU.VAS.LIKERT)[1]<-"Level"
IU.VAS.LIKERT<-dcast(IU.VAS.LIKERT,Trt~Level,value.var='Freq')
rm(levelsMap)

IU.FEFMP <- subset(IU_WIDE,subset=Variable=='FEFMP')
IU.FEFMP.LIKERT <-as.data.frame(xtabs( ~ factor( value,1:5,c('Nimic','Sesizabil','Mediu','Satisfăcător','Puternic') )+Trt,IU.FEFMP))
colnames(IU.FEFMP.LIKERT)[1]<-"Level"
IU.FEFMP.LIKERT<-dcast(IU.FEFMP.LIKERT,Trt~Level,value.var='Freq')

IU.IGPI <- subset(IU_WIDE,subset=Variable=='IGPI' & !is.na(value))

IU.USS <- subset(IU_WIDE,subset=Variable=='USS')

local({
  g.res.val.by.Nasteri<-styleGraph(
    xyplot(value~Nasteri|Variable+Trt,IU_WIDE,
           groups=sex,
           type=c('p','r'),auto.key=T,as.table=T)
  )  
  png(filename="doc/img/incoResByNasteri.png",width=540,height=540,bg=graph.bg)
  print(g.res.val.by.Nasteri)
  dev.off()
  
  g.res.bw_val.all<-styleGraph(
    bwplot(value~Trt|Variable+sex,IU_WIDE,subset=!(Variable %in% c('FEFMP','IGPI')),
           auto.key=T,ylab=''),
    scales=NULL
  )
  png(filename="doc/img/incoResBWValAll.png",width=540,height=540,bg=graph.bg)
  print(g.res.bw_val.all)
  dev.off()
  
  #brewer palletes: BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
  
  g.res.ceii_trt.stack<-styleGraph(
    likert(Trt~.,
           IU.CEII.LIKERT,as.percent = TRUE,
           auto.key=list(columns=1,title="CEII",adj=1,space='right'),ylab.right='',
           col=rev(likertColorBrewer(4,BrewerPaletteName='RdYlGn')))
    ,scales=list( y = list(axs='r',side='rigth')),axis=styleAxis.keepLeft,xlab="Procent",title=''
  )
  
  g.res.cvdsu_trt.stack<-styleGraph(
    likert(Trt~.,
           IU.CVDSU.LIKERT,as.percent = TRUE,
           auto.key=list(columns=1,title="CVDSU",adj=1,space='left'),ylab.right='',
           col=rev(likertColorBrewer(7,BrewerPaletteName='RdYlGn')),
           )
    ,scales=list( y = list(axs='r',side='rigth')),axis=styleAxis.keepLeft,xlab="Procent",title=''
  )
  
  g.res.vas_trt.stack<-styleGraph(
    likert(Trt~.,
           IU.VAS.LIKERT,as.percent = TRUE,
           auto.key=list(columns=1,title="VAS",adj=1,space="right"),ylab.right='',
           col=rev(likertColorBrewer(7,BrewerPaletteName='RdYlGn')))
    ,scales=list( y = list(axs='r',side='rigth')),axis=styleAxis.keepLeft,
    xlab="Procent",title=''
  )
  
  png(filename="doc/img/incoResVariousStack.png",width=540,height=540,bg=graph.bg)
  print(g.res.ceii_trt.stack,split=c(1,1,1,3))
  print(g.res.cvdsu_trt.stack,split=c(1,2,1,3),newpage=FALSE)
  print(g.res.vas_trt.stack,split=c(1,3,1,3),newpage=FALSE)
  dev.off()

  g.res.igpi.hist<-styleGraph(
    histogram(~factor(value,1:7,likertScale.7b),IU.IGPI,
              col=rev(likertColorBrewer(7,BrewerPaletteName='RdYlGn')),
              main="IGPI")
    ,ylab="Procent"
  )
  png(filename="doc/img/incoResIGPI.png",width=540,height=540,bg=graph.bg)
  print(g.res.igpi.hist)
  dev.off()
  
  g.res.fefmp_trt.hist<-styleGraph(
    likert(Trt~.,
           IU.FEFMP.LIKERT,as.percent = TRUE,
           auto.key=list(columns=2),main="FEFMP",ylab.right='',
           col=likertColorBrewer(5,BrewerPaletteName='RdYlGn'))
    ,scales=list( y = list(axs='r',side='rigth')),axis=styleAxis.keepLeft,xlab="Procent"
  )
  png(filename="doc/img/incoResFEFMP.png",width=540,height=540,bg=graph.bg)
  print(g.res.fefmp_trt.hist)
  dev.off()
  
  g.res.uss_trt.hist<-styleGraph(
    histogram(~value|Trt,IU.USS,nint=7,type="percent",
              col=rev(likertColorBrewer(8,BrewerPaletteName='RdYlGn')),
              main="USS")
    ,ylab="Procent",xlab="Numărul de vizite"
  )
  png(filename="doc/img/incoResUSS.png",width=540,height=540,bg=graph.bg)
  print(g.res.uss_trt.hist)
  dev.off()
  
  })

local({
  testResult<-sapply( as.character(setdiff(unique(IU_WIDE$Variable),'IGPI')),
                      function(variable) {
                        test<-t.test(value~Trt,IU_WIDE,var.equal=F,paired=T,subset=Variable==variable)
                        list(
                          variable=variable,
                          '$Pr(>|t|)$'= sprintf("$%e$",test$p.value),
                          '95 \\% CI'= sprintf("$[ %.2f, %.2f ]$", test$conf.int[1], test$conf.int[2] )
                          )
                      })[-1,]
  testResult<-t(testResult)
  
  print(
    xtable(testResult,
           align="|l|c|c|",
           "Rezultatele testului t pentru parametrii măsurati",
           "tab:resTestSummary",
           display=rep("g",3)),
    hline.after=-1:6,
    table.placement="H",
    sanitize.text.function=identity)
})

#imbunatatirea procentuala agregata in USS
print(100-sum(IU.USS$value[IU.USS$Trt=='POST'])/sum(IU.USS$value[IU.USS$Trt=='PRE'])*100)

print(
  xtable(
    lm(value~Trt+group,IU.FEFMP),
    "Rezultatele modelului linear pentru FEFMP",
    "tab:resFEFMPlm"),
  table.placement="H"
)

print(
  xtable(
    lm(value~Trt+Nasteri+group,IU.I2D),
    "Rezultatele modelului linear pentru I2D",
    "tab:resI2Dlm"),
  table.placement="H"
)

if (F) {

histogram(~value|Trt+Variable,IU_WIDE,type='density',scales=list(x="free"),
          panel=function(x,...){
            panel.histogram(x,...)
            panel.densityplot(x,plot.points=F,...)
          }
  )

glms<-lapply( as.character(setdiff(unique(IU_WIDE$Variable),'IGPI')),
              function(variable) {
                glmFit<-function(x) {
                  
                  ret<-glm(
                    value~Trt+Nasteri+Varsta,poisson,
                    IU_WIDE,subset=Variable==x)
                  ret<-lm(value~group,IU_WIDE,subset=Variable==x & Trt=='POST')
                  ret$call<-match.call()
                  ret
                }
                eval(call('glmFit',variable))
              }
)
lapply(glms,summary)

IU.FEFMP$Id<-seq(1,nrow(IU.FEFMP)/2)
IU.FEFMP$BMIc<-findInterval(IU.FEFMP$BMI,co.intervals(IU.FEFMP$BMI,overlap=0)[,1])
IU.FEFMP$Varstac<-findInterval(IU.FEFMP$Varsta,co.intervals(IU.FEFMP$Varsta,overlap=0)[,1])


m1<-glmer(value~Trt+group+(1|sex),data=IU.FEFMP,gaussian)
m2<-glmer(value~Trt+group+(1|sex),data=IU.FEFMP,gaussian)
anova( m1, m2, test="Chisq")
pvals.fnc(m2)

summary(lm(value~Trt+group,IU.FEFMP))
TukeyHSD(aov(value~Trt+group,IU.FEFMP),ordered=T)
manova(cbind(value,Variable)~Trt+group,IU.FEFMP))

IU.I2D$Id<-seq(1,nrow(IU.FEFMP)/2)
IU.I2D$VarstaC<-findInterval(IU.I2D$Varsta,seq(25,100,5))
IU.I2D$fefmp<-IU.FEFMP$value
  
summary(glm(value~Trt+group,poisson,IU.I2D))
summary(aov(value~Trt+group+Error(Varsta),IU.I2D))

histogram(~value|Trt+group,IU.I2D)
m1<-glmer(value~Trt+fefmp+(1|Id),IU.I2D,poisson)
m2<-glmer(value~Trt+(1|Id),IU.I2D,poisson)
m3<-glmer(value~Trt+(1|fefmp)+(1|Id),IU.I2D,poisson)
anova(m1,m2,m3)
pvals.fnc(m3)

anova(lm(value~Trt+sex,IU_WIDE,subset=Variable=='FEFMP'))

summary(
  glm(value>3~Trt+sex,binomial,IU_WIDE,subset=Variable=='FEFMP'))

summary(
  glm(value~Trt+sex,poisson,IU_WIDE,subset=Variable=='FEFMP'))

summary(
  glm(value~Trt+Nasteri,poisson,IU_WIDE,subset=Variable=='I2D'))

summary(
  glm(value~Trt+Nasteri+Varsta,poisson,IU_WIDE,subset=Variable=='I2D'))

anova(
  glm(value~Trt+group,poisson,IU_WIDE,subset=Variable=='I2D'))

summary(glms[[1]])
}