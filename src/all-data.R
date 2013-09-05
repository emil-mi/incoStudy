#functii utile attach,by,nrow,seq,ifelse
#iris <- within(iris, ratio <- Sepal.Length / Sepal.Width)

#estimarea unui kernel fhat<-kde(Varsta.M,hpi(Varsta.M)) sau fhat <- bkde(x=Varsta.M)
#generarea de esantioane conform unui kernel rkde(n=[numarul],fhat=[distributia])

#To bootstrap from a histogram(bins=[],counts=[]): sample(bins, replace = TRUE, prob = counts) 
#To get a kde/density from a histogram: density(rep(bins, counts), bw="SJ") 

#Higher-Order Functions: Reduce,Filter,Find,Map,Negate,Position

#length,ave,tapply,unlist as.character

#apply 1 - pe randuri, 2 - pe coloane

#expand.grid face produs cartezian NxMxW, outer - produs v*u^t, inner- produs scalar u*v

rm(list=intersect(c('TOT','AMB','IU','COMMON','IU_WIDE'),ls()))

psiValues<-c(
  'NU',
  'DEPRESIE',
  'PARKINSON',
  'AVC',
  'MIELITA',
  'FRACTURA COLOANA',
  'FRACTURA VERTEBRALA',
  'SPINA BIFIDA')

columnClasses<-c('factor',rep('numeric',4),'logical','numeric',
                 rep('logical',2),'psi.factor',rep('numeric',12))
extColumnClasses<-c(columnClasses,rep('numeric',2))

setClass('psi.factor')
setAs("character","psi.factor", function(from) ordered(x=from, levels=psiValues ))

TOT <- read.csv("excel/TOT.csv",colClasses=columnClasses)
AMB <- read.csv("excel/AMB.csv",colClasses=columnClasses)
IU <- read.csv("excel/IU.csv",colClasses=extColumnClasses)
COMMON<-rbind(TOT[c(1:10)],AMB[c(1:10)])
VarstaRO2010.insse <- read.table("excel/VarstaRO2010.txt", header=T, colClasses=rep('numeric',7))
VarstaRO2010 <- data.frame(
  Varsta=VarstaRO2010.insse$Varsta,
  
  TotalM=VarstaRO2010.insse$TotalBoth-VarstaRO2010.insse$TotalF,
  TotalF=VarstaRO2010.insse$TotalF,
  Total=VarstaRO2010.insse$TotalBoth,

  UrbanM=VarstaRO2010.insse$UrbanBoth-VarstaRO2010.insse$UrbanF,
  UrbanF=VarstaRO2010.insse$UrbanF,
  Urban=VarstaRO2010.insse$UrbanBoth,
  
  RuralM=VarstaRO2010.insse$RuralBoth-VarstaRO2010.insse$RuralF,
  RuralF=VarstaRO2010.insse$RuralF,
  Rural=VarstaRO2010.insse$RuralBoth
  )

#tempCleanup
suppressWarnings( rm(extColumnClasses,columnClasses,psiValues,VarstaRO2010.insse) )

# #Investigatii varsta
if (F) {
qqmath(~Varsta , data=IU , groups=Tip , auto.key=TRUE , 
       f.value=ppoints(100) , 
       type=c('p','g'),aspect='xy',xlab='')

asTheEconomist(densityplot(~Varsta,data=IU,groups=Tip,auto.key=T,plot.points=F,main='',
                           panel=function(...){
                             panel.densityplot(...)
                             panel.xyplot(0:110,0.6*dnorm(0:110,46,12.3)+0.38*dnorm(0:110,75,9.2))
                             }))

dp1<-asTheEconomist(
  densityplot(~Varsta,data=IU,groups=Tip,auto.key=T,plot.points=F,main='',ylab=''),
  xlab='Varsta'
  )
dp2<-bwplot(Varsta~Tip,data=IU,ylab='Varsta',xlab='Sex',par.settings=theEconomist.theme())

#sterg etichetele de la scala y. Nu am gasit un alt mod pana acum
dp1$yscale.components.old<-dp1$yscale.components
dp1$yscale.components<-function(...) { 
  ret<-dp1$yscale.components.old(...)
  ret$left$labels$labels=NULL
  ret
}
png(filename="doc/img/incoVarstaSex.png",width=540,height=540,bg=graph.bg)
print(dp1,split = c(1, 1, 1, 2))
print(dp2,split = c(1, 2, 1, 2),newpage=FALSE)
dev.off()

}
