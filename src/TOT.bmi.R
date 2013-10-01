rm(list=ls(all.names=T))
source('src/common.R')
source('src/all-data.R')

rm(IU,TOT,AMB)
count_by_ageg<-c(
  sapply(list(18:24,25:44,45:64,65:74),function(r){sum(VarstaRO2010[VarstaRO2010$Varsta-2 %in% r,'TotalM'])}),
  sapply(list(18:24,25:44,45:64,65:74),function(r){sum(VarstaRO2010[VarstaRO2010$Varsta-2 %in% r,'TotalF'])}))

#Overweight and obesity - BMI statistics http://epp.eurostat.ec.europa.eu/statistics_explained/index.php/Overweight_and_obesity_-_BMI_statistics
#1: Source: Aspects of daily living survey 2009
ehis_percents<-c(11.0,34.9,61.2,57.5, 27.2,56.3,68.5,64.8)
ehis_percents<-c(ehis_percents,100-ehis_percents)

EHIS_OB <- data.frame(
  ageg=rep(c('Y18-24','Y25-44','Y45-64','Y65-74'),times=2),
  sex=rep(c('F','M'),each=4,times=2),
  bmig=as.factor(rep(c('OOB','NOR'),each=8)),
  count=round(ehis_percents*count_by_ageg/100),
  percent=ehis_percents
)

EHIS_OB<-EHIS_OB[with(EHIS_OB,order(ageg,Tip)),]
rownames(EHIS_OB)<-seq_along(EHIS_OB[,1])

rm(count_by_ageg,ehis_percents,VarstaRO2010)

fAgeRange<-function(age) {
  unlist(
    sapply( list(c(18,24),c(25,44),c(45,64),c(65,100)),
            function(r,v) { 
              if(v>=r[1] & v<=r[2]) sprintf('Y%d-%d',r[1],min(r[2],74))
            },
            age))
}

COMMON$ageg<-factor(with(COMMON,sapply(Varsta,fAgeRange)),levels=levels(EHIS_OB$ageg))
COMMON$bmig<-factor(with(COMMON,findInterval(BMI,c(25,30))),levels=0:2,labels=c('NOR','OVR','OBE'))
rm(fAgeRange)

#categoriile dupa care clasific
bmig.counts<-with(COMMON,expand.grid(levels(ageg),levels(Tip),levels(bmig)))
names(bmig.counts)<-c('ageg','Tip','bmig')

bmig.counts.ehis<-with(COMMON,expand.grid(levels(ageg),levels(Tip),factor(c('NOR','OOB'))))
names(bmig.counts.ehis)<-c('ageg','Tip','bmig')

#pentru fiecare categorie, numar cate randuri din IU se potrivesc
bmig.counts$count <- apply(
  bmig.counts,1,
  function(cr) nrow( subset(COMMON,ageg==cr[1] & Tip==cr[2] & bmig==cr[3]) )
)

bmig.counts.ehis$count <- apply(
  bmig.counts.ehis,1,
  function(cr) nrow( subset(COMMON,ageg==cr[1] & Tip==cr[2] &
                              (
                                (cr[3]=='NOR' & bmig=='NOR') | (cr[3]=='OOB' & bmig!='NOR') 
                              ) 
  ))
)


#calculez totalul pe grupa de varsta si sex(Tip)
bmig.counts.totals <- aggregate(count~ageg+Tip,bmig.counts,sum)

#calculez procentul pentru fiecare categorie (se repeta de 3 ori pentru ca avem normal,supraponderal,obez)
bmig.counts$percent <-
  bmig.counts$count/rep(aggregate(count~ageg+Tip,bmig.counts,sum)$count,times=nlevels(bmig.counts$bmig))*100

bmig.counts.ehis$percent <-
  bmig.counts.ehis$count/rep(aggregate(count~ageg+Tip,bmig.counts.ehis,sum)$count,times=nlevels(bmig.counts.ehis$bmig))*100

#reordonez datele ca sa fie grupate natural
bmig.counts<-bmig.counts[with(bmig.counts,order(ageg,Tip)),]
rownames(bmig.counts)<-seq_along(bmig.counts[,1])

bmig.counts.ehis<-bmig.counts.ehis[with(bmig.counts.ehis,order(ageg,Tip)),]
rownames(bmig.counts.ehis)<-seq_along(bmig.counts.ehis[,1])

#transform NaN in 0
bmig.counts$percent[is.nan(bmig.counts$percent)]<-0
bmig.counts.ehis$percent[is.nan(bmig.counts.ehis$percent)]<-0


combined<-rbind(
  cbind(subset(EHIS_OB,sex=='F',-c(sex)),data.frame(Tip='EHIS')),
  cbind(bmig.counts.ehis)
)

combined<-combined[with(combined,order(ageg,bmig,Tip)),]
rownames(combined)<-seq_along(combined[,1])

#sterg din combined grupa de varsta 18-24 pentru care nu am date in IU
#combined<-subset(combined,ageg!='Y18-24') # nu arata bine incoBMIvsEHIS-xxx daca lipseste 18-25 
combined$percent[combined$Tip=='EHIS' & combined$ageg=='Y18-24']<-0

#pregatesc datele pentru testul KS pentru femei normale
set.seed(123) #mai jos folosesc runif

data.f<-subset(combined,bmig=='NOR' & ageg!='Y18-24')[c("ageg","percent","Tip","count")]
data.f$percent<-floor(data.f$percent)
data.f$ageMin<-as.integer(substr(data.f$ageg,2,3))
data.f$ageMax<-as.integer(substr(data.f$ageg,5,6))


#plotting

styleGraph(stripplot(BMI~ageg|Tip,data=COMMON,
          panel=function(...){
            panel.stripplot(...)
            panel.abline(h= 25,col.line='yellow')
            panel.abline(h= 30,col.line='red')
          },
          scales=list(y='free',rot=0),
          type=c('p','a'),
          prepanel = function(x, y, ...) { 
            list(ylim = rev(range(COMMON$BMI)*c(0.9,1.1))) 
          }
))

styleGraph( histogram(~BMI|ageg+Tip,COMMON,panel=function(...){
  panel.histogram(...)
  panel.abline(v= 25,col.line='yellow')
  panel.abline(v= 30,col.line='red')
}))

#incoBMIvsEHIS
local({
  g.EHIS.vs.EIU<-styleGraph(
    stripplot(percent~ageg|bmig,combined,groups=Tip,type='b',main='',
              auto.key=list(text=c("EHIS","TOT","AMB"))
      )
    ,ylab="Procent din grupa de varsta",xlab="Grupa de varsta"
  )
  g.EHIS.vs.EIU$xscale.components <- function(...) { 
    ans<-xscale.components.subticks(...)
    ans$bottom$labels$labels<-with(ans$bottom$labels,sapply(labels,substr,2,100))
    ans
  }
  
  png(filename="doc/img/totBMIvsEHIS-full.png",width=540,height=540,bg=graph.bg)
  print(g.EHIS.vs.EIU)
  dev.off()
})

#incoBMIvsEHIS-OOB
local({
  g.EHIS.vs.EIU<-asTheEconomist(
    xyplot(percent~ageg|bmig,combined,groups=Tip,type='b',auto.key=T,main='',
           aspect='fill',subset= bmig=='OOB'
    ),ylab="Procent din grupa de varsta",xlab="Grupa de varsta",
  )
  g.EHIS.vs.EIU$xscale.components <- function(...) { 
    ans<-xscale.components.subticks(...)
    ans$bottom$labels$labels<-with(ans$bottom$labels,sapply(labels,substr,2,100))
    ans
  }
  
  png(filename="doc/img/totBMIvsEHIS-OOB.png",width=540,height=540,bg=graph.bg)
  print(g.EHIS.vs.EIU)
  dev.off()
})

#incoBMIbyBMIG
local({
  g.EIU.bmig<-styleGraph(
    barchart(percent~ageg|Tip,bmig.counts,groups=bmig,type='a',auto.key=T,main='')
    ,ylab="Procent din grupa de varsta",xlab="Grupa de varsta",
    scales = list( y = list(axs = "r",alternating = 2) )
  )
  g.EIU.bmig$xscale.components <- function(...) { 
    ans<-xscale.components.subticks(...)
    ans$bottom$labels$labels<-with(ans$bottom$labels,sapply(labels,substr,2,100))
    ans
  }
  png(filename="doc/img/totBMIbyBMIG.png",width=540,height=540,bg=graph.bg)
  print(g.EIU.bmig)
  dev.off()
})

#incobmiDens
local({
  g.EIU.bmiDens<-histogram(~BMI|Tip,data=COMMON,type='density',
                           auto.key=T,main='',ylab="Procent",
                           par.settings=theEconomist.theme(),
                           yscale.components=function(...)
                           {
                             ans<-yscale.components.default(...)
                             ans$left$labels$labels<-as.character(ans$left$labels$at*30/0.15)
                             ans
                           },
                           panel=function(x,breaks,...){
                             panel.grid(v=-10,h=0)
                             colOVR<-likertColorBrewer(8,BrewerPaletteName='RdYlGn')[1]
                             colOBE<-likertColorBrewer(8,BrewerPaletteName='RdYlGn')[3]
                             panel.rect(25,-1,30,max(x),col=colOBE,alpha=.3)
                             panel.rect(30,-1,max(breaks)+1,max(x),col=colOVR,alpha=.5)
                             panel.histogram(x,breaks,...)
                             panel.densityplot(x,plot.points=F,col='blue',alpha=0.5)
                           },
                           col="#5F92A8"
  )
  png(filename="doc/img/totBMIDens.png",width=540,height=540,bg=graph.bg)
  print(g.EIU.bmiDens)
  dev.off()
})

#diverse sumarizari
with(COMMON,by(BMI,Tip,summary))
as.data.frame(as.list(with(subset(bmig.counts,Tip=='IUE-AMB'),by(count,bmig,sum))))
as.data.frame(as.list(with(subset(bmig.counts,Tip=='IUE-TOT'),by(count,bmig,sum))))

#testele de corespondenta a distributiilor
ks.test(unlist(subset(data.f,Tip=='IUE-AMB')$count)+
        unlist(subset(data.f,Tip=='IUE-TOT')$count),
        unlist(subset(data.f,Tip=='EHIS')$count),exact=F)

ks.test(unlist(subset(data.f,Tip=='IUE-TOT')$count),
        unlist(subset(data.f,Tip=='EHIS')$count))

bmig.counts<-bmig.counts[with(bmig.counts,order(ageg,Tip,bmig)),]

print(
  xtable(bmig.counts,
         align="|l|c|c|c|c|p2[cm]|",
         "Numărul de persoane și procentul din totalul de persoane dintr-o grupa de vârstă din fiecare categorie \\ac{BMI} pe grupuri de tratament și pe grupa de vârstă",
         "tab:bmigCounts",
         display=c(rep("g",5),"f")),
  hline.after=-1:nrow(bmig.counts),
  table.placement="H",
  sanitize.text.function=identity)
