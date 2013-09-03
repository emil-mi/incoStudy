rm(list=ls())
source('src/common.R')
source('src/all-data.R')

IU<-IU[with(IU,order(Tip,Varsta)),]
rownames(IU)<-seq_along(IU[,1])

#outliers 
#IU[IU$Varsta==30 & IU$Tip=='F',"Nasteri"]<-NA
#IU[IU$Varsta==32 & IU$Tip=='F',"Nasteri"]<-NA
#IU[IU$Varsta==37 & IU$Tip=='F',"Nasteri"]<-NA

data.icf<-data.frame(matrix(with(subset(IU,Tip=='F' & !is.na(Nasteri)),
  sapply(sort(Varsta),function(x) { 
    list(x=x,y=mean(Nasteri[Varsta<=x]))
    })
   ),ncol=2,byrow=T))

local({
  g.EIU.cumulNasteriICF<-xyplot(
    X2~X1,data.icf,type='b',ylim=c(0.3,1.5),panel=function(x,y,...) {
      panel.ablineq(h=1.3,label="ICF=1.3 (Statistica, 2011)",at=0.5,pos=3)
      panel.xyplot(x,y,...)
      panel.grid(v=-10,h=0)
      panel.loess(x,y,family="symmetric",col="red",method="loess")
    },
    par.settings=theEconomist.theme(),auto.key=T,xlab="Varsta",ylab="ICF"
    )
  png(filename="doc/img/incoNasteriICF.png",width=540,height=540,bg=graph.bg)
  print(g.EIU.cumulNasteriICF)
  dev.off()
})

histogram(~Nasteri|cut(Varsta-2,
                       breaks=c(seq(20,50,5),max(Varsta)),
                       right=F),
          IU,subset=Tip=='F',type='c',as.table=T,nint=4)

local({
  cat.Varsta<-with(IU[IU$Tip=='F',],cbind(matrix(seq(20,50,5)),matrix(c(seq(25,50,5),max(Varsta)))))
  cat.Varsta<-shingle(IU$Varsta,cat.Varsta)
  g.EIU.NasterioAgegHist<-histogram(~Nasteri|cat.Varsta,
            data=IU,subset=Tip=='F',
            par.settings=theEconomist.theme(),auto.key=T,
            xlab="Numar de persoane",ylab="Numar de nasteri",
            strip=function(...){
              args<-list(...)
              factor.levels<-with(args,factor.levels[which.panel])
              factor.levels<-floor(as.numeric(unlist(strsplit(
                sub("\\[ *(\\d.+), *(\\d.+)]","\\1 \\2",factor.levels),
                " ",
                TRUE))))
              args$var.name<-sprintf('Y%d-%d',factor.levels[1],factor.levels[2])
              do.call(strip.default,args)
            },
            from=0,to=3,type="c",nint=4,
            panel=function(x,...) {
                panel.histogram(x,...)
                h<-density(x,n=30,from=0,to=3)
                h$y<-max(by(x,x,length))*h$y/max(h$y)
                panel.lines(x=h$x,y=h$y,col='green',alpha=0.5)
              }
            ,as.table=T
            ,layout=c(2,3)
    )
  png(filename="doc/img/incoNasterioAgeHist.png",width=540,height=540,bg=graph.bg)
  print(g.EIU.NasterioAgegHist)
  dev.off()
})
