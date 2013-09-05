rm(list=ls())
source('src/common.R')
source('src/all-data.R')

t<-apply(IU,1,function(row){
  Filter(Negate(is.na),
    with(as.list( row ),
           list(
             ifelse(Diabet!="FALSE","DIABET",NA),
             ifelse(Bronsita_cronica!="FALSE","BRONSITA",NA),
             ifelse(Psi=='NU',NA,Psi)
           )         
         )
  )
})
names(t)<-NULL
t<-lapply(t,function(l) ifelse(length(l)==0,NA,l[[1]]))
t<-lapply(t,function(l) factor(l,unique(unlist(t))))
summary(IU$com)
IU$com<-t(data.frame(t))
by(IU,IU$com,nrow)

local({
  g.como.count.by.sex<-styleGraph(barchart(Varsta~com,aggregate(Varsta~Tip+com,IU,length),groups=Tip,
         stack=T,auto.key=T,scales = list(x = list(rot = 60 )),ylab=''))
  png(filename="doc/img/incoComoCntBySex.png",width=540,height=540,bg=graph.bg)
  print(g.como.count.by.sex)
  dev.off()
})
