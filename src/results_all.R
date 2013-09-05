rm(list=ls())
source('src/common.R')
source('src/all-data.R')

#IU_WIDE<-reshape(IU,idvar=1:10,direction="long",varying=11:24,new.row.names=1:10000,timevar="TRT")

IU_WIDE<-IU[,c(1:2,7,11:24)]
IU_WIDE<-melt(IU_WIDE,id.vars=1:3)
IU_WIDE<-with(IU_WIDE,cbind(IU_WIDE[,1:3],
      colsplit(IU_WIDE$variable,"\\.",c("Variable","Trt")),
      value
     ))
IU_WIDE$Trt<-as.factor(IU_WIDE$Trt)
IU_WIDE$Variable<-as.factor(IU_WIDE$Variable)

bwplot(~value|Variable+Trt,IU_WIDE)

local({
  g.res.val.by.BMI<-
    xyplot(value~BMI|Variable+Trt,IU_WIDE,
           groups=Tip,
           type=c('p','r'),
           auto.key=T)

  png(filename="doc/img/incoResByBMI.png",width=540,height=540,bg=graph.bg)
  print(g.res.val.by.BMI)
  dev.off()

  g.res.val.by.Age<-xyplot(value~Varsta|Variable+Trt,IU_WIDE,
                           groups=Tip,
                           type=c('p','r'),auto.key=T)
  png(filename="doc/img/incoResByAge.png",width=540,height=540,bg=graph.bg)
  print(g.res.val.by.Age)
  dev.off()
})

sapply(
  unique(IU_WIDE$Variable),
  function (x) {
    data<-subset(IU_WIDE,Variable==x,c("value","Trt"))
    str(as.table(data))
    if(!any(is.na(data$value))){
      chisq_test(value~Trt,data)$p.value
    }
  }
)

