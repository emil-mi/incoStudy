set.seed(1234)
data<-rbind(
  data.frame(Varsta=rkde(500,kde(IU$Varsta[IU$Tip=='F'],positive=T)),Tip='IU'),
  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanF),Tip='RO'))
data<-with(data,data[Varsta>=27 & Varsta<=77,])
wilcox.test(Varsta~Tip,data=data)

data<-rbind(
  data.frame(Varsta=rkde(500,kde(IU$Varsta[IU$Tip=='M'],positive=T)),Tip='IU'),
  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanM),Tip='RO'))
data<-with(data,data[Varsta>=30 & Varsta<=83,])
wilcox.test(Varsta~Tip,data=data)
