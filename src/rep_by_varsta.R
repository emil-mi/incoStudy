
source('src/functii.R')

set.seed(1234)
data<-rbind(
  data.frame(Varsta=rkde(500,kde(IU$Varsta[IU$Tip=='F'],positive=T)),Tip='IU'),
  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanF),Tip='RO'))
data<-with(data,data[Varsta>=27 & Varsta<=77,])

#Femeile sunt decalate cu 2 ani (iulie 2010-??? 2012)
wilcox.test(data$Varsta[data$Tip=='RO'],data$Varsta[data$Tip=='IU'],mu=-1.942)

#Datele sunt biased din cauza lui kde
#data<-rbind(
#  data.frame(Varsta=rkde(500,kde(IU$Varsta[IU$Tip=='M'],positive=T)),Tip='IU'),
#  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanM),Tip='RO'))
#data<-with(data,data[Varsta>=30 & Varsta<=83,])
#wilcox.test(data$Varsta[data$Tip=='RO'],data$Varsta[data$Tip=='IU'],mu=-8.9)

data<-rbind(
  data.frame(Varsta=IU$Varsta[IU$Tip=='M'],Tip='IU'),
  data.frame(Varsta=rep(VarstaRO2010$Varsta,VarstaRO2010$UrbanM),Tip='RO'))
data<-with(data,data[Varsta>=30 & Varsta<=83,])

#barbatii sunt dintr-un esantion masurat cu 7!!!! ani inaintea studiului curent
wilcox.test(data$Varsta[data$Tip=='RO'],data$Varsta[data$Tip=='IU'],mu=-8.9)

#Subesantionez datele ca sa mearga mai repede calculele
#data<-rbind(
#  data.frame(Varsta=IU$Varsta[IU$Tip=='M'],Tip='IU'),
#  data.frame(Varsta=sample(subset(data,Tip=='RO')$Varsta,100),Tip='RO')
#)
