rm(list=ls())
source('src/common.R')
source('src/all-data.R')

IU<-IU[with(IU,order(Tip,Varsta)),]
rownames(IU)<-seq_along(IU[,1])
