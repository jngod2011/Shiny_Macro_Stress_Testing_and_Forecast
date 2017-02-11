serieslist<-unlist(strsplit("FRED/NROU,FRED/B060RC0A144NBEA",","))
ivlist<-c("FRED/GDP","FRED/PCECC96","FRED/RINV","FRED/LNU01000000","RATEINF/INFLATION_USA","PERTH/FEDF_M")

simdf<-read.csv('simdf.csv')
for (count in 1:(ncol(simdf)-1))
{
  for (i in 1:length(ivlist)){
    if (ivna[i]==1){
      latest<-Quandl(ivlist[ivna[i]],type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='Y',1:ncol(simdf)-1][,count]/100)-1)
      data<-as.data.frame((1+simirf)*latest[1,2])
    }
    if (ivna[i]==2){
      latest<-Quandl(ivlist[ivna[i]],type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='C',1:ncol(simdf)-1][,count]/100)-1)
      data<-as.data.frame((1+simirf)*latest[1,2])
    }
    if (ivna[i]==3){
      latest<-Quandl(ivlist[ivna[i]],type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='I',1:ncol(simdf)-1][,count]/100)-1)
      data<-as.data.frame((1+simirf)*latest[1,2])
    }
    if (ivna[i]==4){
      latest<-Quandl(ivlist[ivna[i]],type='raw')
      data<-as.data.frame(1+simdf[simdf$varname=='L',1:ncol(simdf)-1][,count])*latest[1,2]
    }
    if (ivna[i]==5){
      latest<-Quandl(ivlist[ivna[i]],type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='pi',1:ncol(simdf)-1][,count]/100)-1)
      data<-as.data.frame((1+simirf)*latest[1,2])
    }
    if (ivna[i]==6){
      latest<-Quandl(ivlist[ivna[i]],type='raw')
      data<-as.data.frame(1+simdf[simdf$varname=='R',1:ncol(simdf)-1][,count])*latest[1,2]
    }
    if (count==1 && i==1){
      indepdata<-data
    }else{
      indepdata<-cbind(indepdata,data)
    }
  }
}
n<-ncol(indepdata)/length(ivna)
for (i in 1:len(serieslist)){
  depvar<-dvdata[,i]
  indepvar<-ivdata
  modeldata<-na.omit(as.data.frame(na.locf(merge.zoo(depvar,indepvar))))
  colnames(modeldata)[1]<-str_replace(serieslist[i],"/",".")
  eval(parse(text=paste0("fit<-lm(",str_replace(serieslist[i],"/","."),"~.,data=modeldata)")))
  for (j in 1:n){
    indep<-indepdata[,((j-1)*length(ivlist)+1):(j*length(ivlist))]
    colnames(indep)<-colnames(modeldata)[2:ncol(modeldata)]
    pred<-predict(fit, newdata=indep)
    if (j==1){
      ncsim<-as.data.frame(pred)
    }else{
      ncsim<-cbind(ncsim,pred)
    }
  }
  write.csv(ncsim,paste0(str_replace(serieslist[count],"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_MV.csv"))
}
for (i in 1:14)
{
  if (i==1)
    str<-"Period_1"
  else
    str<-c(str,paste0("Period_",i))
}
library(WGCNA)
colQuantileC(ncsim, 0.5)


