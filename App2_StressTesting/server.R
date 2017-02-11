#load libraries
library(Quandl)
library(shiny)
library(ggplot2)
library(reshape)
require(rCharts)
library(reshape2)
library(shinyBS)
library(forecast)
library(mc2d)
library(stringr)
library(matrixStats)
library(DT)
library(googleVis)
library(sqldf)
library(gEcon)
library(DataCombine)
library(knitr)
library(fBasics)
load("shocks.RData")
sw_gecon_orig <- make_model('SW_03.gcn')
#register at quandl
Quandl.api_key("y7xCCmMxsc8_nddWS-yz")
#return desc function
descfun<-function(series,transform, frequency){
  eval(parse(text=paste0("data<-Quandl(code='",series,"',type='raw',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  desctab<-as.data.frame(unlist(metaData(data)))
  colnames(desctab)<-paste0('Data Description for ',series)
  return(desctab)
}
foresimulate1<-function(serieslist,data,forecount,simcount)
{
  rown<-nrow(data)
  coln<-ncol(data)
  for (i in 2:coln)
  {
    data <- FillDown(data, colnames(data)[i])
  }
  write.csv(data,'test.csv')
  for (count in 1:length(serieslist)){
    curr<-data[,count]
    curr<-ts(curr)
    fit <- auto.arima(curr, max.p=5, max.q=5,
                      max.P=2, max.Q=2, max.order=5, max.d=2, max.D=2, 
                      start.p=0, start.q=0, start.P=0, start.Q=0, 
                      stationary=FALSE, seasonal=TRUE,
                      ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=FALSE,
                      approximation=(length(lh)>100 | frequency(lh)>12), xreg=NULL,
                      test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
                      allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE, num.cores=2)
    forecasts<-forecast(fit, h= as.numeric(forecount))
    write.csv(forecasts,paste0(str_replace(serieslist[count],"/","."),"_",Sys.Date(),"_",forecount,"_UV.csv"))
    ndvar<-as.numeric(simcount)
    shockmat<-matrix(0,as.numeric(simcount),as.numeric(forecount))
    for (i in 1:as.numeric(forecount))
    {
      shockmat[,i]<-mcstoc(rnorm,type="V",mean=mean(fit$residuals,na.rm=TRUE),sd=sd(fit$residuals,na.rm=TRUE))[1:as.numeric(simcount),1,1]
      shockmat[,i]<-shockmat[,i]+forecasts$mean[i]
    }
    shockmat<-as.data.frame(shockmat)
    write.csv(shockmat,paste0(str_replace(serieslist[count],"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_UV.csv"))   
  }
}
simfun1<-function(serieslist,series,transform, frequency,view,forecount,simcount)
{
  serieslist<-unlist(strsplit(serieslist,","))
  if (length(serieslist)==1)
  {
    #get data
    eval(parse(text=paste0("data<-Quandl(code=",serieslist,",type='raw',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  }else{
    for (count in 1:length(serieslist)){
      if (count==1){
        str<-paste0("'",serieslist[count],"',")
      }else if (count<length(serieslist)){
        str<-paste0(str,"'",serieslist[count],"',")
      }else{
        str<-paste0(str,"'",serieslist[count],"'")
      }
    }
    str<-paste0("c(",str,")")
    #get data
    eval(parse(text=paste0("data<-Quandl(code=",str,",type='ts',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  }
  if(view=="Forecast"){
    if(!file.exists(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_UV.csv")))){
      foresimulate1(serieslist,data,forecount,simcount)
    }
    finaldata<-read.csv(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_UV.csv")))
    colnames(finaldata)<-c("Period","Prediction","Lo.80","Hi.80","Lo.95","Hi.95")
  }else
  {
    if(!file.exists(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_UV.csv")))){
      foresimulate1(serieslist,data,forecount,simcount)
    }
    print(series)
    finaldata<-read.csv(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_UV.csv")))
    for (i in 1:forecount)
    {
      if (i==1)
        str<-"Period_1"
      else
        str<-c(str,paste0("Period_",i))
    }
    colnames(finaldata)<-c("Sim#",str)
  }
  return(finaldata)  

  
}
simfun2<-function(iv1,iv2,iv3,iv4,iv5,iv6,serieslist,series,transform, frequency,view,forecount,simcount)
{
  serieslist<-unlist(strsplit(serieslist,","))
  ivlist<-c(iv1,iv2,iv3,iv4,iv5,iv6)
  ivna<-which(ivlist !="NA")
  ivlist<-ivlist[ivlist!="NA"]
  if (length(serieslist)==1)
  {
    #get data
    eval(parse(text=paste0("dvdata<-Quandl(code=",serieslist,",type='raw',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  }else{
    for (count in 1:length(serieslist)){
      if (count==1){
        str<-paste0("'",serieslist[count],"',")
      }else if (count<length(serieslist)){
        str<-paste0(str,"'",serieslist[count],"',")
      }else{
        str<-paste0(str,"'",serieslist[count],"'")
      }
    }
    str<-paste0("c(",str,")")
    #get data
    eval(parse(text=paste0("dvdata<-Quandl(code=",str,",type='raw',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  }
  if (length(ivlist)==1)
  {
    #get data
    eval(parse(text=paste0("ivdata<-Quandl(code=",ivlist,",type='raw',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  }else{
    for (count in 1:length(ivlist)){
      if (count==1){
        str<-paste0("'",ivlist[count],"',")
      }else if (count<length(ivlist)){
        str<-paste0(str,"'",ivlist[count],"',")
      }else{
        str<-paste0(str,"'",ivlist[count],"'")
      }
    }
    str<-paste0("c(",str,")")
    #get data
    eval(parse(text=paste0("ivdata<-Quandl(code=",str,",type='raw',",ifelse(transform=="NULL","transform=NULL,",paste0("transform='",transform,"',")),ifelse(frequency=="NULL","collapse=NULL,",paste0("collapse='",frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
  }
  
  if(view=="Forecast"){
    if(!file.exists(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,".csv")))){
      if(!file.exists(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_MV.csv")))){
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
      }
      finaldatasim<-read.csv(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_MV.csv")))
      for (i in 1:forecount)
      {
        if (i==1)
          str<-"Period_1"
        else
          str<-c(str,paste0("Period_",i))
      }
      colnames(finaldatasim)<-c("Sim#",str)
      
      
      
    }
    finaldata<-read.csv(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,".csv")))
    colnames(finaldata)<-c("Period","Prediction","Lo.80","Hi.80","Lo.95","Hi.95")
  }else{
    if(!file.exists(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_MV.csv")))){
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
    }
    finaldata<-read.csv(paste0(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",forecount,"_",simcount,"_MV.csv")))
    for (i in 1:forecount)
    {
      if (i==1)
        str<-"Period_1"
      else
        str<-c(str,paste0("Period_",i))
    }
    colnames(finaldata)<-c("Sim#",str)
  }
  return(finaldata)  
  
  
}
shinyServer(function(input, output, session) {
  observe({
    #rearrange forecast simulation chooser
    updateSliderInput(session, "control", value = 1,min = 1, max = as.numeric(input$simcount), step =1)
  })
  output$chart1 <- renderChart2({
    serieslist<-unlist(strsplit(input$dataseries,","))
    # Create chart
    if (length(serieslist)==1)
    {
      #get data
      eval(parse(text=paste0("data<-Quandl(code='",serieslist,"',type='raw',",ifelse(input$transform=="NULL","transform=NULL,",paste0("transform='",input$transform,"',")),ifelse(input$frequency=="NULL","collapse=NULL,",paste0("collapse='",input$frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
      #format Date
      plotdata = transform(data,Date2 = as.numeric(as.POSIXct(DATE))*1000)
      plotdata$DATE= NULL
      plotdata$DATE=plotdata$Date2
      plotdata$Date2=NULL
      h1 <- hPlot(VALUE ~ DATE, data = plotdata,type = "line", radius=6)
      h1$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'),title = list(text = paste0(attributes(data)$freq,"_date from ",attributes(data)$meta$oldest_available_date," to ",attributes(data)$meta$newest_available_date)))
      h1$yAxis(title = list(text = paste0(attributes(data)$meta$dataset_code,"_ from ",attributes(data)$meta$database_code )))
    }else{
      for (count in 1:length(serieslist)){
        if (count==1){
          str<-paste0("'",serieslist[count],"',")
        }else if (count<length(serieslist)){
          str<-paste0(str,"'",serieslist[count],"',")
        }else{
          str<-paste0(str,"'",serieslist[count],"'")
        }
      }
      str<-paste0("c(",str,")")
      #get data
      eval(parse(text=paste0("data<-Quandl(code=",str,",type='raw',",ifelse(input$transform=="NULL","transform=NULL,",paste0("transform='",input$transform,"',")),ifelse(input$frequency=="NULL","collapse=NULL,",paste0("collapse='",input$frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
      #format Date
      plotdata = transform(data,Date2 = as.numeric(as.POSIXct(DATE))*1000)
      plotdata$DATE= NULL
      plotdata$DATE=plotdata$Date2
      plotdata$Date2=NULL
      data<-reshape2::melt(plotdata, id.vars = "DATE")
      h1<-hPlot(x = "DATE", y = "value", group = "variable", data = data)
      h1$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'),title = list(text = 'Time'))
      h1$yAxis(title = list(text = paste0('Value')))
    }
    return(h1)
  })
  output$Descselect = renderUI({
    selectInput('Descselect2', 'Choose Variable to show Description', serieslist<-unlist(strsplit(input$dataseries,",")))
  })
  descdata <- reactive(descfun(input$Descselect2,input$transform,input$frequency))
  output$desctab <- renderTable({ 
    desctab<-descdata()
  },include.rownames=TRUE)
  output$downloadData1 <- downloadHandler(
    filename = function() { paste('QuandlSeries',Sys.time(), ifelse(input$Format=='csv','.csv','.xls'), sep='') },
    content = function(file) {
      serieslist<-unlist(strsplit(input$dataseries,","))
      # Create chart
      if (length(serieslist)==1)
      {
        #get data
        eval(parse(text=paste0("data<-Quandl(code='",serieslist,"',type='raw',",ifelse(input$transform=="NULL","transform=NULL,",paste0("transform='",input$transform,"',")),ifelse(input$frequency=="NULL","collapse=NULL,",paste0("collapse='",input$frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
      }else{
        for (count in 1:length(serieslist)){
          if (count==1){
            str<-paste0("'",serieslist[count],"',")
          }else if (count<length(serieslist)){
            str<-paste0(str,"'",serieslist[count],"',")
          }else{
            str<-paste0(str,"'",serieslist[count],"'")
          }
        }
        str<-paste0("c(",str,")")
        #get data
        eval(parse(text=paste0("data<-Quandl(code=",str,",type='raw',",ifelse(input$transform=="NULL","transform=NULL,",paste0("transform='",input$transform,"',")),ifelse(input$frequency=="NULL","collapse=NULL,",paste0("collapse='",input$frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
        #format Date
        data<-reshape2::melt(data, id.vars = "DATE")
      }
      write.csv(data, file)
    }
  )
  output$downloadSimData <- downloadHandler(
    filename = function() { paste('CoreSimulation_',Sys.time(), ifelse(input$Formatcoresim=='csv','.csv','.xls'), sep='') },
    content = function(file) {
      data=read.csv("simdf.csv")
      write.csv(data, file)
    }
  )
  output$Varselect = renderUI({
    selectInput('Varselect2', 'Choose Variable to show Plot and Data', serieslist<-unlist(strsplit(input$dataseriessim,",")))
  })
  datasim1 <- reactive(simfun1(input$dataseriessim,input$Varselect2,input$transform,input$frequency,input$view,as.numeric(input$forecount),as.numeric(input$simcount)))
  datasim2 <- reactive(simfun2(ifelse(input$Simcorenc1==TRUE,input$Simcorenc1var,"NA"),ifelse(input$Simcorenc2==TRUE,input$Simcorenc2var,"NA"),ifelse(input$Simcorenc3==TRUE,input$Simcorenc3var,"NA"),ifelse(input$Simcorenc4==TRUE,input$Simcorenc4var,"NA"),ifelse(input$Simcorenc5==TRUE,input$Simcorenc5var,"NA"),ifelse(input$Simcorenc5==TRUE,input$Simcorenc5var,"NA"),input$dataseriessim,input$Varselect2,input$transform,input$frequency,input$view,as.numeric(input$forecount),as.numeric(input$simcount)))
  
  output$simtab<-DT::renderDataTable({
    if (input$simmet=="Univariate Method"){
      simtab<-datasim1()
    }else{
      simtab<-datasim2()
    }
    
  })
  output$chart2 <- renderChart2({
    series<-input$Varselect2
    if (input$simmet=="Univariate Method"){
      data<-read.csv(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",input$forecount,"_UV.csv"))
      datasim<-read.csv(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,"_UV.csv"))
    }else{
      data<-read.csv(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",input$forecount,"_MV.csv"))
      datasim<-read.csv(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,"_MV.csv"))
    }
    alldatasim<-input$dataseriessim
    serieslist<-unlist(strsplit(input$dataseries,","))
    iddata<-match(series,serieslist)
    sign=unlist(strsplit(input$signind,","))
    sign=sign[iddata]
    if (sign=="+"){
      if (input$simoutlook=="Very Positive")
      {
        prc=0.99
      }else if (input$simoutlook=="Positive"){
        prc=0.75
      }else if (input$simoutlook=="Neutral"){
        prc=0.5
      }else if (input$simoutlook=="Negative"){
        prc=0.25
      }else if (input$simoutlook=="Very Negative"){
        prc=0.01
      }
    }else{
      if (sign=="-"){
        if (input$simoutlook=="Very Positive")
        {
          prc=0.01
        }else if (input$simoutlook=="Positive"){
          prc=0.25
        }else if (input$simoutlook=="Neutral"){
          prc=0.5
        }else if (input$simoutlook=="Negative"){
          prc=0.75
        }else if (input$simoutlook=="Very Negative"){
          prc=0.99
        }
      }
    }
    save(datasim, prc,file="test.RData")
    outlook<-matrixStats::colQuantiles(data.matrix(datasim),probs=seq(from=prc, to=prc, by=0))[2:ncol(datasim)]
    data<-cbind(data,outlook)
    data<-reshape2::melt(data, id.vars = "X")
    h1<-hPlot(x = "X", y = "value", group = "variable", data = data)
    h1$yAxis(title = list(text = paste0("Values")))
    h1$title(text = paste0("Outlook and Average Forecast for _",series))
    return(h1)
  })
  output$noncorestats <-renderTable({
    series<-input$Varselect2
    if (input$simmet=="Univariate Method"){
      data<-read.csv(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",input$forecount,"_UV.csv"))
    }else{
      data<-read.csv(paste0(str_replace(series,"/","."),"_",Sys.Date(),"_",input$forecount,"_MV.csv"))
    }
    data<-data[,as.numeric(input$perselect2)+1]
    data<-as.data.frame(data)
    colnames(data)<-paste0("Simulation_Period_",input$perselect2)
    basicStats(data)

  },include.rownames=TRUE)
  output$Varsimselect = renderUI({
    selectInput('Varsimselect2', 'Choose Simulation File', serieslist<-unlist(strsplit(input$dataseriessim,",")))
  })
  output$downloadData2 <- downloadHandler(
    filename = function() { paste('QuandlSeries_Sim',Sys.time(), ifelse(input$Format=='csv','.csv','.xls'), sep='') },
    content = function(file) {
      data<-read.csv(paste0(str_replace(input$Varsimselect2,"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,".csv"))
      write.csv(data, file)
    }
  )
  output$perselect = renderUI({
    textInput("perselect2", "Enter Period:", "5")
  })
  output$varsimselect = renderUI({
    selectInput('varsimselect2', 'Choose Variable to show Histogram', serieslist<-unlist(strsplit(input$dataseriessim,",")))
  })
  output$corevarselect = renderUI({
    varlist<-NULL
    if (input$core1==TRUE){
      varlist<-c(varlist,"Real GDP growth")
    }
    if (input$core2==TRUE){
      varlist<-c(varlist,"Real Consumption growth")
    }
    if (input$core3==TRUE){
      varlist<-c(varlist,"Real Investment growth")
    }
    if (input$core4==TRUE){
      varlist<-c(varlist,"Aggregate Hours")
    }
    if (input$core5==TRUE){
      varlist<-c(varlist,"Core PCE Inflation")
    }
    if (input$core6==TRUE){
      varlist<-c(varlist,"Fed funds rate")
    }
    selectInput('corevarselect2', 'Choose Core Variable to show Histogram', varlist)
  })
  output$coreperselect = renderUI({
    selectInput('coreperselect2', 'Choose Period to show Histogram', seq(1,as.numeric(input$forecount),1))
  })
  output$coreDist <- renderGvis({
    if(input$corevarselect2=="Real GDP growth"){
      var='Y'
    }else if(input$corevarselect2=="Real Consumption growth"){
      var='C'
    }else if(input$corevarselect2=="Real Investment growth"){
      var='I'
    }else if(input$corevarselect2=="Aggregate Hours"){
      var='L'
    }else if(input$corevarselect2=="Core PCE Inflation"){
      var='pi'
    }else if(input$corevarselect2=="Fed funds rate"){
      var='Y'
    }
    if (input$frequencycore=="daily"){
      ann=365
    }else if (input$frequencycore=="weekly"){
      ann=52
    }else if (input$frequencycore=="monthly"){
      ann=12
    }else if (input$frequencycore=="quarterly"){
      ann=4
    }else if (input$frequencycore=="annual"){
      ann=1
    }else{
      ann=4
    }
    simdf<-read.csv('simdf.csv')
    if (var=='Y'){
      latest<-Quandl(input$Fcore1var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='Y',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-as.data.frame((1+simirf)*latest[1,2])
      colnames(data)<-"Real GDP"
    }
    if (var=='C'){
      latest<-Quandl(input$Fcore2var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='C',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-t(1+simirf)*latest[1,2]
      colnames(data)<-"Real Consumption"
    }
    if (var=='I'){
      latest<-Quandl(input$Fcore3var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='I',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-t(1+simirf)*latest[1,2]
      colnames(data)<-"Real Investment"
    }
    if (var=='L'){
      latest<-Quandl(input$Fcore4var,type='raw')
      data<-as.data.frame(1+simdf[simdf$varname=='L',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)])*latest[1,2]
      colnames(data)<-"Aggregate Hours"
    }
    if (var=='pi'){
      latest<-Quandl(input$Fcore5var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='pi',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-t(1+simirf)*latest[1,2]
      colnames(data)<-"Core PCE Inflation"
    }
    if (var=='R'){
      latest<-Quandl(input$Fcore6var,type='raw')
      data<-as.data.frame(1+simdf[simdf$varname=='R',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)])*latest[1,2]
      colnames(data)<-"Fed funds rate"
    }
    gvis.options <- list(hAxis="{title:'Values'}",
                         width=900, height=600)
    hist.gvis <- gvisHistogram(data=data, option=gvis.options)
    return(hist.gvis)
  })
  output$corestats<-renderTable({
    if(input$corevarselect2=="Real GDP growth"){
      var='Y'
    }else if(input$corevarselect2=="Real Consumption growth"){
      var='C'
    }else if(input$corevarselect2=="Real Investment growth"){
      var='I'
    }else if(input$corevarselect2=="Aggregate Hours"){
      var='L'
    }else if(input$corevarselect2=="Core PCE Inflation"){
      var='pi'
    }else if(input$corevarselect2=="Fed funds rate"){
      var='Y'
    }
    if (input$frequencycore=="daily"){
      ann=365
    }else if (input$frequencycore=="weekly"){
      ann=52
    }else if (input$frequencycore=="monthly"){
      ann=12
    }else if (input$frequencycore=="quarterly"){
      ann=4
    }else if (input$frequencycore=="annual"){
      ann=1
    }else{
      ann=4
    }
    simdf<-read.csv('simdf.csv')
    if (var=='Y'){
      latest<-Quandl(input$Fcore1var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='Y',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-as.data.frame((1+simirf)*latest[1,2])
      colnames(data)<-"Real GDP"
    }
    if (var=='C'){
      latest<-Quandl(input$Fcore2var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='C',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-t(1+simirf)*latest[1,2]
      colnames(data)<-"Real Consumption"
    }
    if (var=='I'){
      latest<-Quandl(input$Fcore3var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='I',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-t(1+simirf)*latest[1,2]
      colnames(data)<-"Real Investment"
    }
    if (var=='L'){
      latest<-Quandl(input$Fcore4var,type='raw')
      data<-as.data.frame(1+simdf[simdf$varname=='L',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)])*latest[1,2]
      colnames(data)<-"Aggregate Hours"
    }
    if (var=='pi'){
      latest<-Quandl(input$Fcore5var,type='raw')
      simirf<-ann*100*(exp(simdf[simdf$varname=='pi',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)]/100)-1)
      data<-t(1+simirf)*latest[1,2]
      colnames(data)<-"Core PCE Inflation"
    }
    if (var=='R'){
      latest<-Quandl(input$Fcore6var,type='raw')
      data<-as.data.frame(1+simdf[simdf$varname=='R',1:ncol(simdf)-1][,as.numeric(input$coreperselect2)])*latest[1,2]
      colnames(data)<-"Fed funds rate"
    }
    basicStats(data)
    
  })
  
  output$Dist <- renderGvis({
    series<-input$Varselect2
    if (input$simmet=="Univariate Method"){
      data<-read.csv(paste0(str_replace(input$varsimselect2,"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,"_UV.csv"))
    }else{
      data<-read.csv(paste0(str_replace(input$varsimselect2,"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,"_MV.csv"))
    }
    
    data<-data[,as.numeric(input$perselect2)+1]
    data<-as.data.frame(data)
    colnames(data)<-paste0("Simulation_Period_",input$perselect2)
    gvis.options <- list(hAxis="{title:'Values'}",
                         width=900, height=600)
    hist.gvis <- gvisHistogram(data=data, option=gvis.options)
    return(hist.gvis)
  })
  output$chart3 <- renderChart2({
    #get dv
    if (input$depvarchc=="Input")
    {
      #get data
      eval(parse(text=paste0("dvdata<-Quandl(code='",input$depvar,"',type='raw',",ifelse(input$transform=="NULL","transform=NULL,",paste0("transform='",input$transform,"',")),ifelse(input$frequency=="NULL","collapse=NULL,",paste0("collapse='",input$frequency,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
      colnames(dvdata)<-c('DATE',str_replace(input$depvar,",","."))
    }else{
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      dvdata<-read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote)
    }
    #get iv
    serieslist<-unlist(strsplit(input$foreseriessim,","))
    if (input$Fcore1==TRUE){
      serieslist<-c(serieslist,'FRED/GDP')
    }
    if (input$Fcore2==TRUE){
      serieslist<-c(serieslist,'FRED/PCECC96')
    }
    if (input$Fcore3==TRUE){
      serieslist<-c(serieslist,'FRED/RINV')
    }
    if (input$Fcore4==TRUE){
      serieslist<-c(serieslist,'FRED/LNU01000000')
    }
    if (input$Fcore5==TRUE){
      serieslist<-c(serieslist,'RATEINF/INFLATION_USA')
    }
    if (input$Fcore6==TRUE){
      serieslist<-c(serieslist,'PERTH/FEDF_M')
    }
    # Create chart
    if (length(serieslist)==1)
    {
      #get data
      eval(parse(text=paste0("data<-Quandl(code='",serieslist,"',type='raw',",ifelse(input$transformfore=="NULL","transform=NULL,",paste0("transform='",input$transformfore,"',")),ifelse(input$frequencyfore=="NULL","collapse=NULL,",paste0("collapse='",input$frequencyfore,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
    }else{
      for (count in 1:length(serieslist)){
        if (count==1){
          str<-paste0("'",serieslist[count],"',")
        }else if (count<length(serieslist)){
          str<-paste0(str,"'",serieslist[count],"',")
        }else{
          str<-paste0(str,"'",serieslist[count],"'")
        }
      }
      str<-paste0("c(",str,")")
      #get data
      eval(parse(text=paste0("data<-Quandl(code=",str,",type='raw',",ifelse(input$transformfore=="NULL","transform=NULL,",paste0("transform='",input$transformfore,"',")),ifelse(input$frequencyfore=="NULL","collapse=NULL,",paste0("collapse='",input$frequencyfore,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
    }
    options(sqldf.driver = "SQLite") # as per FAQ #7 force SQLite
    options(gsubfn.engine = "R") # as per FAQ #5 use R code rather than tcltk
    finaldf<-sqldf('select data.*,dvdata.* from data inner join dvdata on data.DATE=dvdata.DATE')
    finaldf<-subset(finaldf,select=c('DATE',colnames(data)[2:ncol(data)],colnames(dvdata)))
    finaldf$DATE.1<-NULL
    rown<-nrow(finaldf)
    coln<-ncol(finaldf)
    for (i in 1:coln)
    {
      finaldf <- FillDown(finaldf, colnames(finaldf)[i])
    }
    #fit arima
    tsdf<-ts(finaldf)
    xreg<-tsdf[,2:(ncol(tsdf)-1)]
    dvdata<-tsdf[,ncol(tsdf)]
    fit <- auto.arima(dvdata, max.p=5, max.q=5,
                      max.P=2, max.Q=2, max.order=5, max.d=1, max.D=2, 
                      start.p=0, start.q=0, start.P=0, start.Q=0, 
                      stationary=FALSE, seasonal=TRUE,
                      ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=FALSE,
                      approximation=(length(lh)>100 | frequency(lh)>12),
                      test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
                      allowdrift=TRUE, allowmean=TRUE, lambda=NULL, parallel=FALSE, num.cores=2,xreg=xreg)
    saveRDS(fit, paste0(str_replace(input$depvar,"/","."),"_fit.rds"))
    output$plotsummary <- renderPrint({summary(fit)})
    plotdata<-as.data.frame(cbind(fit$x,fitted(fit)))
    colnames(plotdata)<-c(input$depvar,paste0("Fitted_",input$depvar))
    plotdata$Date<-as.character(finaldf$DATE)
    plotdata<-reshape2::melt(plotdata, id.vars = "Date")
    h1<-hPlot(x = "Date", y = "value", group = "variable", data = plotdata)
    h1$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%m-%d}'),title = list(text = 'Time'))
    h1$yAxis(title = list(text = paste0('Value')))
    h1$title(text=paste0("Fit plot for ", input$depvar))
    return(h1)
  })
  output$chart4 <- renderChart2({
    #get iv
    serieslist<-unlist(strsplit(input$foreseriessim,","))
    signarr=unlist(strsplit(input$signfore,","))
    
    # Create chart
    for (i in 1:length(serieslist))
    {
      sign=signarr[i]
      if (sign=="+"){
        if (input$foreoutlook=="Very Positive")
        {
          prc=0.99
        }else if (input$foreoutlook=="Positive"){
          prc=0.75
        }else if (input$foreoutlook=="Neutral"){
          prc=0.5
        }else if (input$foreoutlook=="Negative"){
          prc=0.25
        }else if (input$foreoutlook=="Very Negative"){
          prc=0.01
        }
      }else{
        if (sign=="-"){
          if (input$foreoutlook=="Very Positive")
          {
            prc=0.01
          }else if (input$foreoutlook=="Positive"){
            prc=0.25
          }else if (input$foreoutlook=="Neutral"){
            prc=0.5
          }else if (input$foreoutlook=="Negative"){
            prc=0.75
          }else if (input$foreoutlook=="Very Negative"){
            prc=0.99
          }
        }
      }
      series<-input$Varselect2
      if (input$simmet=="Univariate Method"){
        data<-read.csv(paste0(str_replace(serieslist[i],"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,"_UV.csv"))
      }else{
        data<-read.csv(paste0(str_replace(serieslist[i],"/","."),"_",Sys.Date(),"_",input$forecount,"_",input$simcount,"_MV.csv"))
      }
      
      outlook<-matrixStats::colQuantiles(data.matrix(data),probs=seq(from=prc, to=prc, by=0))[2:ncol(data)]
      if (i==1)
      {
        xreg<-outlook
      }else{
        xreg<-cbind(xreg,outlook)
      }
    }
    xreg<-data.frame(xreg)
    colnames(xreg)<-paste0(str_replace(serieslist,"/",".")," - VALUE")
    #find Annualisation COnst
    if (input$frequencyfore=="daily"){
      ann=365
    }else if (input$frequencyfore=="weekly"){
      ann=52
    }else if (input$frequencyfore=="monthly"){
      ann=12
    }else if (input$frequencyfore=="quarterly"){
      ann=4
    }else if (input$frequencyfore=="annual"){
      ann=1
    }else{
      ann=4
    }
    #get core variables
    n<-input$control
    simdf<-read.csv('simdf.csv')
    if (input$Fcore1==TRUE){
      latest<-Quandl(input$Fcore1var,type='raw')
      conxreg<-colnames(xreg)
      simirf<-ann*100*(exp(simdf[simdf$varname=='Y',1:ncol(simdf)-1][n,]/100)-1)
      xreg$Fore1<-t(1+simirf)*latest[1,2]
      colnames(xreg)<-c(conxreg,paste0(str_replace(input$Fcore1var,"/",".")," - VALUE"))
    }
    if (input$Fcore2==TRUE){
      latest<-Quandl(input$Fcore2var,type='raw')
      conxreg<-colnames(xreg)
      simirf<-ann*100*(exp(simdf[simdf$varname=='C',1:ncol(simdf)-1][n,]/100)-1)
      xreg$Fore2<-t(1+simirf)*latest[1,2]
      colnames(xreg)<-c(conxreg,paste0(str_replace(input$Fcore2var,"/",".")," - VALUE"))
    }
    if (input$Fcore3==TRUE){
      latest<-Quandl(input$Fcore3var,type='raw')
      conxreg<-colnames(xreg)
      simirf<-ann*100*(exp(simdf[simdf$varname=='I',1:ncol(simdf)-1][n,]/100)-1)
      xreg$Fore3<-t(1+simirf)*latest[1,2]
      colnames(xreg)<-c(conxreg,paste0(str_replace(input$Fcore3var,"/",".")," - VALUE"))
    }
    if (input$Fcore4==TRUE){
      latest<-Quandl(input$Fcore4var,type='raw')
      conxreg<-colnames(xreg)
      xreg$Fore4<-t(1+simdf[simdf$varname=='L',1:ncol(simdf)-1][n,])*latest[1,2]
      colnames(xreg)<-c(conxreg,paste0(str_replace(input$Fcore4var,"/",".")," - VALUE"))
    }
    if (input$Fcore5==TRUE){
      latest<-Quandl(input$Fcore5var,type='raw')
      conxreg<-colnames(xreg)
      simirf<-ann*100*(exp(simdf[simdf$varname=='pi',1:ncol(simdf)-1][n,]/100)-1)
      xreg$Fore5<-t(1+simirf)*latest[1,2]
      colnames(xreg)<-c(conxreg,paste0(str_replace(input$Fcore5var,"/",".")," - VALUE"))
    }
    if (input$Fcore6==TRUE){
      latest<-Quandl(input$Fcore6var,type='raw')
      conxreg<-colnames(xreg)
      xreg$Fore6<-t(1+simdf[simdf$varname=='R',1:ncol(simdf)-1][n,])*latest[1,2]
      colnames(xreg)<-c(conxreg,paste0(str_replace(input$Fcore6var,"/",".")," - VALUE"))
    }
    fit<-readRDS(paste0(str_replace(input$depvar,"/","."),"_fit.rds"), refhook = NULL)
    fcast <- forecast(fit, h=input$forecount, xreg=data.frame(xreg))
    plotdata<-as.data.frame(cbind(fcast$mean,fcast$upper,fcast$lower))
    plotdata$Period<-seq(1, as.numeric(input$forecount),1)
    plotdata<-reshape2::melt(plotdata, id.vars = "Period")
    h1<-hPlot(x = "Period", y = "value", group = "variable", data = plotdata)
    h1$xAxis(title = list(text = 'Time'))
    h1$yAxis(title = list(text = paste0('Value')))
    h1$title(text=paste0("Forecast plot for ", input$depvar))
    return(h1)
  })
  output$downloadmanual <- downloadHandler(
    filename = function() { paste0("Stress Testing Manual.pdf") },
    content = function(file) {
      file.copy('Stress Testing Manual.pdf', file)
    }
  )
  observeEvent(input$do, {
    do.call(file.remove,list(grep("*.csv",list.files(getwd()))))
    do.call(file.remove,list(grep("*.rds",list.files(getwd()))))
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Files Removed')
  })
  sw_gecon <- reactive({
    # set our starting values for various parameters
    isolate({
      initv <- list(z = 1, z_f = 1, Q = 1, Q_f = 1, pi = 1, pi_obj = 1,
                    epsilon_b = 1, epsilon_L = 1, epsilon_I = 1, epsilon_a = 1, epsilon_G = 1,
                    r_k = 0.01, r_k_f = 0.01)
      tmp <- initval_var(sw_gecon_orig, init_var = initv)
      initf <- list(
        beta = input$beta,            # Discount factor
        tau = input$tau,            # Capital depreciation rate
        varphi = input$varphi,         # Parameter of investment adjustment cost function
        psi = input$psi,            # Capacity utilisation cost parameter
        sigma_c = input$sigma_c,        # Coefficient of relative risk aversion
        h = input$h,              # Habit formation intensity
        sigma_l = 1 / input$sigma_l_inv,         # Reciprocal of labour elasticity w.r.t. wage
        omega = input$omega,               # Labour disutility parameter,
        alpha = input$alpha,
        gamma_w = input$gamma_w,
        lambda_w = input$lambda_w,
        xi_w = input$xi_w,
        gamma_p = input$gamma_p,
        xi_p = input$xi_p,
        r_pi = input$r_pi
      )
      tmp <- set_free_par(tmp, initf)
      # find the steady state for that set of starting values
      tmp <- steady_state(tmp)
      
      # solve the model in linearised form for 1st order perturbations/randomness
      tmp <- solve_pert(tmp, loglin = TRUE)
    })
    input$goButton
    return(tmp)
    
  })
  sw_gecon_shocked <- reactive({
    # set covariance matrix of the parameters to be used in shock simulation
    a <- c(eta_b = input$eta_b ^ 2, eta_L = input$eta_L ^ 2, eta_I = input$eta_I ^ 2, 
           eta_a = input$eta_a ^ 2,
           eta_w = input$eta_w ^ 2, eta_p = input$eta_p ^ 2,
           eta_G = input$eta_G ^ 2, eta_R = input$eta_R ^ 2, eta_pi = input$eta_pi ^ 2)
    tmp  <- set_shock_cov_mat(sw_gecon(), shock_matrix = diag(a), shock_order = names(a))
    
    # compute the moments with that covariance matrix
    tmp <- compute_moments(tmp,sim=TRUE, nrun=1000)
    return(tmp)
  })
  sw_gecon_irf <- reactive({
    shock_row <- which(shocks$longer_name == input$shock_var)
    compute_irf(sw_gecon_shocked(), var_list = c('C', 'Y', 'pi', 'I', 'L','R'), chol = T,
                shock_list = list(shocks[shock_row, "param"]), path_length = as.numeric(input$forecount))
  })
  output$chart5 <- renderChart2({
    sw_gecon_irf<-sw_gecon_irf()
    irf<-as.data.frame(sw_gecon_irf@sim)
    irf<-as.data.frame(t(irf))
    colnames(irf)<-sw_gecon_irf@var_list
    irf$period=seq(1,as.numeric(input$forecount),1)
    plotdata<-reshape2::melt(irf, id.vars = "period")
    h1<-hPlot(x = "period", y = "value", group = "variable", data = plotdata)
    h1$xAxis(title = list(text = 'Time'))
    h1$yAxis(title = list(text = paste0('Value')))
    h1$title(text=paste0("IRF Plot for ", input$shock_var))
    return(h1)
  })
  sw_sim <- eventReactive(input$Simulate, {
    varlist<-NULL
    if (input$core1==TRUE){
      varlist<-c(varlist,'Y')
    }
    if (input$core2==TRUE){
      varlist<-c(varlist,'C')
    }
    if (input$core3==TRUE){
      varlist<-c(varlist,'I')
    }
    if (input$core4==TRUE){
      varlist<-c(varlist,'L')
    }
    if (input$core5==TRUE){
      varlist<-c(varlist,'pi')
    }
    if (input$core6==TRUE){
      varlist<-c(varlist,'R')
    }
    simdf<-NULL
    withProgress(message = 'Running Simulation', value = 0,{
        for (i in 1:as.numeric(input$simcount))
        {
          shock_row <- which(shocks$longer_name == input$shock_var)
          randpath<-random_path(sw_gecon_shocked(), var_list = varlist,shock_list = list(shocks[shock_row, "param"]), path_length = as.numeric(input$forecount)) 
          currsim<-as.data.frame(get_simulation_results(randpath))
          currsim$varname<-rownames(currsim)
          rownames(currsim)<-NULL
          simdf<-rbind(simdf,currsim)
          # Increment the progress bar, and update the detail text.
          incProgress(1/as.numeric(input$simcount), detail = paste("completed part", i))
        }
    })
    write.csv(simdf,"simdf.csv",row.names = FALSE)
    return('Simulation complete')
  })
  output$nText <- renderText({
    sw_sim()
  })
  output$IVTABLE<-DT::renderDataTable({
    #get iv
    serieslist<-unlist(strsplit(input$foreseriessim,","))
    if (input$Fcore1==TRUE){
      serieslist<-c(serieslist,'FRED/GDP')
    }
    if (input$Fcore2==TRUE){
      serieslist<-c(serieslist,'FRED/PCECC96')
    }
    if (input$Fcore3==TRUE){
      serieslist<-c(serieslist,'FRED/RINV')
    }
    if (input$Fcore4==TRUE){
      serieslist<-c(serieslist,'FRED/LNU01000000')
    }
    if (input$Fcore5==TRUE){
      serieslist<-c(serieslist,'RATEINF/INFLATION_USA')
    }
    if (input$Fcore6==TRUE){
      serieslist<-c(serieslist,'PERTH/FEDF_M')
    }
    # Create chart
    if (length(serieslist)==1)
    {
      #get data
      eval(parse(text=paste0("data<-Quandl(code='",serieslist,"',type='raw',",ifelse(input$transformfore=="NULL","transform=NULL,",paste0("transform='",input$transformfore,"',")),ifelse(input$frequencyfore=="NULL","collapse=NULL,",paste0("collapse='",input$frequencyfore,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
    }else{
      for (count in 1:length(serieslist)){
        if (count==1){
          str<-paste0("'",serieslist[count],"',")
        }else if (count<length(serieslist)){
          str<-paste0(str,"'",serieslist[count],"',")
        }else{
          str<-paste0(str,"'",serieslist[count],"'")
        }
      }
      str<-paste0("c(",str,")")
      #get data
      eval(parse(text=paste0("data<-Quandl(code=",str,",type='raw',",ifelse(input$transformfore=="NULL","transform=NULL,",paste0("transform='",input$transformfore,"',")),ifelse(input$frequencyfore=="NULL","collapse=NULL,",paste0("collapse='",input$frequencyfore,"',")),"order='asc',meta=TRUE,force_irregular = TRUE)")))
    }
    return(data)
  })
  output$pdfviewer <- renderText({
    return(paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = ""))
  })

})

