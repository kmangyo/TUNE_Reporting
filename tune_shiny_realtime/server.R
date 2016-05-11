library(shiny)
library(rjson)
library(dplyr)
library(stringi)
library(scales)

shinyServer(function(input, output, session) {
  
  job_id <-reactive({
    api <- input$api
    if(api==c('')){
      job_id <- c('NA')
    } else {
    session_t <- paste0('http://api.mobileapptracking.com/v2/session/authenticate/api_key.json?&api_keys=',api)
    token <- fromJSON(file=session_t)
    session_token <- token$data
    start_date <- input$dates[1]
    end_date <- input$dates[1]
    export_url <- paste0('http://api.mobileapptracking.com/v2/advertiser/stats/export.json?session_token=',session_token,'&fields=timestamp%2Cpublisher_id%2Cpublisher.name%2Csite_event_id%2Csite_event.name%2Cadvertiser_sub_campaign_id%2Cadvertiser_sub_campaign.name%2Cadvertiser_sub_adgroup_id%2Cadvertiser_sub_adgroup.name%2Cadvertiser_sub_ad_id%2Cadvertiser_sub_ad.name%2Cad_clicks%2Cinstalls%2Copens%2Cevents%2Crevenues%2Ccurrency_code&sort%5Btimestamp%5D=desc&filter=(((debug_mode%3D0+OR+debug_mode+is+NULL)+AND+(test_profile_id%3D0+OR+test_profile_id+IS+NULL)))&group=timestamp%2Cpublisher_id%2Csite_event_id%2Cadvertiser_sub_campaign_id%2Cadvertiser_sub_adgroup_id%2Cadvertiser_sub_ad_id%2Ccurrency_code&timestamp=hour&start_date=',start_date,'&end_date=',end_date,'&response_timezone=Asia%2FSeoul&limit=1062&format=csv')
    export_url_json <- fromJSON(file=export_url)
    job_id <- export_url_json$data$job_id
      }
    })


  dl_url <- reactive({    
    if(input$job_id==c('')){
      dl_url <- c('NA')
    } else {
    csv_url <- paste0('http://api.mobileapptracking.com/v2/export/download.json?job_id=',input$job_id,'&api_key=',input$api)
    dl_url <- fromJSON(file=csv_url)
      if(dl_url$data$status == c('complete')) {
      return(dl_url)
      } else {
          while(dl_url$data$status != c('complete')){
          dl_url<-fromJSON(file=csv_url)
          print(dl_url$data$percent_complete)
          if(dl_url$data$status == c('complete')){
          return(dl_url)
        }
    }  
#         while(is.null(dl_url$data)){
#         dl_url<-fromJSON(file=csv_url)
#         print(dl_url$erros)
#            if(!is.null(dl_url$data)){
#            dl_url <- dl_url$data$data$url
#            return(dl_url)
#          }
#        }  
      }
    }
  })

  output$job_id <- renderText({ 
    job_id()
  })

  output$dl_url <- renderPrint({ 
    dl_url <- dl_url()
    dl_url
  })

  
  output$report <- DT::renderDataTable(DT::datatable({
    if(input$dl_url==c('')){
      report<-data.frame()
    } else {
    report<-read.csv(input$dl_url)
    report$site_event.name<-toupper(report$site_event.name)
    report<-report %>% group_by(timestamp, publisher.name, advertiser_sub_ad.name, site_event.name) %>% summarise(ad_click=sum(ad_clicks),installs=sum(installs), revenues=sum(revenues),events=sum(events) ,opens=sum(opens))
    report<-data.frame(report)
    report<-reshape(report, idvar=c("timestamp","publisher.name","advertiser_sub_ad.name"), timevar="site_event.name", direction="wide")
    report[4:ncol(report)][is.na(report[4:ncol(report)])]<-0
    report<-report[c('timestamp','publisher.name','advertiser_sub_ad.name','ad_click.','installs.INSTALL','revenues.PURCHASE','events.PURCHASE','events.REGISTRATION','opens.OPEN')]
    report <- subset(report, publisher.name!=c('organic'))
    
    report$CVR.install<-percent(with(report, (installs.INSTALL/ad_click.)))
    names(report)<-c('Timestamp','Publisher.name','Advertiser_sub_ad.name','Ad_click','Install','Revenues','Purchase','Registration','Open','CVR.install')
    report<-report[c(1:5,10,6:9)]

    report$id<-with(report,paste0(Publisher.name,c('_'),Advertiser_sub_ad.name))
    report<- report %>% arrange(id, Timestamp)
    report$cumsum_ad_click<-with(report, ave(Ad_click,id,FUN=cumsum))
    report$cumsum_install<-with(report, ave(Install,id,FUN=cumsum))
    report$cumsum.CVR.install<-percent(with(report, (cumsum_install/cumsum_ad_click)))
    report$cumsum_revenues<-with(report, ave(Revenues,id,FUN=cumsum))
    report$cumsum_PURCHASE<-with(report, ave(Purchase,id,FUN=cumsum))
    report$cumsum_REGISTRATION<-with(report, ave(Registration,id,FUN=cumsum))
    report$cumsum_OPEN<-with(report, ave(Open,id,FUN=cumsum))
    report
    }
  }))

})

