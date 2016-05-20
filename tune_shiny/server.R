#https://kmangyo.shinyapps.io/tune-reporting/

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
    export_url <- paste0('http://api.mobileapptracking.com/v2/advertiser/stats/export.json?session_token=',session_token,'&fields=timestamp%2Cpublisher_id%2Cpublisher.name%2Csite_event_id%2Csite_event.name%2Cadvertiser_sub_campaign_id%2Cadvertiser_sub_campaign.name%2Cadvertiser_sub_adgroup_id%2Cadvertiser_sub_adgroup.name%2Cadvertiser_sub_ad_id%2Cadvertiser_sub_ad.name%2Cad_clicks%2Cinstalls%2Copens%2Cevents%2Crevenues%2Ccurrency_code&sort%5Btimestamp%5D=desc&filter=(((debug_mode%3D0+OR+debug_mode+is+NULL)+AND+(test_profile_id%3D0+OR+test_profile_id+IS+NULL)))&group=timestamp%2Cpublisher_id%2Csite_event_id%2Cadvertiser_sub_campaign_id%2Cadvertiser_sub_adgroup_id%2Cadvertiser_sub_ad_id%2Ccurrency_code&timestamp=date&start_date=',input$dates[1],'&end_date=',input$dates[2],'&response_timezone=Asia%2FSeoul&limit=1000&format=csv')
    export_url_json <- fromJSON(file=export_url)
    job_id <- export_url_json$data$job_id
      }
    })

  job_id_puv <- reactive({
    api <- input$api
    if(api==c('')){
      job_id_puv <- c('NA')
    } else {
    csv_url_puv<-paste0("http://api.mobileapptracking.com/v3/logs/advertisers/22012/exports/events?start_date=",input$dates[1],"T00:00:00%2B09:00&end_date=",input$dates[2],"T23:59:59%2B09:00&fields=created,site.name,publisher.name,site_event.name,advertiser_sub_ad.name,user_id&timezone=Asia/Seoul&api_key=",input$api,"&limit=100000&filter=(publisher.name%20!=%20%27%27)")
    job_id_puv <- fromJSON(file=csv_url_puv)
    job_id_puv$export_job_status_url
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
            if(dl_url$data$status == c('complete')){
            dl_url <- dl_url$data$data$url
            return(dl_url)
          }
        }
      }
    }
  })

  dl_url_puv <- reactive({
    job_id_puv <- input$job_id_puv    
    if(job_id_puv==c('')){
      dl_url_puv <- c('NA')
    } else {
    dl_url_puv <- fromJSON(file=job_id_puv)
      if(dl_url_puv$status == c('complete')) {
      return(dl_url_puv)
      } else {
          while(dl_url_puv$status != c('complete')){
          dl_url_puv<-fromJSON(file=dl_url_puv$export_job_status_url)
            if(dl_url_puv$status == c('complete')){
            dl_url_puv <- dl_url_puv$url
            return(dl_url_puv)
          }
        }
      }
    }
  })

  output$job_id <- renderText({ 
    job_id()
  })

  output$job_id_puv <- renderText({ 
    job_id_puv()
  }) 

  output$dl_url <- renderPrint({ 
    dl_url <- dl_url()
    dl_url
  })

  output$dl_url_puv <- renderPrint({ 
    dl_url_puv <- dl_url_puv()
    dl_url_puv
  })  
  
  output$report <- DT::renderDataTable(DT::datatable({
    if(input$dl_url==c('')){
      report<-data.frame()
    } else {
    report <- read.csv(input$dl_url)
    report$site_event.name <- toupper(report$site_event.name)
    report <- report %>% group_by(timestamp, publisher.name, advertiser_sub_ad.name, site_event.name) %>% summarise(ad_click=sum(ad_clicks),installs=sum(installs), revenues=sum(revenues),events=sum(events) ,opens=sum(opens))
    report <- data.frame(report)
    report <- reshape(report, idvar=c("timestamp","publisher.name","advertiser_sub_ad.name"), timevar="site_event.name", direction="wide")
    report[4:ncol(report)][is.na(report[4:ncol(report)])] <- 0
    report <- report[c('timestamp','publisher.name','advertiser_sub_ad.name','ad_click.','installs.INSTALL','revenues.PURCHASE','events.PURCHASE','events.REGISTRATION','opens.OPEN')]
    report <- subset(report, publisher.name!=c('organic'))
    report$CVR.install<-with(report, (installs.INSTALL/ad_click.))
    report$CVR.install[is.na(report$CVR.install)]<-0
    report$CVR.install[is.infinite(report$CVR.install)]<-0
    report$CVR.install<-percent(report$CVR.install)  
    names(report)<-c('Timestamp','Publisher.name','Advertiser_sub_ad.name','Ad_click','Install','Revenues','Purchase','Registration','Open','CVR.install')
    report<-report[c(1:5,10,6:9)]
    report
    }
  }))

  output$report_puv <- DT::renderDataTable(DT::datatable({
    if(input$dl_url_puv==c('')){
      report_puv <- data.frame()
    } else {
    report_puv <- read.csv(input$dl_url_puv)
    report_puv$site_event.name <- toupper(report_puv$site_event.name)
    report_puv <- subset(report_puv, site_event.name==c('PURCHASE') )
    report_puv$created <- stri_sub(report_puv$created,1,10)
    report_puv <- report_puv [!duplicated(report_puv [c('created','publisher.name','user_id','advertiser_sub_ad.name')]),]
    report_puv <- report_puv %>% group_by(created,publisher.name, advertiser_sub_ad.name) %>% summarise(count=n())
    report_puv
    }
  }))

})
