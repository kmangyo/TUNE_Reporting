library(shiny)
library(rjson)
library(dplyr)
library(stringi)

shinyUI(fluidPage(
  titlePanel("TUNE Reporting"),
  sidebarLayout(
    sidebarPanel( textInput("api", label = h4("API Key"), value = ""),
                  dateRangeInput("dates", label = h4("Date range")),submitButton("Update View"),
                  textInput("dl_url", label = h4("DL URL"), value = ""), 
                  textInput("dl_url_puv", label = h4("DL URL PUV"), value = ""), submitButton("Update View"),
                  selectInput('file1', label=h4('Publisher.names'),'', multiple=TRUE, selectize=TRUE),
                  selectInput('file2', label=h4('Ad.name'),'', multiple=TRUE, selectize=TRUE), submitButton("Update View")
                  ),

    mainPanel(
      h4("DL URL"),         
      verbatimTextOutput("dl_url"),
      h4("DL URL PUV"),         
      verbatimTextOutput("dl_url_puv"),
      h4("Selected REPORT"),
      DT::dataTableOutput("report"), 
      h4("REPORT All & Off"),
      DT::dataTableOutput("report_off"), 
      h4("Selected PUV REPORT "),
      DT::dataTableOutput("report_puv"),
      h4("REPORT PUV All & Off"),
      DT::dataTableOutput("report_puv_off")
  )
  )
))
