library(shiny)
library(rjson)
library(dplyr)
library(stringi)

shinyUI(fluidPage(
  titlePanel("TUNE Reporting"),
  sidebarLayout(
    sidebarPanel( textInput("api", label = h3("API Key"), value = ""),
                  dateRangeInput("dates", label = h3("Date range")),submitButton("Update View"),
                  textInput("dl_url", label = h3("DL_URL"), value = ""), submitButton("Update View"),
                  textInput("dl_url_puv", label = h3("DL_URL_PUV"), value = ""), submitButton("Update View")
                  ),

    mainPanel(
      h4("DL_URL"),         
      verbatimTextOutput("dl_url"),
      h4("DL_URL_PUV"),         
      verbatimTextOutput("dl_url_puv"),
      h4("REPORT"),
      DT::dataTableOutput("report"), 
      h4("REPORT_PUV"),
      DT::dataTableOutput("report_puv")
  )
  )
))
