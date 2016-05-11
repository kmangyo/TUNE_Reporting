library(shiny)
library(rjson)
library(dplyr)
library(stringi)

shinyUI(fluidPage(
  titlePanel("TUNE Real-Time Reporting"),
  sidebarLayout(
    sidebarPanel( textInput("api", label = h3("API Key"), value = ""),
                  dateInput("dates", label = h3("Date range")), submitButton("Update View"),
                  textInput("job_id", label = h3("Job_ID"), value = ""), submitButton("Update View"),
                  textInput("dl_url", label = h3("DL_URL"), value = ""), submitButton("Update View")
                  ),

    mainPanel(
      h4("Job_ID"),
      verbatimTextOutput("job_id"),
      h4("DL_URL"),         
      verbatimTextOutput("dl_url"),
      h4("REPORT"),
      DT::dataTableOutput("report")
  )
  )
))
