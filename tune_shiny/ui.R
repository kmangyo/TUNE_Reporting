library(shiny)
library(rjson)
library(dplyr)

shinyUI(fluidPage(
  titlePanel("TUNE Reporting"),
  sidebarLayout(
    sidebarPanel( textInput("api", label = h3("API Key"), value = ""),
                  dateRangeInput("dates", label = h3("Date range")), submitButton("Update View"),
                  textInput("job_id", label = h3("Job_ID"), value = ""), submitButton("Update View"),
                  textInput("dl_url", label = h3("DL_URL"), value = ""), submitButton("Update View")
                  ),

    mainPanel(
      h4("Job_id"),
      verbatimTextOutput("job_id"),      
      h4("DL_url"),         
      verbatimTextOutput("dl_url"),
      h4("Report"),
      DT::dataTableOutput("report") 
  )
  )
))
