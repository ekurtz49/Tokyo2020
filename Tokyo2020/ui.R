#
# ELQ
#



#library(rsconnect)
library(shiny)
library(ggplot2)
library(rvest)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("Greater Kurtz Family Tokyo 2020 Standings", align="center"), windowTitle = "Greater Kurtz Family Tokyo 2020 Standings"),

  # Plot output
  fluidRow(
       plotOutput("mainBarChart", height="700px")
  ),
  
  # Show a button to check for new data
  fluidRow(
    div(actionButton("refreshData", "Refresh Data"),align="center")
    #textOutput("lastUpdate")
  )
))
