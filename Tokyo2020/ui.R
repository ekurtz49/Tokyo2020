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
  titlePanel("Greater Kurtz Family Tokyo 2020 Standings"),

    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
))
