#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
    # Application title
    titlePanel("KeyWord Analyser App"),
    
    # Sidebar with a slider input for number of bins
    
        sidebarLayout(
            sidebarPanel(
                fileInput("corpus", "Upload the corpus file"),
                textInput("keywords", label = h3("Keywords")),
                h5("Eg of keywords: engine,mileage,comfort,performance,interior,maintenance,price")
            ), # End of sidebarPanel
            
            # Show a plot of the generated distribution
            mainPanel(tabsetPanel(type = "tabs",
                                  tabPanel("Keyword Filter", 
                                           tableOutput('keywordfiltered')),
                                  tabPanel("Relative Frequencies", 
                                           plotOutput('relativefreq')),
                                  tabPanel("Word Cloud", 
                                           plotOutput('wordcloudplot'))
        ) # End of tabsetPanel
    ) # End of mainPanel
    ) # End of sidebarLayout
) # End of fluidPage
) # End of shinyUI


