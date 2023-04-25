# 2023_0425
# This is a Shiny web application that is used to predict the next word.
# Created By: John Nguyen
#


library(shiny)
library(stringr)
library(ngram)
library(dplyr)
library(tidytext)
library(readr) # read_delim
library(tidyr)
library(tm)

# read in other functions to use
source("CapstoneFunction.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predicting a Word"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h5("Enter a word into the box on Main Panel:"),
            h5("Note: If the enter word is not found, then it is not in the database"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textInput("input_user", label = "Write Something"),
            verbatimTextOutput("output_user")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$output_user<-renderText({
        data<-clean_up_phrase(input$input_user);
        answer<-phrase(data)
        
        return(answer)
    })

    
}

# Run the application  
shinyApp(ui = ui, server = server)














