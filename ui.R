#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(markdown)
library(dplyr)
library(tm)

shinyUI(fluidPage(
    titlePanel(" Predict"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("text",h6("type in a word or phrase and press submit button"), value = ""),
            submitButton("submit")
        ),
        
        mainPanel(
            h3('are you ready for this.....and, the next word is:'),
            verbatimTextOutput("prediction"),
            
            h4('If no other words are derived, by default the word "the", the 
                most common word used in english will be displayed.  Due to 
               limitations, all words cannot be accounted for and presented as 
               the next word.')
        )
    )
))