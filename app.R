#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fontawesome)
library(tidyverse)
library(tidytext)
library(ggtext)
library(here)

source("app-function.R", local = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = "bootstrap.css",
    
    fluidRow(
        column(width = 12, 
               tags$div(align = "right", 
                        a(href = "https://github.com/sschmutz/machine-learning-project-2020/blob/main/presentation/classifying_headlines.pdf", 
                          h4(fa("github", fill = "#B9BABD"))
                        )
               )
        )
    ),
    
    tags$div(align = "center", titlePanel("Classify your News Headline")),
    
    
    br(),
    
    fluidRow(
        column(width = 12,
               tags$div(align = "center", textAreaInput("headline", "", "Enter a Headline"))
        ),
        
        column(width = 12,
               tags$div(align = "center", actionButton("submit", "Predict"))
        )
    ),
    
    br(),
    
    fluidRow(
        plotOutput("class_cond_prob")
    ),
    
    br(),
    
    fluidRow(
        column(width = 12,
               uiOutput("predicted_class"), align = "center")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    text_output <- eventReactive(input$submit, {
        class_disp <- news_classifier(input$headline)
        paste0("The predicted source of the headline is <b>", class_disp, "</b>")
    })
    
    output$predicted_class <- renderUI({
        HTML(text_output())
    })
    
    plot_output <- eventReactive(input$submit, {
        class_conditional_probability_plot(input$headline)
    })
    
    output$class_cond_prob <- renderPlot({
        
        input$submit
        
        # Use isolate() to avoid dependency on input$n
        isolate({
            #class_conditional_probability_plot(input$headline)
            plot_output()
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
