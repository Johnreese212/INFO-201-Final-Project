# INFO 201 Final App
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("DT")

# Gives us access to the data and analysis from a6
source("analysis_a6.R")

page_one <- tabPanel(
  "Overview"
)

page_two <- tabPanel(
  "Keywords"
)

page_three <- tabPanel(
  "Twitter Activity Analysis"
)

page_four <- tabPanel(
  "Approval Rating and Tweeting Frequency", # Label for the tab in the navbar
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "approve", label = "Metric",choices = c("approve","disapprove"))
    ),
    mainPanel(
      plotOutput(outputId = "frequency_plot"),
      plotOutput(outputId = "date_plot")
    )
  )
  
  
  
  
)

page_five <- tabPanel(
  "Russia"
)

my_ui <- navbarPage(
  "My app",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)


my_server <- function(input,output) {
  output$frequency_plot <- renderPlot({
    ggplot(data = daily_approval_and_frequency)+
      geom_point(mapping = aes_string(x = "Frequency", y = input$approve)) +
      geom_smooth(mapping = aes_string(x = "Frequency", y = input$approve))
  })
  
  output$date_plot <- renderPlot({
    apr <- toString(input$approve)
    ggplot(data = daily_approval_and_frequency)+
      geom_point(mapping = aes_string(x = "Date", y = apr, color = "Frequency")) 
  })
}


shinyApp(ui = my_ui, server = my_server)






















