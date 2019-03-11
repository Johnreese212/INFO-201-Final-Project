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
  "Tweet interaction based on key words",
  titlePanel("How using certain key words affect the amount of interactions a tweet gets")

)

page_three <- tabPanel(
  "Twitter Activity Analysis"
)

page_four <- tabPanel(
  "Approval Rating and Tweeting Frequency", # Label for the tab in the navbar
  titlePanel("How did changes in Trump's approval/disapproval rating affect how often he tweeted?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "approve", label = "Metric",choices = c("approve","disapprove")),
      textOutput(outputId = "corr")
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
    ggplot(data = daily_approval_and_frequency,mapping = aes_string(x = "Frequency", y = input$approve)) +
      geom_point() +
      geom_smooth(method = "lm")
  })
  
  output$date_plot <- renderPlot({
    apr <- toString(input$approve)
    ggplot(data = daily_approval_and_frequency)+
      geom_point(mapping = aes_string(x = "Date", y = apr, color = "Frequency")) 
  })
  
  output$corr <- renderText({
    tex <- ""
    if (input$approve == "approve") {
      x <- as.numeric(daily_approval_and_frequency$approve)
      y <- as.numeric(daily_approval_and_frequency$Frequency)
      freq_cor <- round(cor(x,y),2)
      tex <- paste("The correlation between approval rating and frequency is", freq_cor)
    } else {
      x <- as.numeric(daily_approval_and_frequency$disapprove)
      y <- as.numeric(daily_approval_and_frequency$Frequency)
      freq_cor <- round(cor(x,y),2)
      tex <- paste("The correlation between disapproval rating and frequency is", freq_cor)
    }
    tex
  })
}


shinyApp(ui = my_ui, server = my_server)






















