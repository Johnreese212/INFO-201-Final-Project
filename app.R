# INFO 201 Final App
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("DT")

source("analysis_a6.R")

page_one <- tabPanel(
  "Overview"
)

page_two <- tabPanel(
  "Keywords"
)

page_three <- tabPanel(
  "Twitter Activity Analysis",
  titlePanel("Analysis of President Trump's Twitter Activity in 2017"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "month",
        label = "Month",
        min = 1,
        max = 12,
        value = 1,
        ticks = FALSE
      ),
      h2(textOutput("monthname"))
    ),
    mainPanel(
      plotOutput("plotmontlytweets"),
      h1(textOutput("twitterdata")),
      tableOutput("tablemonthlytweets")
    )
  )
)

page_four <- tabPanel(
  "Approval Rating and Tweeting Frequency"
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
  
  # Michelle Q data
  output$plotmontlytweets <- renderPlot({
    data <- twitter_data

    plot.data <- data %>% filter(month == input$month)
    
    ggplot(data = plot.data) +
      geom_point(mapping = aes(x = day, y = time, color = is_retweet)) + 
      scale_y_discrete(breaks = NULL) +
      labs(
        title = paste("All of Donald Trump's Tweets in", month.name[input$month]),
        y = "Time of Day (00:00 to 24:00)",
        x = "Day of the Month",
        color = "Is is a Retweet?"
      )
  })
  output$twitterdata <- renderText({
    twitter_info <- get_monthly_info(input$month)
    twitter_info
  })
  output$monthname <- renderText({
    month <- month.name[input$month]
    month
  })
  output$tablemonthlytweets <- renderTable({
    table <- monthly_tweets
    table
  })
}


shinyApp(ui = my_ui, server = my_server)


















