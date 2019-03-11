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
  "Overview",
  titlePanel("Trump's tweets and his approval rating during 2017"),
  p("by Roshni Sinha, Michelle Ponting, Andy Straavaldson and John Reese -- Group AA2"),
  p("Donald Trump often takes to Twitter to share his thoughts with the American public. 
    As president of the United States, his duties and the perception of the American public are often 
    intertwined. Twitter, a social media platform used to share short posts, acts as a means of communication 
    between Trump and both his supporters and rivals, allowing others an insight to his mind and life in the 
    Oval Office. In this assignment, we will analyze the effect Trump's tweets had on his approval rating 
    in 2017. By comparing these two sets of data, we can gain broader knowledge of how social media is 
    used in politics.")
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
      textOutput(outputId = "corr"),
      p(""),
      p("As can be soon, there exists a weak negative correlation between both Trump's 
      approval rating and tweeting frequency and a weak positive correlation between Trump's
      disapproval rating and his tweeting frequency. Interestingly, the former correlation is stronger
      than the latter. Perhaps this indicates that Trump pays more attention to his approval ratings than
      his dissaproval ratings.")
    ),
    mainPanel(
      plotOutput(outputId = "frequency_plot"),
      plotOutput(outputId = "date_plot"),
      p(""),
      p("- The frequency is the number of times that Donald Trump tweeted in a day"),
      p("- The approval rating is average of all approval ratings released in a day.")
    )
  )
)

page_five <- tabPanel(
  "Russia"
)

my_ui <- navbarPage(
  "Tweets of Approval",
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
      tex <- paste("The correlation between approval rating and frequency is", paste0(freq_cor, "."))
    } else {
      x <- as.numeric(daily_approval_and_frequency$disapprove)
      y <- as.numeric(daily_approval_and_frequency$Frequency)
      freq_cor <- round(cor(x,y),2)
      tex <- paste("The correlation between disapproval rating and frequency is", paste0(freq_cor, "."))
    }
    tex
  })
}


shinyApp(ui = my_ui, server = my_server)






















