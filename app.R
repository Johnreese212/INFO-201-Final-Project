# INFO 201 Final App
library("shiny")
library("plyr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("DT")

# Gives us access to the data and analysis from a6
source("analysis_a6.R")

page_one <- tabPanel(
  "Overview",
  fluidRow(
    column(12,
      column(6,
        titlePanel(h1("Trump's tweets and his approval rating during 2017", style = "color: #800000;")),
        p("by Roshni Sinha, Michelle Ponting, Andy Straavaldson and John Reese -- Group AA2")),
      column(6, 
        imageOutput("trumpImage")
      )
    ),
    column(12,
           p("Donald Trump often takes to Twitter to share his thoughts with the American public. As president of the United States, his duties and the perception of the American public are often intertwined. Twitter, a social media platform used to share short posts, acts as a means of communication between Trump and both his supporters and rivals, allowing others an insight to his mind and life in the Oval Office. In this assignment, we will analyze the effect Trump's tweets had on his approval rating in 2017. By comparing these two sets of data, we can gain broader knowledge of how social media is used in politics.")
    )
  )
)

page_two <- tabPanel(
 "Tweet interaction based on key words",
  titlePanel("How using certain key words affect the amount of interactions a tweet gets"),
  p("This will show which topics bring in the most likes and retweets among twitter users, and 
    will show which topics spark the most conversation and activity on twitter and what topics people
    care the most about")
)

page_three <- tabPanel(
  "Twitter Activity Analysis",
  titlePanel("Analysis of President Trump's Twitter Activity in 2017"),
  fluidRow(
    column(12, 
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
          h2(textOutput("monthname")),
          p(textOutput("twitterdata")),
          br(),
          p("In 2017, he posted 2,602 tweets total, including retweets, and 2,417, that were not retweets.")
        ),
        mainPanel(
          plotOutput("plotmontlytweets")
        )
      )
    ),
    column(12, 
           tableOutput("tablemonthlytweets")
    )
  )
  
)

page_four <- tabPanel(
  "Approval Rating and Tweeting Frequency", # Label for the tab in the navbar
  titlePanel("How did changes in Trump's approval/disapproval rating affect how often he tweeted?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "approve", label = "Metric",choices = c("approve","disapprove")),
      textOutput(outputId = "corr"),
      p(""),
      p("As can be seen, there exists a weak negative correlation between both Trump's approval rating and tweeting frequency and a weak positive correlation between Trump's disapproval rating and his tweeting frequency. Interestingly, the former correlation is stronger than the latter. Perhaps this indicates that Trump pays more attention to his approval ratings than his dissaproval ratings.")
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
  "Approval Rating and Tweet Content",
  titlePanel("Tracking Trump's Tweets"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput(
        inputId = "query",
        label = "Trump mentioned:"
      ),
    p("This page allows the user to search any term Trump mentions and view his approval rating at the time he said it. Only approval rating from Gallup are plotted to allow for consistency. Trump's average approval rating was 38.5%, denoted by the black horizontal line. Some common terms mentioned by Trump are 'Russia' and 'Democrats'. As a general trend, Trump's approval rating seems to decline slightly after tweeting about Russia.")
    ),
    
    mainPanel(
      plotOutput("plot_query"),
      tableOutput("searchquery")
    )
    
  )
)

my_ui <- navbarPage(
  "Tweets of Approval",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five,
  fluid = TRUE
)

my_server <- function(input,output) {
  # Overview
  
  output$trumpImage <- renderImage({
    list(src = "donald_trump.jpg",
         contentType = 'image/png',
         alt = "Donald Trump Image",
         width = "400", 
         height = "auto")
  }, deleteFile = FALSE)
  
  # John's Q
  
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
  
  # Michelle Q data
  output$plotmontlytweets <- renderPlot({
    data <- twitter_data

    plot.data <- data %>% filter(month == input$month)
    
    ggplot(data = plot.data) +
      geom_point(mapping = aes(x = day, y = time, color = is_retweet)) + 
      scale_y_discrete(breaks = c("00:00:00", "24:00:00")) +
      labs(
        title = paste("All of Donald Trump's Tweets in", month.name[input$month]),
        y = "Time of Day",
        x = "Day of the Month",
        color = "Is it a Retweet?"
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
    table <- monthly_table
    table
  })
  
  #Roshni's data
  
  output$searchquery <- renderTable({
    
    if (input$query == input$query) {
      plot.data <- search_query(input$query) 
    } else {
      plot.data <- trump_tweets_date
    }
  })
  
  output$plot_query<- renderPlot({
    if (input$query == input$query) {
      plot.data <- trump_approval_filtered 
    }
    
    ggplot(trump_approval_filtered, aes(x = enddate, y = approve)) + geom_line(group = 1, color = "blue") + geom_hline(yintercept = 38.5, color = "black") + geom_vline(xintercept = plotquery(input$query), color = "red")
    
  })
}

shinyApp(ui = my_ui, server = my_server)








