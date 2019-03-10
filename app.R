# INFO 201 Final App
library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")
library("maps")
library("DT")

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
  
}


shinyApp(ui = my_ui, server = my_server)






















