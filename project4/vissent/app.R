library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "vissent.css",
  titlePanel("IMDB Movie Review Sentiment Analysis"),
  textAreaInput("review_content", "Please input the movie review:", width = '400px', height = '200px'),
  actionButton(inputId = "predict", label = "Evaluate"),
  htmlOutput("marked_review")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  review_text <- eventReactive(input$predict, input$review_content)
  observeEvent(input$predict, {
      output$marked_review <- renderUI(tags$h1(review_text()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

