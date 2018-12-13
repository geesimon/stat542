library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "vissent.css",
   titlePanel("IMDB Movie Review Sentiment Analysis"),
   textAreaInput("text", "Plesae input your text:"),
   actionButton(inputId = "predict", label = "Evaluate"),
   plotOutput("distPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   observeEvent(input$predict, {print(input$clicks)})
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # 
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

