
library(text2vec)
library(shiny)
library(glmnet)


##########Load Vocabulary##############
f= file("myVocab.txt")
vocab = readLines(f)
close(f)
vectorizer = vocab_vectorizer(create_vocabulary(vocab, ngram = c(1L, 4L)))

##########Build Model##################
all = read.table("data.tsv",stringsAsFactors = F,header = T)
all$new_id = as.integer(all$new_id)
all$sentiment = as.integer(all$sentiment)
all$review = gsub('<.*?>', ' ', all$review)  #Remove HTML tag
all$review = gsub('[^[:alnum:]]', ' ', all$review) #Remove punctuation

it.all = itoken(all$review,
                preprocessor = tolower,
                tokenizer = word_tokenizer, #tokenize_word_stems,
                ids = all$id,
                progressbar = TRUE)
dtm.all = create_dtm(it.all, vectorizer)

NFOLDS = 5
my.cv = cv.glmnet(x = dtm.all, y = all$sentiment,
                  family = 'binomial',
                  alpha = 0,
                  type.measure = "auc",
                  nfolds = NFOLDS)

my.fit = glmnet(x = dtm.all, y = all$sentiment,
                lambda = my.cv$lambda.min, family='binomial', alpha = 0)

#############################################
predict_sentiment <- function(review_text){
  review_text = gsub('<.*?>', ' ', review_text)  #Remove HTML tag
  review_text = gsub('[^[:alnum:]]', ' ', review_text) #Remove punctuation
  
  it_review = itoken(review_text, 
                     preprocessor = tolower,
                     tokenizer = word_tokenizer, 
                     progressbar = FALSE)
  dtm = create_dtm(it_review, vectorizer)
  
  sent.text = names(dtm[1,][dtm[1,] != 0])
  p = predict(my.fit, dtm, type = 'response')[,1][1]
  
  return (list(Positive = p > 0.5, mark.text = sent.text[1]))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "vissent.css",
  titlePanel("IMDB Movie Review Sentiment Analysis"),
  textAreaInput("id_review", "Please input the movie review:", width = '600px', height = '300px'),
  fluidRow(
    column(3, actionButton(inputId = "id_predict", label = "Evaluate")),
    column(3, htmlOutput("id_sentiment"))
  ),
  htmlOutput("marked_review")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  review.text <- eventReactive(input$id_predict, input$id_review)
  observeEvent(input$id_predict, {
    pred = predict_sentiment(review.text())
    if (pred$Positive){
      output$id_sentiment <- renderUI({tags$h4("Positive", style="color:green")})
    } else {
      output$id_sentiment <- renderUI({tags$h4("Negative", style="color:red")})
    }
    
    output$marked_review <- renderUI({tags$div(pred$mark.text)})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

