library(text2vec)
library(shiny)
# library(glmnet)

##########Load Vocabulary##############
f= file("myVocab.txt")
vocab = readLines(f)
close(f)
vectorizer = vocab_vectorizer(create_vocabulary(vocab, ngram = c(1L, 4L)))

# ##########Build Model##################
# all = read.table("data.tsv",stringsAsFactors = F,header = T)
# all$new_id = as.integer(all$new_id)
# all$sentiment = as.integer(all$sentiment)
# all$review = gsub('<.*?>', ' ', all$review)  #Remove HTML tag
# all$review = gsub('[^[:alnum:]]', ' ', all$review) #Remove punctuation
# 
# print("Loading data...")
# it.all = itoken(all$review,
#                 preprocessor = tolower,
#                 tokenizer = word_tokenizer, #tokenize_word_stems,
#                 ids = all$id,
#                 progressbar = TRUE)
# 
# 
# dtm.all = create_dtm(it.all, vectorizer)
# 
# print("Building model...")
# NFOLDS = 5
# my.cv = cv.glmnet(x = dtm.all, y = all$sentiment,
#                   family = 'binomial',
#                   alpha = 0,
#                   type.measure = "auc",
#                   nfolds = NFOLDS)
# 
# my.fit = glmnet(x = dtm.all, y = all$sentiment,
#                 lambda = my.cv$lambda.min, family='binomial', alpha = 0)
# 
# save(my.fit, file="model.dat")
# cat("Training AUC:", glmnet:::auc(all$sentiment, predict(my.fit, dtm.all, type = 'response')[,1]), "\n")

#Load pre-built model 
load(file="model.dat")

#Nomalize
betas = coef(my.fit)[-1,1]
pos_min = min(betas[betas >= 0])
pos_max = max(betas[betas >= 0])
betas[betas >= 0] = round((betas[betas >= 0] - pos_min) / (pos_max - pos_min) * 10) + 11
neg_min = min(betas[betas < 0])
neg_max = max(betas[betas < 0])
betas[betas < 0] = round((betas[betas < 0] - neg_max) / (neg_min - neg_max) * 9) + 1

#Define style for sentiment words
SentCSSClass = c(
  "word1", "word2", "word3", "word4", "word5",
  "word6","word7", "word8", "word9", "word10",
  "word11","word12","word13", "word14", "word15",
  "word16", "word17","word18","word19", "word20",
  "word21"
)

SentWord = cbind(word = gsub("_", " ", names(betas)), class=SentCSSClass[betas])

#############################################
htmlmark_sentiment <- function(review.text, sent.word.id){
  for (id in sent.word.id){
    #print(SentWord[id, 1])
    review.text = gsub(SentWord[id, 1], tags$div(class = SentWord[id, 2], SentWord[id, 1]), 
                       review.text)
  }
  
  return (review.text)
}

predict_sentiment <- function(review.text){
  review.text = gsub('<.*?>', ' ', review.text)  #Remove HTML tag
  #review.text = gsub('[^[:alnum:]]', ' ', review.text) #Remove punctuation
  
  it_review = itoken(review.text, 
                     preprocessor = tolower,
                     tokenizer = word_tokenizer, 
                     progressbar = FALSE)
  dtm = create_dtm(it_review, vectorizer)
  
  sent.html = htmlmark_sentiment(review.text, which(dtm[1,] != 0))
  p = predict(my.fit, dtm, type = 'response')[,1][1]
  
  return (list(Positive = p > 0.5, mark.text = sent.html))
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
  htmlOutput("id_marked_review")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  review.text <- eventReactive(input$id_predict, input$id_review)
  observeEvent(input$id_predict, {
    pred = predict_sentiment(review.text())
    if (pred$Positive){
      output$id_sentiment <- renderUI({tags$div("Positive", class = "positive")})
    } else {
      output$id_sentiment <- renderUI({tags$div("Negative", class = "negative")})
    }
    
    output$id_marked_review <- renderText({pred$mark.text})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

