suppressPackageStartupMessages(c(
  library(shiny),
  library(stringi),
  library(strip),
  library(dplyr),
  library(data.table),
  library(tidyr),
  library(tidytext),
  library(rsconnect)
))
if(!exists("ngramhelper", mode="function")) source("./ngramhelper.R", encoding = 'UTF-8')
#source("./ngramhelper.R", encoding = 'UTF-8')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Swiftkey Word Prediction using Katz Back Off"),
  
  h5("Make sure to clear the older text for the next sentence to input"),
  
  h5("Note: Please wait for the Output to settle down and print the next word , It is Slow and not very efficient !!"),
   
  textInput("text", label = h3("Text input"), value = ""),
         
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$value <- renderPrint({
   #  unlist(strsplit(input$text, " ")[[1]])
   #   })
  
    WordPrediction <- reactive(
      {
        text <- input$text
        Count <- length(text)
        lasttwo <- tail(strsplit(text, split = " ")[[1]],2)
        lasttwonew <- paste(lasttwo[1],lasttwo[2], sep = " ")
        
        # Lets put the code here
        predictedword<-ngramhelper(text = lasttwonew)
        print(predictedword)
        
      }
    )
    output$value <- renderPrint( {
      cat("next word is", WordPrediction())
      })
    #output$enteredWords <- renderText({ input$text }, quoted = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

