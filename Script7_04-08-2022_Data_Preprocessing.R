library("shiny")
library("tidyverse")
library("rvest")
library("tuber")
library("tidytext")
library("dplyr")
library("textdata")
library("ggplot2")
library("tm")
library("topicmodels")

yt_oauth("754004588089-j155ksbuqlnrkgnvrd5l31kh5ugf6qob.apps.googleusercontent.com", "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", token="")

#https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
#to extract pecific pattern after a pattern in r
ui <- fluidPage(textInput(inputId = "title",
                          label = "Enter the url",
                          value = "https://www.youtube.com/watch?v=YF1eYbfbH5k"),
                verbatimTextOutput("video"),
                verbatimTextOutput("stats"),
                verbatimTextOutput("comments"),
                plotOutput("sentiment_bar"),
                textInput(inputId = "title_search",
                          label = "Enter the text to be searched",
                          value = "Barack Obama"),
                verbatimTextOutput("search")
)
server <- function(input, output){
  video_id <- reactive({
    sub(".*v=", "", input$title)
  })
  
  output$video <- renderPrint({
    print("The Video Id is : ") 
    video_id()
  })
  
  output$stats <- renderPrint({
    print("Video Statistics are : ")
    get_stats(video_id())
  })
  
  output$comments <- renderPrint({
    print("Comments Table : ")
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      mutate(comment_number = row_number()) %>%
      unnest_tokens(word, textDisplay) %>%
      anti_join(stop_words)  %>%
      inner_join(get_sentiments("bing")) %>%
      mutate(index = comment_number %/% 15) %>%
      count(index, sentiment) %>%
      pivot_wider(names_from = "sentiment", values_from = "n") %>%
      mutate(sentiment = positive - negative) %>%
      mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                         ifelse(sentiment < 0, "Negative",
                                                "Neutral")))
  })
  
  output$sentiment_bar <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      mutate(comment_number = row_number()) %>%
      unnest_tokens(word, textDisplay) %>%
      anti_join(stop_words)  %>%
      inner_join(get_sentiments("bing")) %>%
      mutate(index = comment_number %/% 15) %>%
      count(index, sentiment) %>%
      pivot_wider(names_from = "sentiment", values_from = "n") %>%
      mutate(sentiment = positive - negative) %>%
      mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                         ifelse(sentiment < 0, "Negative",
                                                "Neutral"))) %>%
      ggplot(aes(x = Sentiment_Category)) + geom_bar()
  })
  
  output$search <- renderPrint({
    print("Searched Results are : ")
    yt_search(input$title_search) %>% select(video_id,title, channelTitle, description) %>% head(5)
  })
}

shinyApp(ui = ui, server = server)

#Sharing App

