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
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101)
  })

  
  output$search <- renderPrint({
    yt_search("Barack Obama") %>% select(video_id,title, channelTitle, description) %>% head(5)
  })
}

shinyApp(ui = ui, server = server)

#Sharing App


