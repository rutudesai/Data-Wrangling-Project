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

yt_oauth()


#https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
#to extract pecific pattern after a pattern in r
ui <- fluidPage(textInput(inputId = "title",
                          label = "Enter the url",
                          value = "videoURL"),
                verbatimTextOutput("video"),
                verbatimTextOutput("stats"),
                verbatimTextOutput("comments"),
                textInput(inputId = "title_search",
                          label = "Enter the text to be searched",
                          value = "searchText"),
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
    print("Searched Results for ",input$title_search," are : ")
    yt_search(input$title_search) %>% select(video_id,title, channelTitle, description) %>% head(5)
  })
}

shinyApp(ui = ui, server = server)

#Sharing App

