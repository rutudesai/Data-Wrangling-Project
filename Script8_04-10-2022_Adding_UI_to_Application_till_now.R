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
ui <- fluidPage(tags$h1("Data Wrangling and Husbandry 16:954:597:01 Project"),
                           tags$p("This is an", tags$strong("application")," which finds the overall sentiment of any video on YouTube and classifies the topic of video based on the User Comments."),
                           tags$hr(),
                           navbarPage(title = "16:954:597:01",
                           tabPanel("About Us", tags$h2("Team Introduction : "),
                                    tags$li("Rajesh Bhat : rmb317"),
                                    tags$li("Rutu Desai : rmd228"),
                                    tags$li("Shubham Kokane : ssk203")
                                    ),
                           tabPanel("Project Details", 
                                    tags$h2("Details About Project :"),
                                    tags$p(tags$h3(tags$li("Project Aim : ")), "The main aim of the project is to analyse the sentiments of the comments
                                           from different YouTube Comments and to find the topic based on the
                                           classification of the comments using Latent Dirichlet Allocation (LDA) method
                                           and try to present the sentiment score for each video by establishing a colour
                                           grading for each sentiment on public opinions.",
                                           tags$h3(tags$li("Abstract : ")), "We know that data is generated tremendously every second of the day and
                                           YouTube being one of the most favourite video streaming platform which has
                                           a lot of data generated on each video which can have different types of
                                           context (semantically). To find if the comments are acceptable or not it's very
                                           difficult nowadays so to find the Topic from comments and finding the
                                           comments which are good and bad so that bad comments can be filtered is
                                           really necessary.",
                                           tags$h3(tags$li("About Data and Process : ")),"We will scrape the data from different genre YouTube comment sections and
                                           append it to a .csv file for further analysis.",
                                           tags$h3(tags$li("Methodology Used : ")), "We will start by scraping data from YouTube comments of different genres.
                                           Use different methodologies to clean the data, for instance using regex to
                                           find specific patterns, removing stop words.
                                           Use Data Visualizations to show different graphs and charts.
                                           Will then compare both methods of LDA (VEM and Gibbs) and try to compare
                                           each model with different 'k' values.
                                           Try to find/guess topics from the model created above.
                                           Give out the sentiment score of each video.
                                           Colour grading will be established on public sentiments.")),
                           navbarMenu(title = "Application",
                                    tabPanel("Search with Video Link", 
                                    textInput(inputId = "title",
                                              label = "Enter the url",
                                              value = "https://www.youtube.com/watch?v=YF1eYbfbH5k"),
                                    verbatimTextOutput("video"),
                                    verbatimTextOutput("stats"),
                                    verbatimTextOutput("comments"),
                                    plotOutput("sentiment_bar")),
                                    tabPanel("Search Video Based on a Topic and get Video Link",
                                    textInput(inputId = "title_search",
                                              label = "Enter the text to be searched",
                                              value = "Barack Obama"),
                                    verbatimTextOutput("search")))
                           )
               
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
      ggplot(aes(x = Sentiment_Category)) + geom_bar() + 
      ggtitle("Bar Plot to compare Sentiments in Comments Section") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Sentiment Category") + ylab("Count of Sentiment")
  })
  
  output$search <- renderPrint({
    print("Searched Results are : ")
    yt_search(input$title_search) %>% select(video_id,title, channelTitle, description) %>% head(5)
  })
}

shinyApp(ui = ui, server = server)

#Sharing App

