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
library("wordcloud")
library("wordcloud2")
library("RColorBrewer")


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
                                              value = "https://www.youtube.com/watch?v=EGcXF0iG-2s"),
                                              #value = "https://www.youtube.com/watch?v=hSRoKK4ZeRE"),
                                    verbatimTextOutput("video"),
                                    verbatimTextOutput("stats"),
                                    verbatimTextOutput("comments"),
                                    plotOutput("word_cloud"),
                                    plotOutput("overall_sentiment"),
                                    plotOutput("sentiment_bar"),
                                    fluidRow(
                                      column(6,
                                             plotOutput("topic_1")),
                                      column(6,
                                             plotOutput("topic_2")))),
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
      mutate(across(everything(), replace_na, 0)) %>%
      mutate(sentiment = positive - negative) %>%
      mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                         ifelse(sentiment < 0, "Negative",
                                                "Neutral")))
  })
  
  output$word_cloud <- renderPlot({
      get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      mutate(comment_number = row_number()) %>%
      unnest_tokens(word, textDisplay) %>%
      inner_join(get_sentiments("bing")) %>%
      anti_join(stop_words)  %>%
      group_by(word) %>% 
      select(c(4)) %>% 
      dplyr::summarise(frequency = n()) %>% 
      with(wordcloud(words = word, freq = frequency, min.freq = 1, max.words=300, 
                 random.order=FALSE, rot.per=0.35 ,colors=brewer.pal(8, "Dark2")))
  })
 
  output$overall_sentiment <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(videoId, authorDisplayName, textDisplay) %>%
      mutate(comment_number = row_number()) %>%
      unnest_tokens(word, textDisplay) %>%
      anti_join(stop_words)  %>%
      inner_join(get_sentiments("bing")) %>%
      mutate(index = comment_number %/% 15) %>%
      count(index, sentiment) %>%
      pivot_wider(names_from = "sentiment", values_from = "n") %>%
      mutate(across(everything(), replace_na, 0)) %>%
      mutate(sentiment = positive - negative) %>%
      mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                         ifelse(sentiment < 0, "Negative",
                                                "Neutral"))) %>%
      ggplot(aes(index, sentiment)) + 
      geom_bar(stat="identity") + 
      geom_smooth() +
      ggtitle("Bar Plot to Sentiments in Comments Section perevry 15 comments") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Comment Numbers") + ylab("Count of Sentiment Cateory")
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
      mutate(across(everything(), replace_na, 0)) %>%
      mutate(sentiment = positive - negative) %>%
      mutate(Sentiment_Category = ifelse(sentiment > 0, "Positive",
                                         ifelse(sentiment < 0, "Negative",
                                                "Neutral"))) %>%
      ggplot(aes(x = Sentiment_Category)) + geom_bar() + 
      ggtitle("Bar Plot to compare Sentiments in Comments Section") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Sentiment Category") + ylab("Count of Sentiment") + scale_fill_brewer(palette = "Reds")
  })
  
  output$topic_1 <- renderPlot({
      get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(textDisplay) %>%
      VectorSource(.) %>%
      Corpus(.) %>%
      DocumentTermMatrix(.) %>%
      .[apply(. , 1, sum) !=0, ] %>%
      LDA(., k = 2) %>%
      tidy(.) %>%
      as_tibble() %>%
      rename(word = term) %>%
      anti_join(stop_words) %>%
      rename(term = word) %>%
      filter(topic == 1) %>%
      mutate(beta_rank = min_rank(desc(beta))) %>%
      arrange(beta_rank) %>%
      head(10) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(beta, term)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Bar Plot to Show Terms in Topic 1 after LDA Classification") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Beta Score") + ylab("Word") + scale_fill_brewer(palette = "Greens")
      }) 
      
#      as_tibble(tidy(LDA(.[apply(DocumentTermMatrix(Corpus(VectorSource(.$textDisplay))), 1, sum)!=0, ], k=2)))
  
  output$topic_2 <- renderPlot({
    get_comment_threads(c(video_id=sub(".*v=", "", input$title)), max_results = 101) %>% 
      select(textDisplay) %>%
      VectorSource(.) %>%
      Corpus(.) %>%
      DocumentTermMatrix(.) %>%
      .[apply(. , 1, sum) !=0, ] %>%
      LDA(., k = 2) %>%
      tidy(.) %>%
      as_tibble() %>%
      rename(word = term) %>%
      anti_join(stop_words) %>%
      rename(term = word) %>%
      filter(topic == 2) %>%
      mutate(beta_rank = min_rank(desc(beta))) %>%
      arrange(beta_rank) %>%
      head(10) %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(beta, term)) +
      scale_fill_brewer(palette = "Blues") +
      geom_col(show.legend = FALSE) +
      ggtitle("Bar Plot to Show Terms in Topic 2 after LDA Classification") + 
      theme(plot.title = element_text(size = 10, hjust=0.5)) +
      xlab("Beta Score") + ylab("Word")
  })
  
  output$search <- renderPrint({
    print("Searched Results are : ")
    yt_search(input$title_search) %>% select(video_id, title, channelTitle, description) %>% head(5)
  })
}

shinyApp(ui = ui, server = server)

#Sharing App

