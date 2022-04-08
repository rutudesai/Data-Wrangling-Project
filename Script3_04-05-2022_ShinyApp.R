#Tutorial Video : https://shiny.rstudio.com/tutorial/ 
#Using Shiny to make WebApplication using RStudio:

install.packages("shiny")
library("shiny")

#basic syntax:
#App Template

#ui <- fluidPage(
  # Input() functions,
  # Output() functions
#)
#server <- function(input, output){}
#shinyApp(ui = ui, server = server)

#Only these 3 lines will open a webpage

ui <- fluidPage(
  sliderInput(inputId = "num",
                label = "Choose a number",
                value = 25, min = 1, max = 100),
  textInput(inputId = "title",
                label = "Enter the title",
                value = "Histogram of Random Normal Values"),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

#Tell the server how to assemble inputs into outputs:
# 3 rules:
# 1) Save objects to dsiplay to output$
# 2) Build objects to display with render()
# 3) Use input values with input$
server <- function(input, output){
  data <- reactive({rnorm(input$num)
    })#So that whenever calling rnorm(input$num), we get same value and not different, for both outputs
  #Reactive values use caching
  output$hist <- renderPlot({ 
    hist(rnorm(data()), main = isolate(input$title)) #isolate() will help in stopping getting reactive results, it will wait till whole value is inserted. 
    })
  output$stats <- renderPrint({
    summary(data())
  })
}

shinyApp(ui = ui, server = server)

#Sharing App

