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

ui <- fluidPage(sliderInput(inputId = "num",
                label = "Choose a number",
                value = 25, min = 1, max = 100),
                plotOutput("hist"))

#Tell the server how to assemble inputs into outputs:
# 3 rules:
# 1) Save objects to dsiplay to output$
# 2) Build objects to display with render()
# 3) Use input values with input$
server <- function(input, output){
  output$hist <- renderPlot({ 
    title <- "Histogram of Random Normal Numbers"
    hist(rnorm(input$num), main = title) 
    })
}

shinyApp(ui = ui, server = server)

#Sharing App

