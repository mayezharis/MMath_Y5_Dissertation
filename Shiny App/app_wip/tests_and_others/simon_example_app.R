
library(shiny)

# Define initial values

x2 <- seq(from=0,to=1,by=0.05)
y2 <- 5 + 2*x2 + rnorm(length(x2), sd = 0.5)
coef2 <- coef(lm(y2~x2))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Example: Y = 5 + 2x + E, where E~N(0,1/4)"),
  h4("Key -- Points: data set of 20 rows, Red line: line of best fit, Blue dahsed line: E[Y]=5+2x "),
  
  # Sidebar with a slider input for cut
  sidebarLayout(
    sidebarPanel(
      actionButton("add1", "Add 1"),
      actionButton("add100", "Add 100"),
      actionButton("reset", "Reset"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot1"),
      plotOutput("distPlot2")
    )
  )
)

#Define server actions to perform on action

server <- function(input, output) {
  
  data <- reactiveValues(x2 = x2, y2 = y2, lines = list(coef2))
  
  output$distPlot1 <- renderPlot({
    plot(data$x2, data$y2, ylim = c(4,8),xlim=c(0,1),
         main = "Fit of current data set", xlab = "x", ylab = "y")
    abline(a=data$lines[[1]][1], b=data$lines[[1]][2],col="red")
    abline(a=5,b=2,col="blue",lty=2)
  })
  
  output$distPlot2 <- renderPlot({
    plot(data$x2, data$y2, ylim = c(4,8),xlim=c(0,1),type="n",
         main = "History of best fit lines", xlab = "x", ylab = "y")
    for(i in seq_along(data$lines)){
      abline(a=data$lines[[i]][1], b=data$lines[[i]][2], col="grey60")
    }
    abline(a=data$lines[[1]][1], b=data$lines[[1]][2],col="red")
    abline(a=5,b=2,col="blue",lty=2)
    
  })

  
  
  observeEvent(input$add1,{
    data$y2 <- 5 + 2*x2 + rnorm(length(data$x2), sd = 0.5)
    data$lines = c(list(coef(lm(data$y2~data$x2))), data$lines)
  })

  observeEvent(input$add100,{
    for(i in 1:100){
      data$y2 <- 5 + 2*x2 + rnorm(length(data$x2), sd = 0.5)
      data$lines = c(list(coef(lm(data$y2~data$x2))), data$lines)
    }
  })
  
  observeEvent(input$reset,{
    data$x2 <- seq(from=0,to=1,by=0.05)
    data$y2 <- 5 + 2*x2 + rnorm(length(data$x2), sd = 0.5)
    data$lines = list(coef(lm(data$y2~data$x2)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
