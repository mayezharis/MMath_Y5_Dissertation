
library(shiny)
library(ggplot2)
library(tidyverse)


# Define initial values

x2 <- seq(from=0,to=1,by=0.05)
y2 <- 5 + 2*x2 + rnorm(length(x2), sd = 0.5)
data2 <- data.frame(x2=x2,y2=y2)
fit <- lm(y2~x2, data = data2)
coef2 <- data.frame(a = coef(fit)[1], b = coef(fit)[2])


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
  
  data <- reactiveValues(data2 = data2, 
                         lines = coef2,
                         plot = ggplot(data = data.frame(x2=Inf,y2=Inf),   #hack!
                                       mapping = aes(x=x2,y=y2)) + 
                           geom_point() + 
                           theme_bw() +
                           labs(x = "x",y = "y", title = "History of best fit lines") +
                           coord_cartesian(xlim=c(0,1),ylim=c(4,8))
                         )
  
  output$distPlot1 <- renderPlot({
    ggplot(data = data$data2, mapping = aes(x=x2,y=y2)) + 
      geom_point() + 
      geom_abline(intercept = data$lines$a[1], 
                  slope = data$lines$b[1], col = "red") +
      geom_abline(intercept = 5, slope = 2, col = "blue", lty = "dashed") + 
      theme_bw() +
      labs(x = "x",y = "y", title = "Fit of current data set") +
      coord_cartesian(xlim=c(0,1),ylim=c(4,8))
  })
  
  output$distPlot2 <- renderPlot({
    data$plot + 
      geom_abline(intercept = data$lines$a[1], slope = data$lines$b[1], col = "red")+
      geom_abline(intercept = 5, slope = 2, col = "blue", lty = "dashed")
    
  })

  
  
  observeEvent(input$add1,{
    data$plot <- data$plot + 
      geom_abline(intercept = data$lines$a[1], 
                  slope = data$lines$b[1], col = "grey60")  #Add old to history
    data$data2$y2 <- 5 + 2*data2$x2 + rnorm(nrow(data$data2), sd = 0.5)
    data$lines = rbind(coef(lm(y2~x2, data = data$data2)),data$lines)
  })

  observeEvent(input$add100,{
    for(i in 1:100){
      data$plot <- data$plot + 
        geom_abline(intercept = data$lines$a[1], 
                    slope = data$lines$b[1], col = "grey60")  #Add old to history before making new
      data$data2$y2 <- 5 + 2*data2$x2 + rnorm(nrow(data$data2), sd = 0.5)
      data$lines = rbind(coef(lm(y2~x2, data = data$data2)),data$lines)
    }
  })
  
  observeEvent(input$reset,{
    data$data2$x2 <- seq(from=0,to=1,by=0.05)
    data$data2$y2 <- 5 + 2*data2$x2 + rnorm(nrow(data$data2), sd = 0.5)
    data$lines = rbind(coef(lm(y2~x2, data = data$data2)),data$lines)
    data$lines = data$lines[1, ,drop=FALSE]
    
    data$plot = ggplot(data = data.frame(x2=Inf,y2=Inf),   #hack!
                       mapping = aes(x=x2,y=y2)) + 
      geom_point() + 
      geom_abline(intercept = 5, slope = 2, col = "blue", lty = "dashed") + 
      theme_bw() +
      labs(x = "x",y = "y", title = "History of best fit lines") +
      coord_cartesian(xlim=c(0,1),ylim=c(4,8))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
