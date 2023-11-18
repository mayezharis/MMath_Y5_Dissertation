x2 <- seq(from=0,to=1,by=0.05)
y2 <- 5 + 2*x2 + rnorm(length(x2), sd = 0.5)
data2 <- data.frame(x2=x2,y2=y2)
fit <- lm(y2~x2, data = data2)

ggplot(data = data.frame(x2=Inf, y2 = Inf),
       mapping = aes(x=x2,y=y2)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "x",y = "y", title = "History of best fit lines")



library(shiny)
library(ggplot2)
library(shinyWidgets)

ui <- fluidPage(
  
  actionGroupButtons(
    inputIds = c("Bar", "Histogram", "Line"),
    labels = list("Bar", "Histogram","Line"),
    status = "danger",
    fullwidth = T
  ),
  
  plotOutput('plot',height = '563px'),
  verbatimTextOutput('text')
  
)

server <- function(input, output) {
  
  v <- reactiveValues(data = iris,
                      plot = NULL,
                      text = NULL)
  
  observeEvent(input$Bar, {
    v$plot <- ggplot(v$data, aes(Species,Petal.Length)) +
      geom_bar(stat="identity") 
    v$text <- "Bar"
  })
  
  observeEvent(input$Histogram, {
    data <- iris
    v$plot <- ggplot(v$data, aes(Petal.Length)) +
      geom_histogram()
    v$text <- "Histogram"
  })  
  
  observeEvent(input$Line, {
    data <- iris
    v$plot <- ggplot(v$data, aes(Petal.Length,Sepal.Length)) +
      geom_line()
    v$text <- "Line"
  })  
  
  output$plot <- renderPlot({
    if (is.null(v$plot)) return()
    v$plot
  })
  
  
  output$text <- renderText({
    
    if (is.null(v$text)) return()
    v$text
    
  })
}

shinyApp(ui, server)

