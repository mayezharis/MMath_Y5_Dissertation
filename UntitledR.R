library(shiny)

ui <- fluidPage(
  actionButton("button1", "Button 1"),
  actionButton("button2", "Button 2"),
  textOutput("output")
)

server <- function(input, output, session) {
  # Observe changes for both button 1 and button 2
  observeEvent(c(input$button1, input$button2), {
    # Your handler logic here
    output$output <- renderText({
      paste("Button 1 clicked:", input$button1, "times\n",
            "Button 2 clicked:", input$button2, "times")
    })
  })
}

shinyApp(ui, server)