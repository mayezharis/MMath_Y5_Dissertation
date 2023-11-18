#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

share_data <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/diag_plots/share_data.csv")

library(shiny)
library(shinyjs)
library(rhandsontable)
library(shinycssloaders)
# library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Model Diagnostics",
  fluid = TRUE,
  tabPanel(
    "Enter data",
    useShinyjs(),
    fluidRow(
      
      column(4,
             selectInput("y_var", "Y Variable:", c("Age (in years)" = "age",
                                                   "Gender" = "female",
                                                   "BMI" = "bmi",
                                                   "Education Level" = "isced1997_r")),
             selectInput("x_var", "X Variable:", c("Age (in years)" = "age",
                                                   "Gender" = "female",
                                                   "BMI" = "bmi",
                                                   "Education Level" = "isced1997_r"))
            )
             )
      ),
      
      column(8,
             withSpinner(plotOutput("fittedLine")),
             verbatimTextOutput("DF"),
             br(),
             tableOutput("fittedEqn")
      ),
  
  
  tabPanel(
    "Observed plot",
    sidebarLayout(
      sidebarPanel(
        radioButtons("plot", label = h4("Type of residual plot"),
                     c("Residuals vs. fitted values" = "resid.fitted",
                       "Residuals vs. x" = "resid.x", 
                       "Normal Q-Q plot" = "qq"), 
                     selected = "resid.fitted")
      ),
      mainPanel(
        plotOutput("origPlot")
      )
    )
    
  )
)
  
     
    # ),
        # sidebarPanel(
        #   selectInput("y_var", "Y Variable:", c("Age (in years)" = "age",
        #                                                     "Gender" = "female",
        #                                                     "BMI" = "bmi",
        #                                                     "Education Level" = "isced1997_r")),
        #   
        #   conditionalPanel(condition = "input.y_var == 'age'",
        #                    selectInput("x_var", "X Variable:", c("Gender" = "female",
        #                                                          "BMI" = "bmi",
        #                                                          "Education Level" = "isced1997_r"))),
        #   conditionalPanel(condition = "input.y_var == 'female'",
        #                    selectInput("x_var", "X Variable:", c("Age (in years)" = "age",
        #                                                          "BMI" = "bmi",
        #                                                          "Education Level" = "isced1997_r"))),
        #   conditionalPanel(condition = "input.y_var == 'bmi'",
        #                    selectInput("x_var", "X Variable:", c("Age (in years)" = "age",
        #                                                          "Gender" = "female",
        #                                                          "Education Level" = "isced1997_r"))),
        #   conditionalPanel(condition = "input.y_var == 'isced1997_r'",
        #                    selectInput("x_var", "X Variable:", c("Age (in years)" = "age",
        #                                                          "Gender" = "female",
        #                                                          "BMI" = "bmi"))),
        #   actionButton("run", "Re-sample data"),
        # ),
        # # Show a plot of the generated distribution
        # mainPanel(
        #    plotOutput("diagnostic_plots, height = 1000px")
#         )
#     )
# )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rv <- reactiveValues(show = FALSE)
  
  reg_data <- reactive({share_data[, c(input$x_var, input$y_var)]
    })
  
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
    })
  
  output$fittedEqn <- renderTable({
    mod() %>%
      tidy() %>%
      select(Term, Estimate)
    })
  
  output$fittedLine <- renderPlot({
    reg_data() %>%
      ggplot(aes_string(input$x_var, input$y_var)) +
      geom_point(shape = 1) +
      geom_smooth(method = "lm", se = FALSE)
  })
  
  output$origPlot <- renderPlot({
    obsData <- augment(mod())
    dataPlot <- switch(input$plot,
                       resid.fitted = obsData %>%
                         ggplot() +
                         geom_hline(yintercept = 0, linetype = 2, color = "black") +
                         geom_point(aes(x = .fitted, y = .resid), shape = 1) +
                         labs(x = "Fitted values", y = "Residuals"),
                       resid.x = obsData %>%
                         ggplot() +
                         geom_hline(yintercept = 0, linetype = 2, color = "black") +
                         geom_point(aes_string(x = input$x_var, y = ".resid"), shape = 1) +
                         labs(x = input$x_var, y = "Residuals"),
                       qq = obsData %>%
                         ggplot(aes(sample = .std.resid)) +
                         geom_qq_line() +
                         geom_qq() +
                         labs(x = "N(0, 1) quantiles", y = "Standardized residuals")
    )
    dataPlot +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
