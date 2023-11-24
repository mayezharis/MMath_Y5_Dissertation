#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Different data sets to play around with. Uncomment to use (remember to change choices in UI)

# ENERGY DATA
EnergyData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/energy_data_cleaned.csv")

# NEWS DATA
NewsData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/news_data_cleaned.csv")


# MLB DATA
BaseballData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/mlb_data_cleaned.csv")


# CRICKET BATTING DATA
CricketData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/cric_bat_data.csv")


# NBA PLAYER DATA
BasketballData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/nba_data_cleaned.csv")




dataset_choices <- c("NBA Player Data" = "nba_data_cleaned",
                     "Cricket Batting Data" = "cric_bat_data",
                     "MLB Game Data" = "mlb_data_cleaned",
                     "Energy Consumption Data" = "energy_data_cleaned",
                     "Article Popularity Data" = "news_data_cleaned")


library(shiny)
library(shinyjs)
library(rhandsontable)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)


# DEFINE UI
ui <- 
  fluidPage(
    h1("Model Diagnostics"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "DataSet",
                    label = "Choose dataset:",
                    choices = dataset_choices),
        selectInput(inputId = "y_var",
                    label = "Y Variable:",
                    choices = colnames(data())),  # drop-down for y variable
        
        selectInput(inputId = "x_var",
                    label = "X Variable:",
                    choices = colnames(data())),  # drop-down for x-variable
        
        sliderInput(inputId = "sample_size",
                    label = "Select sample size:",
                    min = 50,
                    max = 500, 
                    value = 250),  # slider to decide sample size
        
        # selectInput("sample_size", "Select Sample Size:",
        #             choices = c(500, 1000, 2500, 5000),
        #             selected = 1000),  # drop-down for sample size
        
        radioButtons("plot_type", 
                     label = h3("Type of diagnostic plot"),
                     c("Residuals vs. Fitted values" = "resid.fitted",
                       "Scale-Location" = "scale.location",
                       "Normal Q-Q plot" = "qq")), # radio buttons to decide the plot type
        # 
        radioButtons(inputId = "plot_options",
                     label = h4("Plot Options (Residuals vs. Fitted and Scale-Location plots only):"),
                     choices = c("Show red line" = "wRL",
                                 "Show confidence interval" = "wCI",
                                 "Show data only" = "woRL")), # radio buttons to change options for red line
        
        actionButton("resample", "Generate new sample")  # regenerate a new random sample of the data
      ),
      mainPanel(fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    withSpinner(plotOutput("fittedLine1")),
                    withSpinner(plotOutput("origPlot1"))),   # render the plots
        br(),
        splitLayout(cellWidths = c("50%", "50%"),
                    withSpinner(plotOutput("fittedLine2")))#,
        # withSpinner(plotOutput("origPlot2"))),   # render the plots
        # tableOutput("fittedEqn"))
      )
      )
    )
  )




# DEFINE SERVER
server <- function(input, output, session) {

  data <- reactive({
    if (input$DataSet == "nba_data_cleaned"){
      dataset <- BasketballData
    }
    else if (input$DataSet == "cric_bat_data"){
      dataset <- CricketData
    }
    else if (input$DataSet == "mlb_data_cleaned"){
      dataset <- BaseballData
    }
    else if (input$DataSet == "energy_data_cleaned"){
      dataset <- EnergyData
    }
    else if (input$DataSet == "news_data_cleaned"){
      dataset <- NewsData
    }
    return(dataset)
  })
  observeEvent(data(), {
    updateSelectInput(inputId = "y_var",
                      label = "Y Variable:",
                      choices = colnames(data()))})
  observeEvent(data(), {
    updateSelectInput(inputId = "x_var",
                      label = "X Variable:",
                      choices = colnames(data()))
  })
  
  
  # Define the data sample with the inputted sample size
  reg_data <- reactive({
    if (input$resample >= 0) {
      # req(input$y_var, input$x_var %in% colnames(data))
      data_sample <- sample_n(data(), as.numeric(input$sample_size))
      return(data_sample[, c(input$x_var, input$y_var)])
    }
  })
  
  # Create a reactiveValues for the history of linear fit
  history_data <- reactiveValues(history = data.frame(intercept = numeric(), slope = numeric()))
  
  
  # fit a linear model on sample
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  
  # history plots
  observeEvent(input$resample, {
    new_row <- data.frame(
      intercept = coef(mod())[1],
      slope = coef(mod())[2]
    )
    history_data$history <- rbind(history_data$history, new_row)
    # Update the plot
    output$fittedLine2 <- renderPlot({
      ggplot(data = data(), mapping = aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(alpha = 0) +
        geom_abline(data = history_data$history,
                    aes(intercept = intercept, slope = slope),
                    col = "cornflowerblue") +
        labs(title = "History of Linear Fits")
    })
  })

  
  
  ######################################################################
  ######################################################################
  #####################  FIRST FITTED LINE  ############################
  ######################################################################
  ######################################################################
  
  
  output$fittedLine1 <- renderPlot({
    reg_data() %>%
      ggplot(aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(shape = 1) +
      labs(title = "Data Fit") +
      # geom_smooth(method = "lm", se = FALSE, col="blue")
      geom_abline(intercept = coef(mod())[1],
                  slope = coef(mod())[2],
                  col = "blue", linewidth = 0.5)
  })
  ######################################################################
  ######################################################################
  ######################################################################
  
  
  
  

  
  
  ######################################################################
  ######################################################################
  ######################  DIAGNOSTIC PLOTS  ############################
  ######################################################################
  ######################################################################
  
  output$origPlot1 <- renderPlot({
    obsData <- augment(mod())
    dataPlot <- switch(
      input$plot_type,
      resid.fitted = switch(input$plot_options,
                            wRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                              geom_hline(yintercept = 0, linetype = 2, color = "black") +
                              geom_point(shape = 1) +
                              geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5) +
                              labs(title = "Residuals vs. Fitted",
                                   x = "Fitted values",
                                   y = "Residuals") +
                              coord_cartesian(xlim = c(min(obsData$.fitted), max(obsData$.fitted)),
                                              ylim = c(min(obsData$.resid), max(obsData$.resid))),
                            wCI = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                              geom_hline(yintercept = 0, linetype = 2, color = "black") +
                              geom_point(shape = 1) +
                              geom_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5) +
                              labs(title = "Residuals vs. Fitted",
                                   x = "Fitted values",
                                   y = "Residuals"),
                            woRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                              geom_hline(yintercept = 0, linetype = 2, color = "black") +
                              geom_point(shape = 1) +
                              labs(title = "Residuals vs. Fitted",
                                   x = "Fitted values",
                                   y = "Residuals")
      ),
      
      scale.location = switch(input$plot_options,
                              wRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                                geom_point(shape = 1) +
                                geom_smooth(col="red", method = "loess", se=FALSE) +
                                labs(title= "Scale-Location",
                                     x ="Fitted values",
                                     y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                              wCI = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                                geom_point(shape = 1) +
                                geom_smooth(col="red", method = "loess", se=!FALSE) +
                                labs(title= "Scale-Location",
                                     x ="Fitted values",
                                     y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                              woRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                                geom_point(shape = 1) +
                                labs(title= "Scale-Location",
                                     x ="Fitted values",
                                     y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
      ),
      
      qq = obsData %>%
        ggplot(aes(sample = .std.resid)) +
        geom_qq_line() +
        geom_qq() +
        labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals"))
    dataPlot
  })
  ######################################################################
  ######################################################################
  ###################################################################### 
  
}

# Run the application 
shinyApp(ui = ui, server = server)