#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


######### NEWS DATA #########
list_of_vars_n <- c("shares", "n_tokens_title", "n_tokens_content", 
                    "n_unique_tokens", "num_hrefs", "num_imgs", 
                    "num_videos", "average_token_length")

n_data_choices <- c("No. of shares" =  "shares",
                    "No. of words in title" = "n_tokens_title",
                    "No. of words in text" = "n_tokens_content",
                    "No. of unique words in text" = "n_unique_tokens",
                    "No. of links" = "num_hrefs",
                    "No. of images" = "num_imgs",
                    "No. of videos" = "num_videos",
                    "Average word length" = "average_token_length")


######### MLB DATA #########
mlb_y_choices <- c("Home Team Score" = "h_score",
                   "Visiting Team Score" = "v_score",
                   "Score Difference" = "score_diff")
mlb_x_choices <- c("No. of hits by home team" = "h_hits",
                   "No. of hits by visiting team" = "v_hits",
                   "No. of homeruns by home team" = "h_homeruns",
                   "No. of homeruns by visiting team" = "v_homeruns",
                   "No. of strikeouts by home team" = "h_strikeouts",
                   "No. of strikeouts by visiting team" = "v_strikeouts",
                   "No. of at bats by home team" = "h_at_bats",
                   "No. of at bats by visiting team" = "v_at_bats",
                   "Game length (in minutes)" = "length_minutes",
                   "Attendance" = "attendance")


######### CRICKET DATA #########
batting_vars_y <- c("No. of runs scored" = "Innings.Runs.Scored", 
                    "Minutes Batted" = "Innings.Minutes.Batted",
                    "No. of boundaries (fours)" = "Innings.Boundary.Fours",
                    "No. of boundaries (sixes)" = "Innings.Boundary.Sixes")

batting_vars_x <- c("Minutes Batted" = "Innings.Minutes.Batted",
                    "No.of Balls Faced" = "Innings.Balls.Faced", 
                    "No. of boundaries (fours)" = "Innings.Boundary.Fours", 
                    "No. of boundaries (sixes)" = "Innings.Boundary.Sixes", 
                    "Average Strike Rate" = "Innings.Batting.Strike.Rate")





# list_of_vars_e <- c("Appliances", "lights", "total_energy",
#                     "T_overall", "H_overall", "T_bedroom", 
#                     "H_bedroom", "T_shared", "H_shared", 
#                     "T_diff", "H_diff")



######### ENERGY DATA #########
e_data_y <- c("total_energy", "Appliances", "lights")
e_data_x <- c("T_overall", "H_overall", "T_bedroom", "H_bedroom", 
              "T_shared", "H_shared", "T_diff", "H_diff")

e_data_x_choices <- c("Average indoor temperature" = "T_overall",
                      "Average indoor humidity" = "H_overall",
                      "Average bedroom temperature" = "T_bedroom",
                      "Average bedroom humidity" = "H_bedroom",
                      "Average temperature in shared rooms" = "T_shared",
                      "Average humidity in shared rooms" = "H_shared",
                      "Difference between indoor and outdoor temp." = "T_diff",
                      "Difference between indoor and outdoor humidity" = "H_diff")

e_data_y_choices <- c("Total energy used" = "total_energy",
                      "Energy used by appliances" = "Appliances",
                      "Energy used by lights" = "lights")



######### NBA DATA #########
nba_y_vars <- c("Average Points per Game" = "PTS", 
                "Averge No. of minutes played per game" = "MP", 
                "Games Played" = "G", 
                "Games Started" = "GS")

nba_x_vars <- c("Age" = "Age", 
                "Average no. of minutes player per game" = "MP", 
                "Games Played" = "G",
                "Games Started" = "GS",
                "Percentage of field goals made" = "FG.",
                "Percentage of 3 pointers made (%3PM)" = "X3P.",
                "Percentage of 2 Pointers made (%2PM)" = "X2P.",
                "%3PM/%2PM" = "eFG.",
                "Percentage of free throws made" = "FT.",
                "Average rebounds per game" = "TRB",
                "Average assists per game" = "AST",
                "Average steals per game" = "STL",
                "Average blocks per game" = "BLK",
                "Average turnovers per game" = "TOV")

# datasets <- c(nba_data_cleaned, cric_bat_data, mlb_data_cleaned, energy_data_cleaned, news_data_cleaned)


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


# DEFINE UI
ui <- 
  fluidPage(
    h1("Model Diagnostics"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "DataSet",
                    label = "Choose dataset:",
                    choices = dataset_choices,
                    selected = "energy_data_cleaned"),
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
        
        actionButton("resample", "Generate new sample"),  # regenerate a new random sample of the data
        actionButton("reset_button", "Reset")  # reset the app
      ),
      mainPanel(fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    withSpinner(plotOutput("fittedLine")),
                    withSpinner(plotOutput("origPlot"))),   # render the plots
        br(),
        # tableOutput("fittedEqn"),
        uiOutput("update_prev_plots")  # UI to display previous iteration plots
        )
      )
    )
  )




server <- function(input, output, session) {
  
  # Different data sets to play around with. Uncomment to use (remember to change choices in UI)
  
  # ENERGY DATA
  EnergyData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/energy_data_cleaned.csv")
  
  # NEWS DATA
  NewsData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/news_data_cleaned.csv")
  
  # MLB DATA
  BaseballData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/mlb_data_cleaned.csv")
  
  # CRICKET BATTING DATA
  CricketData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/cric_bat_data.csv")
  
  # NBA PLAYER DATA
  BasketballData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/nba_data_cleaned.csv")
  
  # CHOOSING DATASET BASED ON INPUT
  data <- reactive({
    if (input$DataSet == "nba_data_cleaned") {
      dataset <- BasketballData
    } else if (input$DataSet == "cric_bat_data") {
      dataset <- CricketData
    } else if (input$DataSet == "mlb_data_cleaned") {
      dataset <- BaseballData
    } else if (input$DataSet == "energy_data_cleaned") {
      dataset <- EnergyData
    } else if (input$DataSet == "news_data_cleaned") {
      dataset <- NewsData
    }
    return(dataset)
  })
  
  observeEvent(data(), {
    updateSelectInput(inputId = "y_var",
                      label = "Y Variable:",
                      choices = colnames(data()))
  })
  
  observeEvent(data(), {
    updateSelectInput(inputId = "x_var",
                      label = "X Variable:",
                      choices = colnames(data()))
  })
  
  # Define the data sample with the inputted sample size
  reg_data <- reactive({
    req(input$resample, input$x_var, input$y_var, input$sample_size)
    isolate({
      if (input$resample >= 0) {
        data_sample <- sample_n(data(), as.numeric(input$sample_size))
      return(data_sample[, c(input$x_var, input$y_var)])
        }
      })
    })
  
  # fit a linear model on sample
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  # Initialize history
  history <- reactiveValues(plots = list())
  
  # Function to generate data and create plots
  generate_plots <- function() {
    obsDataCopy <- augment(mod())
    
    current_iteration <- list(
      obsDataCopy = obsDataCopy,
      plotType = input$plot_type,
      plotOptions = input$plot_options
    )
    
    # Update the history with the current iteration
    history$plots <- c(history$plots, list(current_iteration))
    
    # Limit the number of iterations to 10
    history$plots <- tail(history$plots, 10)
    
    return(current_iteration)
  }
  
  
  renderResidFitted <- function(obsDataCopy, plotOptions) {
    if (plotOptions == "wRL") {
      ggplot(obsDataCopy, aes(x = .fitted, y = .resid)) +
        geom_hline(yintercept = 0, linetype = 2, color = "black") +
        geom_point(shape = 1) +
        geom_smooth(col = "red", method = "loess", se = FALSE, linewidth = 0.5) +
        labs(title = "Residuals vs. Fitted",
             x = "Fitted values",
             y = "Residuals")
    } else if (plotOptions == "wCI") {
      ggplot(obsDataCopy, aes(x = .fitted, y = .resid)) +
        geom_hline(yintercept = 0, linetype = 2, color = "black") +
        geom_point(shape = 1) +
        geom_smooth(col = "red", method = "loess", se = !FALSE, linewidth = 0.5) +
        labs(title = "Residuals vs. Fitted",
             x = "Fitted values",
             y = "Residuals")
    } else if (plotOptions == "woRL") {
      ggplot(obsDataCopy, aes(x = .fitted, y = .resid)) +
        geom_hline(yintercept = 0, linetype = 2, color = "black") +
        geom_point(shape = 1) +
        labs(title = "Residuals vs. Fitted",
             x = "Fitted values",
             y = "Residuals")
    }
  }
  
  renderScaleLocation <- function(obsDataCopy, plotOptions) {
    if (plotOptions == "wRL") {
      ggplot(obsDataCopy, aes(x = .fitted, y = sqrt(abs((.resid - mean(.resid)) / sd(.resid))))) +
        geom_point(shape = 1) +
        geom_smooth(col = "red", method = "loess", se = FALSE) +
        labs(title = "Scale-Location",
             x = "Fitted values",
             y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
    } else if (plotOptions == "wCI") {
      ggplot(obsDataCopy, aes(x = .fitted, y = sqrt(abs((.resid - mean(.resid)) / sd(.resid))))) +
        geom_point(shape = 1) +
        geom_smooth(col = "red", method = "loess", se = !FALSE) +
        labs(title = "Scale-Location",
             x = "Fitted values",
             y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
    } else if (plotOptions == "woRL") {
      ggplot(obsDataCopy, aes(x = .fitted, y = sqrt(abs((.resid - mean(.resid)) / sd(.resid))))) +
        geom_point(shape = 1) +
        labs(title = "Scale-Location",
             x = "Fitted values",
             y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
    }
  }
  
  renderQQ <- function(obsDataCopy) {
    ggplot(obsDataCopy, aes(sample = .std.resid)) +
      geom_qq_line() +
      geom_qq() +
      labs(title = "Normal Q-Q Plot",
           x = "N(0, 1) quantiles",
           y = "Standardized residuals")
  }
  
  renderFittedLine <- function(obsDataCopy) {
    ggplot(reg_data(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(shape = 1) +
      geom_smooth(method = "lm", se = FALSE, col = "blue") +
      ggtitle("Data Fit")
  }
  
  renderPrevPlot <- function(i, type) {
    if (type == "fittedLine") {
      renderPlot({
        current_iteration <- history$plots[[i]]
        obsDataCopy <- current_iteration$obsDataCopy
        renderFittedLine(obsDataCopy)
      }, height = 300, width = 400)
    } else {
      renderPlot({
        current_iteration <- history$plots[[i]]
        obsDataCopy <- current_iteration$obsDataCopy
        plotType <- current_iteration$plotType
        plotOptions <- current_iteration$plotOptions
        
        switch(
          plotType,
          "resid.fitted" = renderResidFitted(obsDataCopy, plotOptions),
          "scale.location" = renderScaleLocation(obsDataCopy, plotOptions),
          "qq" = renderQQ(obsDataCopy)
        )
      }, height = 300, width = 400)
    }
    }
  
  # Observe the resample button click
  observe({
    # Observe changes in input parameters that should trigger plot updates
    inputs_to_observe <- c("DataSet", "y_var", "x_var", "sample_size", "plot_type", "plot_options")
    
    # Create a reactiveValues to store the previous inputs
    prev_inputs <- reactiveVal()
    
    # Observe changes in inputs
    observe({
      inputs <- sapply(inputs_to_observe, function(input_name) input[[input_name]])
      
      # Check if inputs have changed
      if (!identical(inputs, prev_inputs$values)) {
        prev_inputs$values <- inputs  # Update previous inputs
        
        # Regenerate and update the diagnostic plots
        plots <- generate_plots()
        
        output$fittedLine <- renderPrevPlot(length(history$plots), "fittedLine")
        output$origPlot <- renderPrevPlot(length(history$plots), "resid.fitted")
        
        # Update the previous iteration plots
        update_prev_plots()
      }
    })
  })
  
  # Observe the reset button click
  observeEvent(input$reset_button, {
    # Reset the history
    history$plots <- list()
    # Clear the current and previous iteration plots
    output$fittedLine <- renderPlot(NULL)
    output$origPlot <- renderPlot(NULL)
    output$prev_plots_ui <- renderUI(NULL)
  })
  
  # Function to update the UI with previous iteration plots
  update_prev_plots <- function() {
    # Create a list to store previous iteration plots
    prev_plots <- list()
    
    # Iterate over each iteration in history and add plots to the list
    for (i in seq_along(history$plots)) {
      current_iteration <- history$plots[[i]]
      obsDataCopy <- current_iteration$obsDataCopy
      plotType <- current_iteration$plotType
      plotOptions <- current_iteration$plotOptions
      
      # Add fittedLine plot to the list
      prev_plots[[paste0("fittedLine_", i)]] <- plotFittedLine(obsDataCopy)
      
      # Add diagnostic plot to the list based on plotType
      switch(
        plotType,
        "resid.fitted" = prev_plots[[paste0("residFitted_", i)]] <- plotResidFitted(obsDataCopy, plotOptions),
        "scale.location" = prev_plots[[paste0("scaleLocation_", i)]] <- plotScaleLocation(obsDataCopy, plotOptions),
        "qq" = prev_plots[[paste0("qqPlot_", i)]] <- plotQQ(obsDataCopy)
      )
    }
    
    # Create fluidRow with columns for each set of plots
    fluidRow(
      column(6, prev_plots),  # Adjust the width as needed
    )
  }
}


# Run the application 
shinyApp(ui = ui, server = server)