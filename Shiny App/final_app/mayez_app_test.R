#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Different datasets

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

# datsets <- list(
#   nba_data_cleaned    = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/nba_data_cleaned.csv"),
#   cric_bat_data       = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/cric_bat_data.csv"),
#   mlb_data_cleaned    = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/mlb_data_cleaned.csv"),
#   energy_data_cleaned = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/energy_data_cleaned.csv"),
#   news_data_cleaned   = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/news_data_cleaned.csv")
# )


dataset_choices <- c("NBA Player Data" = "nba_data_cleaned",
                     "Cricket Batting Data" = "cric_bat_data",
                     "MLB Game Data" = "mlb_data_cleaned",
                     "Energy Consumption Data" = "energy_data_cleaned",
                     "Article Popularity Data" = "news_data_cleaned")


library(shiny)
library(shinycssloaders)
library(shinyjs)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(latex2exp)
library(broom)



ui <-
  navbarPage(
    title = "Model Diagnostics",
    tabPanel(title = "Select Data",
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "DataSet",
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
                                        value = 250),
                            actionButton("resample", "Generate new sample")
               ),
               mainPanel(withSpinner(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineOriginal")),
                             plotOutput("fittedLineHoriginal")))
               )
               )
             )
    ),
    tabPanel(title = "Residuals vs. Fitted", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = "plot_options_rvf",
                                         label = h4("Plot Options:"),
                                         choices = c("Show red line" = "wRL",
                                                     "Show confidence interval" = "wCI",
                                                     "Show data only" = "woRL")), # radio buttons to change options for red line
                            
                            actionButton("resampleRVF", "Generate new sample")),  # regenerate a new random sample of the data),  # slider to decide sample size),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineRVF")),
                             withSpinner(plotOutput("rvfPlot1"))),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHrvf"))))
             )
    ),
    tabPanel(title = "Scale Location", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = "plot_options_sl",
                                         label = h4("Plot Options:"),
                                         choices = c("Show red line" = "wRL",
                                                     "Show confidence interval" = "wCI",
                                                     "Show data only" = "woRL")), # radio buttons to change options for red line
                            
                            actionButton("resampleSL", "Generate new sample")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineSL")),
                             withSpinner(plotOutput("slPlot1"))),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHsl"))
               )
               )
             )
    ),
    tabPanel(title = "Normal Q-Q", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(actionButton("resampleQQ", "Generate new sample")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineQQ")),
                             withSpinner(plotOutput("qqPlot1"))),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHqq"))))
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

  sample_size <- reactive({
    as.numeric(input$sample_size)
    })
    
    
    
  observeEvent(data(), {
    updateSelectInput(inputId = "y_var",
                      label = "Y Variable:",
                      choices = names(data()))})
  observeEvent(data(), {
    updateSelectInput(inputId = "x_var",
                      label = "X Variable:",
                      choices = names(data()))
  })


  # Define the data sample with the inputted sample size
  reg_data <- reactive({
    if (input$resample >= 0) {
      # req(input$y_var, input$x_var %in% colnames(data))
      data_sample <- sample_n(data(), sample_size())
      return(data_sample[, c(input$x_var, input$y_var)])
    }
  })

  # Create a reactiveValues for the history of linear fit
  history_data <- reactiveValues(history = data.frame(intercept = numeric(), slope = numeric()))

  # fit a linear model on sample
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  obsData <- reactive({
    augment(mod())
  })
  
  rvf_first <- reactive({obsData() %>% ggplot(aes(x = .fitted, y = .resid)) +
    geom_point(shape = 1) +
    geom_hline(yintercept = 0, linetype = 2, color = "black") +
    stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = sample_size()) +
    labs(title = "Residuals vs. Fitted",
         x = "Fitted values",
         y = "Residuals") +
    coord_cartesian(xlim = c(min(obsData()$.fitted), max(obsData()$.fitted)),
                    ylim = c(min(obsData()$.resid), max(obsData()$.resid)))})
  
  rvf_info <- reactive({ggplot_build(rvf_first)$data[[3]][, c(input$x_var,input$y_var)]})
  
  
  
  # history plots
  observeEvent(input$resample, {
    new_row <- data.frame(
      intercept = coef(mod())[1],
      slope = coef(mod())[2]
    )
    history_data$history <- rbind(history_data$history, new_row)
    
    # Update the plot
    output$fittedLineHoriginal <- 
      output$fittedLineHrvf <-
      output$fittedLineHsl <- 
      output$fittedLineHqq <- renderPlot({
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


  output$fittedLineOriginal <- 
    output$fittedLineRVF <-
    output$fittedLineSL <-
    output$fittedLineQQ <- renderPlot({
      reg_data() %>%
        ggplot(aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(shape = 1) +
        labs(title = "Data Fit") +
        # stat_smooth(method = "lm", se = FALSE, col="blue")
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
  
  output$rvfPlot1 <- renderPlot({
    rvfPlot <- switch(input$plot_options_rvf,
                      wRL = obsData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = sample_size()) +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals") +
                        coord_cartesian(xlim = c(min(obsData()$.fitted), max(obsData()$.fitted)),
                                        ylim = c(min(obsData()$.resid), max(obsData()$.resid))),
                      wCI = obsData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        stat_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5, n = sample_size()) +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals"),
                      woRL = obsData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals"))
    rvfPlot
    })

  output$qqPlot1 <- renderPlot({
    qqPlot <- obsData() %>%
      ggplot(aes(sample = .std.resid)) +
      geom_qq_line() +
      geom_qq() +
      labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals")
    qqPlot
  })

  output$slPlot1 <- renderPlot({
    slPlot <- switch(input$plot_options_sl,
                     wRL = obsData() %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                       geom_point(shape = 1) +
                       stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = sample_size()) +
                       labs(title= "Scale-Location",
                            x ="Fitted values",
                            y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                     wCI = obsData() %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                       geom_point(shape = 1) +
                       stat_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5, n = sample_size()) +
                       labs(title= "Scale-Location",
                            x ="Fitted values",
                            y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                     woRL = obsData() %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
                       geom_point(shape = 1) +
                       labs(title= "Scale-Location",
                            x ="Fitted values",
                            y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")))
    slPlot
  })

  ######################################################################
  ######################################################################
  ######################################################################
}

# Run the application 
shinyApp(ui = ui, server = server)