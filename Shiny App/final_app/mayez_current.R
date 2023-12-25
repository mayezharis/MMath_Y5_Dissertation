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


jscode <- "shinyjs.refresh_page = function() { history.go(0); }" # function to refresh app


ui <-
  navbarPage(
    title = "Model Diagnostics",
    tabPanel(title = "Select Data",
             useShinyjs(),
             extendShinyjs(text = jscode, functions = "refresh_page"),
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
                            
                            actionButton("resample", "Generate new sample"),
                            br(),
                            br(),
                            actionButton("refresh", "Clear all outputs"),
                            br(),
                            br(),
                            textOutput("NumSamples")
               ),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineOriginal")),
                             plotOutput("fittedLineHoriginal"))
               )
               )
             )
    ),
    tabPanel(title = "Residuals vs. Fitted",
             useShinyjs(),
             extendShinyjs(text = jscode, functions = "refresh_page"),
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = "plot_options_rvf",
                                         label = h4("Plot Options:"),
                                         choices = c("Show red line" = "wRL",
                                                     "Show confidence interval" = "wCI",
                                                     "Show data only" = "woRL")), # radio buttons to change options for red line
                            actionButton("resample_rvf", "Generate new sample"),
                            br(),
                            br(),
                            actionButton("refresh_rvf", "Clear all outputs"),  # regenerate a new random sample of the data)
                            br(),
                            br(),
                            textOutput("NumSamplesRVF")
               ),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineRVF")),
                             withSpinner(plotOutput("rvfPlot1"))),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHrvf"),
                             plotOutput("rvf_history_plot"))
               ))
             )
    ),
    tabPanel(title = "Scale Location",
             useShinyjs(),
             extendShinyjs(text = jscode, functions = "refresh_page"),
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(radioButtons(inputId = "plot_options_sl",
                                         label = h4("Plot Options:"),
                                         choices = c("Show red line" = "wRL",
                                                     "Show confidence interval" = "wCI",
                                                     "Show data only" = "woRL")), # radio buttons to change options for red line
                            
                            actionButton("resample_sl", "Generate new sample"),
                            br(),
                            br(),
                            actionButton("refresh_sl", "Clear all outputs"),
                            br(),
                            br(),
                            textOutput("NumSamplesSL")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineSL")),
                             withSpinner(plotOutput("slPlot1"))),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHsl"),
                             plotOutput("sl_history_plot"))
               )
               )
             )
    ),
    tabPanel(title = "Normal Q-Q",
             useShinyjs(),
             extendShinyjs(text = jscode, functions = "refresh_page"),
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(actionButton("resample_qq", "Generate new sample"),
                            br(),
                            br(),
                            actionButton("refresh_qq", "Clear all outputs"),
                            br(),
                            br(),
                            textOutput("NumSamplesQQ")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("fittedLineQQ")),
                             withSpinner(plotOutput("qqPlot1"))),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHqq"),
                             plotOutput("qq_history_plot"))))
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
  
  observeEvent(input$refresh, {
    js$refresh_page();
  })
  
  observeEvent(input$refresh_rvf, {
    js$refresh_page();
  })

  observeEvent(input$refresh_sl, {
    js$refresh_page();
  })

  observeEvent(input$refresh_qq, {
    js$refresh_page();
  })


  sample_size <- reactive({
    as.numeric(input$sample_size)
    })
  
  
  resample <- reactiveValues(counter = 0)
  
  observeEvent(input$resample, {
    resample$counter <- resample$counter + 1
  })
  observeEvent(input$resample_rvf, {
    resample$counter <- resample$counter + 1
  })
  observeEvent(input$resample_sl, {
    resample$counter <- resample$counter + 1
  })
  observeEvent(input$resample_qq, {
    resample$counter <- resample$counter + 1
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

  
  mod_full <- reactive({
    lm(paste(input$y_var, "~", input$x_var), data())
  })
  aug_data_full <- reactive({
    augment(mod_full())
  })
  
  
  # Define the data sample with the inputted sample size
  reg_data <- reactive({
    if (resample$counter > 0) {
      # req(input$y_var, input$x_var %in% colnames(data))
      data_sample <- sample_n(data(), sample_size())
      return(data_sample[, c(input$x_var, input$y_var)])
   }
  })

  # fit a linear model on sample
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  obsData <- reactive({
    augment(mod())
  })
  
  normalize <- function(x, na.rm = TRUE) {
    return((x- min(x)) /(max(x)-min(x)))
  }
  
  ######################################################################
  ######################################################################
  ########################  RVF PLOT HISTORY  ##########################
  ######################################################################
  ######################################################################
  rvf_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  
  
  rvf_empty <- function() {
    ggplot(aug_data_full(), aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0) +
      geom_hline(yintercept = 0, linetype = 2, col = "black") +
      labs(title = "Residuals vs. Fitted History",
           x = "Fitted values",
           y = "Residuals")
  }
  
  rvf_current <- function(data) {
    data %>% ggplot(aes(x = .fitted, y = .resid)) +
      geom_point() +
      stat_smooth(method = "loess", se = FALSE, n = sample_size())
  }
  
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      rvf_new_line <- ggplot_build(rvf_current(obsData()))$data[[2]][, c("x", "y")]
      
      
      rvf_new_line$group <- nrow(rvf_history_data$history) / sample_size() + 1
      
      rvf_new_line$is_recent <- "TRUE"
      
      if (nrow(rvf_history_data$history) > 0) {
        rvf_history_data$history$is_recent <- "FALSE"
      }
      
      rvf_history_data$history <- rbind(rvf_history_data$history, rvf_new_line)
    
      output$rvf_history_plot <- renderPlot({
        rvf_empty() +
          geom_line(data = rvf_history_data$history,
                    aes(x = x, y = y, group = group, color = is_recent),
                    linewidth = 0.5, show.legend = FALSE) +
          scale_color_manual(values = c("FALSE" = "cornflowerblue", "TRUE" = "red"))
      })
    }
  })
  
  ######################################################################
  ######################################################################
  ######################################################################
  
  
  ######################################################################
  ######################################################################
  ########################  SL PLOT HISTORY  ###########################
  ######################################################################
  ######################################################################
  
  sl_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  
  sl_empty <- function() {
    ggplot(aug_data_full(), 
           aes(x = normalize(.fitted), y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
      geom_point(alpha = 0) +
      labs(title = "Scale-Location History",
           x = "Fitted values",
           y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
  }
  
  sl_current <- function(data) {
    data %>% ggplot(aes(x = normalize(.fitted), y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
      geom_point() +
      stat_smooth(method = "loess", se = FALSE, n = sample_size())
  }
  
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      sl_new_line <- ggplot_build(sl_current(obsData()))$data[[2]][, c("x", "y")]
      
      sl_new_line$group <- nrow(sl_history_data$history) / sample_size() + 1
      
      sl_new_line$is_recent <- "TRUE"
      
      if (nrow(sl_history_data$history) > 0) {
        sl_history_data$history$is_recent <- "FALSE"
      }
      
      
      sl_history_data$history <- rbind(sl_history_data$history, sl_new_line)
      
      
      output$sl_history_plot <- renderPlot({
        sl_empty() + 
          geom_line(data = sl_history_data$history, 
                    aes(x = x, y = y, group = group, color = is_recent), 
                    linewidth = 0.5, show.legend = FALSE) +
          scale_color_manual(values = c("FALSE" = "cornflowerblue", "TRUE" = "red"))
        
      })
    }
  })
  
  ######################################################################
  ######################################################################
  ######################################################################
  
  
  
  
  ######################################################################
  ######################################################################
  ########################  QQ PLOT HISTORY  ###########################
  ######################################################################
  ######################################################################
  
  qq_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  
  qq_empty <- function() {
    aug_data_full() %>% ggplot(aes(sample = .std.resid)) +
      stat_qq_line(linetype = 2) +
      labs(title = "History of Normal Q-Q Plot", 
           x = "N(0, 1) quantiles", 
           y = "Standardized residuals")
  }
  
  qq_current <- function(data) {
    data %>% ggplot(aes(sample = .std.resid)) +
      stat_qq_line() +
      stat_qq(geom = "path")
  }
  
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      
      qq_new_line <- ggplot_build(qq_current(obsData()))$data[[2]][, c("x", "y")]
      
      qq_new_line$group <- nrow(qq_history_data$history) / sample_size() + 1
      
      qq_new_line$is_recent <- "TRUE"
      
      if (nrow(qq_history_data$history) > 0) {
        qq_history_data$history$is_recent <- "FALSE"
      }
      
      
      qq_history_data$history <- rbind(qq_history_data$history, qq_new_line)
      
      
      output$qq_history_plot <- renderPlot({
        qq_empty() +
          geom_line(data = qq_history_data$history,
                    aes(x = x, y = y, group = group, color = is_recent),
                    linewidth = 0.5, inherit.aes = FALSE, show.legend = FALSE) +
          scale_color_manual(values = c("FALSE" = "cornflowerblue", "TRUE" = "red"))
      })
    }
  })
  
  ######################################################################
  ######################################################################
  ######################################################################
  
  
  
  
  

    
  ######################################################################
  ######################################################################
  ##################  FITTED LINE PLOT HISTORY  ########################
  ######################################################################
  ######################################################################
  
  # Create a reactiveValues for the history of linear fit
  history_data <- reactiveValues(history = data.frame(intercept = numeric(), slope = numeric()))
  
  # history plots
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      new_row <- data.frame(
        intercept = coef(mod())[1],
        slope = coef(mod())[2],
        is_recent = "TRUE"
      )
      
      if (input$x_var == input$y_var) {
        new_row$intercept <- 0
        new_row$slope <- 1
      }
      
      if (nrow(history_data$history) > 0) {
        history_data$history$is_recent <- "FALSE"
      }
      
      history_data$history <- rbind(history_data$history, new_row)

      
      # Update the plot
      output$fittedLineHoriginal <- 
        output$fittedLineHrvf <-
        output$fittedLineHsl <- 
        output$fittedLineHqq <- renderPlot({
          ggplot(data = data(), mapping = aes_string(x = input$x_var, y = input$y_var)) +
            geom_point(alpha = 0) +
            geom_abline(data = history_data$history,
                        aes(intercept = intercept, slope = slope, color = is_recent),
                        linewidth = 0.5, show.legend = FALSE) +
            scale_color_manual(values = c("FALSE" = "indianred1", "TRUE" = "blue")) +
            labs(title = "History of Linear Fits")
        })
      }
    })

  ######################################################################
  ######################################################################
  ######################################################################

  

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
        stat_smooth(method = "lm", se = FALSE, col="blue", linewidth = 0.5)
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
                             y = "Residuals"), # +
                        # coord_cartesian(xlim = c(min(obsData()$.fitted), max(obsData()$.fitted)),
                        #                 ylim = c(min(obsData()$.resid), max(obsData()$.resid))),
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
      stat_qq_line() +
      stat_qq() +
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
  
  
  ########################## COUNT OF SAMPLES ##########################
  output$NumSamples <- 
    output$NumSamplesRVF <- 
    output$NumSamplesSL <-
    output$NumSamplesQQ <- renderText({
      if(resample$counter > 0) {
        paste("No. of new samples: ", resample$counter)
      }
    })
  ######################################################################
  
}

# Run the application 
shinyApp(ui = ui, server = server)