library(shiny)
library(tidyverse)
library(broom)



dataset_choices <- c("NBA Player Data" = "nba_data_cleaned",
                     "Cricket Batting Data" = "cric_bat_data",
                     "MLB Game Data" = "mlb_data_cleaned",
                     "Energy Consumption Data" = "energy_data_cleaned",
                     "Article Popularity Data" = "news_data_cleaned")


# Define UI
ui <- fluidPage(
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
                  choices = colnames(data())),
      
      sliderInput("sample_size", 
                  "Select sample size:", 
                  min = 50, 
                  max = 500, 
                  value = 250),
      
      radioButtons("residual_type", 
                   "Diagnostic plot type:",
                   choices = c("Residuals vs. Fitted", 
                               "Scale-Location", 
                               "Normal Q-Q Plot")),
      radioButtons(inputId = "plot_options",
                   label = h4("Plot Options (Residuals vs. Fitted and Scale-Location plots only):"),
                   choices = c("Show red line" = "wRL",
                               "Show confidence interval" = "wCI",
                               "Show data only" = "woRL")), # radio buttons to change options for red line
      
      actionButton("resample", "Generate new sample"),
      actionButton("reset_button", "Reset")
    ),
    
    mainPanel(fluidRow(
      splitLayout(
        cellWidths = c("50%", "50%"),
        withSpinner(plotOutput("fittedLine")),
        withSpinner(plotOutput("origplot")))))))
    # ),
    #   fluidRow(
    #     column(6, plotOutput("previousFittedLine")),
    #     column(6, plotOutput("previousOrigPlot"))
      # tableOutput("fitted_eqn")

    
    
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
  
  
  # Function to generate a new sample
  reg_data <- reactive({
    if (input$resample >= 0) {
      data_sample <- sample_n(data(), as.numeric(input$sample_size))
      return(data_sample[, c(input$x_var, input$y_var)])
    }
  })
  
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  
  # Reactive expression to update plots when a new sample is generated
  
  # Function to create and store plots
  create_plots <- function() {
    p1 <- renderPlot({
      reg_data() %>%
        ggplot(aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(shape = 1) +
        geom_smooth(method = "lm", se = FALSE, col="blue")
    })
    
    p2 <- renderPlot({
      obsData <- augment(mod())
      switch(
        input$plot_type,
        resid.fitted = switch(input$plot_options,
                              wRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
                                geom_hline(yintercept = 0, linetype = 2, color = "black") +
                                geom_point(shape = 1) +
                                geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5) +
                                labs(title = "Residuals vs. Fitted",
                                     x = "Fitted values",
                                     y = "Residuals"),
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
    })
    
    # Add the new plots to the list of previous plots
    previous_plots$plots <- c(list(p1()), list(p2()), previous_plots$plots)
    previous_plots$data <- c(sample_data, previous_plots$data)
    
    # Keep only the last 10 plots and data
    if (length(previous_plots$plots) > 10) {
      previous_plots$plots <- previous_plots$plots[1:10]
      previous_plots$data <- previous_plots$data[1:10]
    }
  }
  
  
  # Render the current fittedLine plot
  output$fittedLine <- renderPlot({
    reg_data() %>%
      ggplot(aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(shape = 1) +
      geom_smooth(method = "lm", se = FALSE, col="blue")
  })
  
  # Render the current origPlot
  output$origPlot <- renderPlot({
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
                                   y = "Residuals"),
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
  
  # Render all previous linear fit plots
  output$previousFittedLine <- renderPlot({
    previous_plots_ui <- lapply(previous_plots$plots, function(plot) {
      plotOutput(NS(paste0("prevFitted_", plot$id)))
    })
    do.call(tagList, previous_plots_ui)
  })
  
  # Render all previous diagnostic plots
  output$previousOrigPlot <- renderPlot({
    previous_plots_ui <- lapply(previous_plots$plots, function(plot) {
      plotOutput(NS(paste0("prevOrig_", plot$id)))
    })
    do.call(tagList, previous_plots_ui)
  })
}


# server <- function(input, output, session) {
#   # Function to generate a new sample
#   reg_data <- reactive({
#     if (input$resample >= 0) {
#       # req(input$y_var, input$x_var %in% colnames(data))
#       data_sample <- sample_n(data(), as.numeric(input$sample_size))
#       return(data_sample[, c(input$x_var, input$y_var)])
#     }
#   })
#   
#   sample_data <- reg_data()
#   
#   # Function to create and store plots
#   create_plots <- function(sample_data) {
#     p1 <- renderPlot({
#       sample_data %>%
#         ggplot(aes_string(x = input$x_var, y = input$y_var)) +
#         geom_point(shape = 1) +
#         geom_smooth(method = "lm", se = FALSE, col="blue")
#     })
#     
#     p2 <- renderPlot({
#       switch(
#         input$plot_type,
#         resid.fitted = switch(input$plot_options,
#                               wRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
#                                 geom_hline(yintercept = 0, linetype = 2, color = "black") +
#                                 geom_point(shape = 1) +
#                                 geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5) +
#                                 labs(title = "Residuals vs. Fitted",
#                                      x = "Fitted values",
#                                      y = "Residuals"),
#                               wCI = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
#                                 geom_hline(yintercept = 0, linetype = 2, color = "black") +
#                                 geom_point(shape = 1) +
#                                 geom_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5) +
#                                 labs(title = "Residuals vs. Fitted",
#                                      x = "Fitted values",
#                                      y = "Residuals"),
#                               woRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
#                                 geom_hline(yintercept = 0, linetype = 2, color = "black") +
#                                 geom_point(shape = 1) +
#                                 labs(title = "Residuals vs. Fitted",
#                                      x = "Fitted values",
#                                      y = "Residuals")
#         ),
#         
#         scale.location = switch(input$plot_options,
#                                 wRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
#                                   geom_point(shape = 1) +
#                                   geom_smooth(col="red", method = "loess", se=FALSE) +
#                                   labs(title= "Scale-Location",
#                                        x ="Fitted values",
#                                        y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
#                                 wCI = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
#                                   geom_point(shape = 1) +
#                                   geom_smooth(col="red", method = "loess", se=!FALSE) +
#                                   labs(title= "Scale-Location",
#                                        x ="Fitted values",
#                                        y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
#                                 woRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
#                                   geom_point(shape = 1) +
#                                   labs(title= "Scale-Location",
#                                        x ="Fitted values",
#                                        y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
#         ),
#         
#         qq = obsData %>%
#           ggplot(aes(sample = .std.resid)) +
#           geom_qq_line() +
#           geom_qq() +
#           labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals"))
#     })
#     
#     # Add the new plots to the list of previous plots
#     previous_plots$plots <- c(list(p1()), list(p2()), previous_plots$plots)
#     previous_plots$data <- c(sample_data, previous_plots$data)
#     
#     # Keep only the last 10 plots and data
#     if (length(previous_plots$plots) > 10) {
#       previous_plots$plots <- previous_plots$plots[1:10]
#       previous_plots$data <- previous_plots$data[1:10]
#     }
#   }
#   
#   # Reactive expression to update plots when a new sample is generated
#   observe({
#     sample_data <- generate_sample()
#     create_plots(sample_data)
#   })
#   
#   # Render the current plots
#   output$fittedLine <- renderPlot({
#     reg_data() %>%
#       ggplot(aes_string(x = input$x_var, y = input$y_var)) +
#       geom_point(shape = 1) +
#       geom_smooth(method = "lm", se = FALSE, col="blue")
#   })
#   
#   
#   output$origPlot <- renderPlot({
#     dataPlot -> switch(
#       input$plot_type,
#       resid.fitted = switch(input$plot_options,
#                             wRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
#                               geom_hline(yintercept = 0, linetype = 2, color = "black") +
#                               geom_point(shape = 1) +
#                               geom_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5) +
#                               labs(title = "Residuals vs. Fitted",
#                                    x = "Fitted values",
#                                    y = "Residuals"),
#                             wCI = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
#                               geom_hline(yintercept = 0, linetype = 2, color = "black") +
#                               geom_point(shape = 1) +
#                               geom_smooth(col="red", method = "loess", se=!FALSE, linewidth = 0.5) +
#                               labs(title = "Residuals vs. Fitted",
#                                    x = "Fitted values",
#                                    y = "Residuals"),
#                             woRL = obsData %>% ggplot(aes(x = .fitted, y = .resid)) +
#                               geom_hline(yintercept = 0, linetype = 2, color = "black") +
#                               geom_point(shape = 1) +
#                               labs(title = "Residuals vs. Fitted",
#                                    x = "Fitted values",
#                                    y = "Residuals")
#       ),
#       
#       scale.location = switch(input$plot_options,
#                               wRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
#                                 geom_point(shape = 1) +
#                                 geom_smooth(col="red", method = "loess", se=FALSE) +
#                                 labs(title= "Scale-Location",
#                                      x ="Fitted values",
#                                      y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
#                               wCI = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
#                                 geom_point(shape = 1) +
#                                 geom_smooth(col="red", method = "loess", se=!FALSE) +
#                                 labs(title= "Scale-Location",
#                                      x ="Fitted values",
#                                      y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
#                               woRL = obsData %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
#                                 geom_point(shape = 1) +
#                                 labs(title= "Scale-Location",
#                                      x ="Fitted values",
#                                      y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
#       ),
#       
#       qq = obsData %>%
#         ggplot(aes(sample = .std.resid)) +
#         geom_qq_line() +
#         geom_qq() +
#         labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals"))
#     dataPlot
#   })
#   
#   # Render all previous linear fit plots
#   output$previousFittedLine <- renderPlot({
#     previous_plots_ui <- lapply(previous_plots$plots, function(plot) {
#       plotOutput(NS(paste0("prevFitted_", plot$id)))
#     })
#     do.call(tagList, previous_plots_ui)
#   })
#   
#   # Render all previous diagnostic plots
#   output$previousOrigPlot <- renderPlot({
#     previous_plots_ui <- lapply(previous_plots$plots, function(plot) {
#       plotOutput(NS(paste0("prevOrig_", plot$id)))
#     })
#     do.call(tagList, previous_plots_ui)
#   })
#   
#   # Function to render all previous plots
#   # renderPreviousPlots <- function(prefix, plots) {
#   #   lapply(seq_along(plots), function(i) {
#   #     output[[paste0(prefix, i)]] <- renderPlot({
#   #       # Code to render the corresponding plot
#   #       # Use plots[[i]]
#   #       # ...
#   #     })
#   #   })
#   # }
#   
#   # Call the function to render all previous linear fit plots
#   renderPreviousPlots("prevFitted_", previous_plots$plots)
#   
#   # Call the function to render all previous diagnostic plots
#   renderPreviousPlots("prevOrig_", previous_plots$plots)
# }

# shinyApp(ui, server)


# Run the application 
shinyApp(ui = ui, server = server)