#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Different datasets

# TOY DATA
ToyData1 <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/toy_data_1.csv")
ToyData2 <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/toy_data_2.csv")
ToyData3 <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/toy_data_3.csv")
ToyData4 <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/toy_data_4.csv")
ToyData5 <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/toy_data_5.csv")


# ENERGY DATA
EnergyData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/energy_data_cleaned.csv")


# NEWS DATA
NewsData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/news_data_cleaned.csv")


# MLB DATA
BaseballData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/mlb_data_cleaned.csv")


# CRICKET BATTING DATA
CricketBatData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/cric_bat_data.csv")
CricketBowlData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/cric_bowl_data.csv")


# NBA PLAYER DATA
BasketballData <- read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/nba_data_cleaned.csv")




dataset_choices <- c("Toy Dataset 1 (linear)" = "toy_data_1",
                     "Toy Dataset 2 (quadratic)" = "toy_data_2",
                     "Toy Dataset 3 (cubic)" = "toy_data_3",
                     "Toy Dataset 4 (logarithmic)" = "toy_data_4",
                     "Toy Dataset 5 (composite function)" = "toy_data_5",
                     "NBA Player Data" = "nba_data_cleaned",
                     "Cricket Batting Data" = "cric_bat_data",
                     "Cricket Bowling Data" = "cric_bowl_data",
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
    tags$head(
      tags$link(rel="stylesheet", 
                href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                crossorigin="anonymous"),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
      HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
    ),
    title = "Model Diagnostics",
    tabPanel(title = "Select Data",
             useShinyjs(),
             extendShinyjs(text = jscode, functions = "refresh_page"),
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "DataSet",
                                        label = "Dataset:",
                                        choices = c("Select a dataset" = "" ,dataset_choices)),
                            
                            selectInput(inputId = "y_var",
                                        label = "Y Variable:",
                                        choices = c("Please select a dataset first" = "")),  # drop-down for y variable
                            
                            selectInput(inputId = "x_var",
                                        label = "X Variable:",
                                        choices = c("Please select a dataset first" = "")),  # drop-down for x-variable
                            
                            selectInput(inputId = "sample_size",
                                        label = "Sample size:",
                                        choices = c("Please select dataset and variables first" = "")),
                            
                            uiOutput("resample"),
                            br(),
                            uiOutput("refresh"),
                            br(),
                            br(),
                            textOutput("NumSamples")
               ),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineOriginal"),
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
                            uiOutput("resample_rvf"),
                            br(),
                            uiOutput("refresh_rvf"),  # regenerate a new random sample of the data)
                            br(),
                            br(),
                            textOutput("NumSamplesRVF")
               ),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineRVF"),
                             plotOutput("rvfPlot1")),   # render the plots
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
                            uiOutput("resample_sl"),
                            br(),
                            uiOutput("refresh_sl"),
                            br(),
                            br(),
                            textOutput("NumSamplesSL")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineSL"),
                             plotOutput("slPlot1")),   # render the plots
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
               sidebarPanel(uiOutput("resample_qq"),
                            br(),
                            uiOutput("refresh_qq"),
                            br(),
                            br(),
                            textOutput("NumSamplesQQ")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineQQ"),
                             plotOutput("qqPlot1")),   # render the plots
                 br(),
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineHqq"),
                             plotOutput("qq_history_plot"))))
             )
    ),
    tabPanel(title = "About",
             h2("App overview"),
             p("This app will allow you to construct linear regression models with small samples of various 
               large datasets. Using this, you may explore and investigate how models might vary under resampling. 
               The app specifically focuses on how the linear fit of the data and the model diagnostic plots may 
               vary with each different sample of the data that you take. Along with the linear fit and diagnostic 
               plots for the current sample, you will also be able to see plots from all previous samples for easy 
               comparison."),
             p("It will allow you to explore using toy datasets with predetermined relationships between the 
               independent and response variables, in addition to real world datasets with more random elements 
               and no previously known relationships between the various independent and response variables 
               contained within each of them."),
             
             
             h2("Linear fit"),
             p("This is a scatter plot of the current sample of the dataset. It allows us to see how the 
               data points in the current sample are structured. Additionally, we add a linear fit line 
               to the plot, which represents our linear regression model in the form of a straight line, 
               given by $\\mathbb{E}(Y_i) = \\alpha + \\beta x_i$ for $i = 1,\\dots,n$."),
             
             
             h2("Model diagnostic plots"),
             p("When constructing a normal linear regression model, given by 
               $Y_i \\sim N(\\alpha+\\beta x_i, \\sigma^2)$ for $i = 1, \\dots, n$, we 
               make the assumptions of:",
               tags$ol(
                 tags$li("Linearity of the expectation, $\\mathbb{E}(Y_i) = \\alpha + \\beta x_i$;"),
                 tags$li("Homoscedasticity (i.e. have the same variance); and"),
                 tags$li("Normally distributed.")
                 )),
             
             h4("Residuals vs. Fitted"),
             p("The  first assumption can be verified through the Residuals vs. Fitted plot. This is a scatter 
               plot of the residuals against fitted values. If the linear assumption is true, then the residuals 
               should be randomly distributed around the $x$-axis with no discernable pattern or clustering with 
               respect to the fitted values."),
             
             h4("Scale-Location"),
             p("The second assumption can be verified through the Scale-Location plot. This is a scatter plot 
               of the absolute values of standardized residuals against fitted values. Here, we are checking 
               for constant variance, so if the assumption is true, we expect there to be a random scatter around 
               a horizontal line with no discernable pattern or cluster with respect to the fitted values."),
             
             h4("Normal Q-Q"),
             p("The third assumption can be verified through the Normal Q-Q plot. This is a scatter plot of the 
               quantiles calculated from the sample data against the quantiles of a standard normal distribution 
               with mean 0 and standard deviation 1. If the assumption is true, we should see the data points 
               lying on a straight diagonal line, typically given by the $y=x$ line."),
             
             h4("Smoothing line and confidence intervals"),
             p("Both the Residuals vs. Fitted and Scale-Location plots may also contain a smoothing line to show 
               the general trend among the data points. While this may not be representative of the data’s 
               underlying structure, it can be helpful for identifying patterns or clusters which may not be 
               obvious to the naked eye. It is important to consider the error that may be associated with 
               calculating this line at each point, and this can be done through the use of a confidence band."),
             
             
             h2("Datasets"),
             p("This app allows you to choose from a selection of datasets to construct a model from. The toy 
               datasets have been constructed with predetermined relationships between their respective 
               independent and response variables."),
             
             h3("Toy datasets"),
             p("These are all datasets with a predetermined relationship between the independent and response 
               variables. To allow for a more realistic relationship, an element of randomness was included by 
               adding random noise to each of the response variables. The datasets were all defined by the 
               independent and response variables below."),
             h4("Dataset 1 (linear):"),
             p(tags$ul(
               tags$li("$x_i \\sim \\mathrm{Unif}(0,1)$"),
               tags$li("$y_i = x_i + r_i,\\,\\,$ where $\\,r_i \\sim N(0,0.25)$.")
             )),
             h4("Dataset 2 (quadratic):"),
             p(tags$ul(
               tags$li("$x_i \\sim N(0,1)$"),
               tags$li("$y_i = (0.5x_i-1)(0.5x_i+1) + r_i,\\,\\,$ where $\\,r_i \\sim N(0,0.8)$.")
             )),
             h4("Dataset 3 (cubic):"),
             p(tags$ul(
               tags$li("$x_i \\sim N(0,1)$"),
               tags$li("$y_i = (x_i-0.5)(-18x_i+0.4)(-x_i-0.5) + r_i,\\,\\,$ where $\\,r_i \\sim N(0,20)$.")
             )),
             h4("Dataset 4 (logarithmic):"),
             p(tags$ul(
               tags$li("$x_i \\sim N(0,1)$"),
               tags$li("$y_i = \\mathrm{ln}(x_i + s_i) + r_i,\\,\\,$ where $\\,s_i \\sim N(0,1)\\,\\,$ and 
                       $\\,r_i \\sim N(0,0.75)$")
             )),
             h4("Dataset 5 (composite function):"),
             p(tags$ul(
               tags$li("$x_i \\sim \\mathrm{Unif}(-0.25,1)$"),
               tags$li("$y_i = 0.5\\mathrm{exp}(-20x_i^3) - 0.5 + r_i,\\,\\,$ where $\\,r_i \\sim N(0,0.05)$")
             )),
             
             h3("Real world datasets"),
             p("When constructing a linear regression model, we are typically aiming to determine whether there 
               exists a relationship between independent variables and a response variable. Naturally, we aim to 
               use combinations of independent variables and response variables which could realistically have a 
               relationship between them. For example, when considering basketball player statistics, it is 
               understood that while a player playing more minutes per game does not necessarily guarantee they 
               will have a higher point score, it does increase the likelihood of them having a higher score at 
               the end of the game."),
             p("The datasets available on this app are all open source datasets found on the internet. They have 
               all been modified for the purposes of this app. This includes removing extreme outliers and unwanted 
               variables. Additionally, random noise of varying degrees has been added to all datasets to make the 
               data continuous and easier to visualise.")
             ),
  )




# DEFINE SERVER
server <- function(input, output, session) {

  data <- reactive({
    validate(need(input$DataSet != "", "Please select a data set."))
    if (input$DataSet == "toy_data_1") {
      dataset <- ToyData1
    }
    else if (input$DataSet == "toy_data_2") {
      dataset <- ToyData2
    }
    else if (input$DataSet == "toy_data_3") {
      dataset <- ToyData3
    }
    else if (input$DataSet == "toy_data_4") {
      dataset <- ToyData4
    }
    else if (input$DataSet == "toy_data_5") {
      dataset <- ToyData5
    }
    else if (input$DataSet == "nba_data_cleaned"){
      dataset <- BasketballData
    }
    else if (input$DataSet == "cric_bat_data"){
      dataset <- CricketBatData
    }
    else if (input$DataSet == "cric_bowl_data"){
      dataset <- CricketBowlData
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
  
  var_names <- reactive({
    req(!is.null(data()))
    colnames(data())
  })
  
  
  
  observeEvent(data(), {
    updateSelectInput(inputId = "y_var",
                      label = "Y Variable:",
                      choices = c("Select variable" = "", names(data())
                                  ))
  })
  
  observeEvent(data(), {
    updateSelectInput(inputId = "x_var",
                      label = "X Variable:",
                      choices = c("Select a y-variable please" = ""))
    })
  
  observeEvent(input$y_var, {
    req(input$y_var != "")
    updateSelectInput(inputId = "x_var",
                      label = "X Variable:",
                      choices = c("Select variable" = "", setdiff(names(data()), input$y_var)))
  })
  
  observeEvent(input$x_var, {
    req(input$x_var != "")
    updateSelectInput(inputId = "sample_size",
                      label = "Sample size:",
                      choices = c("Select a sample size" = "", 50, 100, 200, 300, 400, 500))
  })
  
  observeEvent(input$x_var, {
    if (input$x_var == ""){
      updateSelectInput(inputId = "sample_size",
                        label = "Sample size:",
                        choices = c("Please select dataset and variables first" = ""))
    }
  })


  sample_size <- reactive({
    req(input$sample_size != "")
    as.numeric(input$sample_size)
    })
  
  
  
  ######################################################################
  #####################  GENERATING NEW SAMPLES  #######################
  ######################################################################
  ######################################################################
  output$resample <- renderUI({
    req(sample_size())
    actionButton("resample", "Generate new sample")
  })
    output$resample_rvf <- renderUI({
    req(sample_size())
    actionButton("resample_rvf", "Generate new sample")
  })
  output$resample_sl <- renderUI({
    req(sample_size())
    actionButton("resample_sl", "Generate new sample")
  })
  output$resample_qq <- renderUI({
    req(sample_size())
    actionButton("resample_qq", "Generate new sample")
  })
  
  #====================================================================#
  
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
  
  #====================================================================#
  
  observeEvent(input$DataSet, {
    resample$counter <- 0
  })
  observeEvent(input$x_var, {
    resample$counter <- 0
  })
  observeEvent(input$y_var, {
    resample$counter <- 0
  })
  observeEvent(input$sample_size, {
    resample$counter <- 0
  })
  
  ######################################################################
  ######################################################################
  ######################################################################
    

  
    
  ######################################################################
  #######################  REFRESHING THE APP  #########################
  ######################################################################
  ######################################################################
  output$refresh  <- renderUI({
    req(resample$counter >0)
    actionButton("refresh", "Reset inputs")
  })
  output$refresh_rvf  <- renderUI({
    req(resample$counter >0)
    actionButton("refresh_rvf", "Reset inputs")
  })
  output$refresh_sl  <- renderUI({
    req(resample$counter >0)
    actionButton("refresh_sl", "Reset inputs")
  })
  output$refresh_qq  <- renderUI({
    req(resample$counter >0)
    actionButton("refresh_qq", "Reset inputs")
  })
  
  #====================================================================#
  
  observeEvent(input$refresh, {js$refresh_page();})
  
  observeEvent(input$refresh_rvf, {js$refresh_page();})
  
  observeEvent(input$refresh_sl, {js$refresh_page();})
  
  observeEvent(input$refresh_qq, {js$refresh_page();})
  
  ######################################################################
  ######################################################################
  ######################################################################
  

  
  mod_full <- reactive({
    lm(paste(input$y_var, "~", input$x_var), data())
  })
  
  aug_data_full <- reactive({
    augment(mod_full())
  })
  
  
  # Define the data sample with the inputted sample size
  reg_data <- reactive({
    req(input$x_var, input$y_var, input$x_var %in% names(data()))
    if (resample$counter > 0) {
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
  
  rvf_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      rvf_history_data$history <- data.frame(x = numeric(), y = numeric())
    }
  })
  
  rvf_empty <- reactive({
    req(resample$counter > 0)
    ggplot(aug_data_full(), aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0) +
      geom_hline(yintercept = 0, linetype = 2, col = "black") +
      labs(title = "Residuals vs. Fitted History",
           x = "Fitted values",
           y = "Residuals")
  })
  
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
  
  sl_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      sl_history_data$history <- data.frame(x = numeric(), y = numeric())
    }
  })
  
  sl_empty <- reactive({
    req(resample$counter > 0)
    ggplot(aug_data_full(), 
           aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
      geom_point(alpha = 0) +
      labs(title = "Scale-Location History",
           x = "Fitted values",
           y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
  })
  
  sl_current <- function(data) {
    data %>% ggplot(aes(x = .fitted, y = sqrt(abs((.resid-mean(.resid))/sd(.resid))))) +
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
  
  qq_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      qq_history_data$history <- data.frame(x = numeric(), y = numeric())
    }
  })
  
  qq_empty <- reactive({
    req(resample$counter > 0)
    aug_data_full() %>% ggplot(aes(sample = .std.resid)) +
      stat_qq_line(linetype = 2) +
      labs(title = "History of Normal Q-Q Plot", 
           x = "N(0, 1) quantiles", 
           y = "Standardized residuals")
  })
  
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
  
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      history_data$history <- data.frame(intercept = numeric(), slope = numeric())
    }
  })
  
  # history plots
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      new_row <- data.frame(
        intercept = coef(mod())[1],
        slope = coef(mod())[2],
        is_recent = "TRUE"
      )
      
      if (nrow(history_data$history) > 0) {
        history_data$history$is_recent <- "FALSE"
      }
      
      history_data$history <- rbind(history_data$history, new_row)

      
      # Update the plot
      output$fittedLineHoriginal <- 
        output$fittedLineHrvf <-
        output$fittedLineHsl <- 
        output$fittedLineHqq <- renderPlot({
          req(resample$counter > 0)
          ggplot(data = data(), mapping = aes_string(x = input$x_var, y = input$y_var)) +
            geom_point(alpha = 0) +
            geom_abline(data = history_data$history,
                        aes(intercept = intercept, slope = slope, color = is_recent),
                        linewidth = 0.5, show.legend = FALSE) +
            scale_color_manual(values = c("FALSE" = "cornflowerblue", "TRUE" = "red")) +
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

  output$fittedLineOriginal <- 
    output$fittedLineRVF <-
    output$fittedLineSL <-
    output$fittedLineQQ <- renderPlot({
      req(resample$counter > 0)
      reg_data() %>%
        ggplot(aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(shape = 1) +
        labs(title = "Data Fit") +
        stat_smooth(method = "lm", se = FALSE, col="red", linewidth = 0.5)
        # geom_abline(intercept = coef(mod())[1], slope = coef(mod())[2], color = "red")
    })

  ######################################################################
  ######################################################################
  ######################################################################





  ######################################################################
  ######################################################################
  ######################  DIAGNOSTIC PLOTS  ############################
  ######################################################################
  
  output$rvfPlot1 <- renderPlot({
    req(resample$counter > 0)
    rvfPlot <- switch(input$plot_options_rvf,
                      wRL = obsData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        stat_smooth(col="red", method = "loess", se=FALSE, linewidth = 0.5, n = sample_size()) +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals"),
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
    req(resample$counter > 0)
    qqPlot <- obsData() %>%
      ggplot(aes(sample = .std.resid)) +
      stat_qq_line() +
      stat_qq() +
      labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals")
    qqPlot
  })

  output$slPlot1 <- renderPlot({
    req(resample$counter > 0)
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
  
  
  
  
  ######################################################################
  ########################## COUNT OF SAMPLES ##########################
  ######################################################################
  
  output$NumSamples <- 
    output$NumSamplesRVF <- 
    output$NumSamplesSL <-
    output$NumSamplesQQ <- renderText({
      req(resample$counter > 0)
      req(input$sample_size != "")
      paste("No. of samples: ", resample$counter)
    })
  
  ######################################################################
  ######################################################################
  
}

# Run the application 
shinyApp(ui = ui, server = server)