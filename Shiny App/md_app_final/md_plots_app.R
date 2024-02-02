
######################################################################
########################  CALLING DATASETS  ##########################
######################################################################

# TOY DATA
ToyData1 <- read.csv("md_app_datasets/toy_data_1.csv")
ToyData2 <- read.csv("md_app_datasets/toy_data_2.csv")
ToyData3 <- read.csv("md_app_datasets/toy_data_3.csv")
ToyData4 <- read.csv("md_app_datasets/toy_data_4.csv")
ToyData5 <- read.csv("md_app_datasets/toy_data_5.csv")


# REAL-WORLD DATA
EnergyData <- read.csv("md_app_datasets/energy_data_cleaned.csv")
NewsData <- read.csv("md_app_datasets/news_data_cleaned.csv")
BaseballData <- read.csv("md_app_datasets/mlb_data_cleaned.csv")
CricketBatData <- read.csv("md_app_datasets/cric_bat_data.csv")
CricketBowlData <- read.csv("md_app_datasets/cric_bowl_data.csv")
BasketballData <- read.csv("md_app_datasets/nba_data_cleaned.csv")



# List of inputs for dataset selection
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



# Function to reset app
jscode <- "shinyjs.refresh_page = function() { history.go(0); }" 

# Shiny packages
library(shiny)
library(shinycssloaders)
library(shinyjs)

# Other packages
library(tidyverse)
library(latex2exp)
library(broom)


######################################################################
######################################################################
########################### SETTING UP UI ############################
######################################################################
######################################################################

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
                                        choices = c("Select a dataset" = "", dataset_choices)),
                            
                            selectInput(inputId = "y_var",
                                        label = "Y Variable:",
                                        choices = c("Please select a dataset first" = "")),
                            
                            selectInput(inputId = "x_var",
                                        label = "X Variable:",
                                        choices = c("Please select a dataset first" = "")), 
                            
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
                                                     "Show data only" = "woRL")),
                            uiOutput("resample_rvf"),
                            br(),
                            uiOutput("refresh_rvf"),
                            br(),
                            br(),
                            textOutput("NumSamplesRVF")
               ),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineRVF"),
                             plotOutput("rvfPlot1")),   
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
                                                     "Show data only" = "woRL")),
                            uiOutput("resample_sl"),
                            br(),
                            uiOutput("refresh_sl"),
                            br(),
                            br(),
                            textOutput("NumSamplesSL")),
               mainPanel(fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput("fittedLineSL"),
                             plotOutput("slPlot1")),
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
                             plotOutput("qqPlot1")),
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
               plots for the current sample, you will also be able to see plots from all previous samples for quick 
               comparison."),
             p("It will allow you to explore using toy datasets with predetermined relationships between the 
               explanatory and response variables, in addition to real world datasets with more random elements 
               and no previously known relationships between the various explanatory and response variables 
               contained within each of them."),
             
             
             h2("Linear fit"),
             p("This is a scatter plot of the current sample of the dataset. It allows us to see how the 
               data points in the current sample are structured. Additionally, we add a linear fit line 
               to the plot using a least squares estimation, which represents our linear regression model in the form of a straight line, 
               given by $\\mathbb{E}[Y_i] = \\alpha + \\beta x_i$ for $i = 1,\\dots,n$."),
             
             
             h2("Model diagnostic plots"),
             p("When constructing a normal linear regression model, given by 
               $Y_i \\sim N(\\alpha+\\beta x_i, \\sigma^2)$ for $i = 1, \\dots, n$, we 
               make the following assumptions:",
               tags$ol(
                 tags$li("The expectation of the response and the explanatory variable are linearly related, 
                         i.e. $\\mathbb{E}[Y_i] = \\alpha + \\beta x_i$;"),
                 tags$li("the errors have constant variance, $\\sigma^2$ (i.e. they exhibit homoscedasticity); and"),
                 tags$li("the errors are normally distributed.")
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
               lying on a straight diagonal line, also known as the identity line."),
             
             h4("Smoothing line and confidence intervals"),
             p("Both the Residuals vs. Fitted and Scale-Location plots may also contain a smoothing line, 
               constructed using local averages (LOESS), to show the general trend among the data points. 
               While this may not be representative of the data’s underlying structure, it can be helpful for 
               identifying patterns or clusters which may not be obvious to the naked eye. It is important to 
               consider the error that may be associated with calculating this line at each point, and this 
               can be done through the use of a confidence band."),
             
             
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
               tags$li("$y_i = x_i + \\epsilon_i,\\,\\,$ where $\\,\\epsilon_i \\sim N(0,0.25)$.")
             )),
             h4("Dataset 2 (quadratic):"),
             p(tags$ul(
               tags$li("$x_i \\sim N(0,1)$"),
               tags$li("$y_i = (0.5x_i-1)(0.5x_i+1) + \\epsilon_i,\\,\\,$ where $\\,\\epsilon_i \\sim N(0,0.8)$.")
             )),
             h4("Dataset 3 (cubic):"),
             p(tags$ul(
               tags$li("$x_i \\sim N(0,1)$"),
               tags$li("$y_i = (x_i-0.5)(-18x_i+0.4)(-x_i-0.5) + \\epsilon_i,\\,\\,$ where $\\,\\epsilon_i \\sim N(0,20)$.")
             )),
             h4("Dataset 4 (logarithmic):"),
             p(tags$ul(
               tags$li("$x_i \\sim N(0,1)$"),
               tags$li("$y_i = \\mathrm{ln}(x_i + s_i) + \\epsilon_i,\\,\\,$ where $\\,s_i \\sim N(0,1)\\,\\,$ and 
                       $\\,\\epsilon_i \\sim N(0,0.75)$")
             )),
             h4("Dataset 5 (composite function):"),
             p(tags$ul(
               tags$li("$x_i \\sim \\mathrm{Unif}(-0.25,1)$"),
               tags$li("$y_i = 0.5\\mathrm{exp}(-20x_i^3) - 0.5 + \\epsilon_i,\\,\\,$ where $\\,\\epsilon_i \\sim N(0,0.05)$")
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
               data continuous and easier to visualise."),
             
             h2("Author"),
             p("This application was made by Mayez Haris for an undergraduate final year dissertation at the University of 
               Edinburgh. The source code for the application is available",
               a("here", href="https://github.com/mayezharis/MMath_Y5_Dissertation/tree/main/Shiny%20App/md_app_final"), "."),
             ),
  )


######################################################################
######################################################################
######################### SETTING UP SERVER ##########################
######################################################################
######################################################################

server <- function(input, output, session) {

  # Calling a dataset based on the dataset selected
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
  
  # Ensuring the dataset is suitable and calling the column names
  var_names <- reactive({
    req(!is.null(data()))
    colnames(data())
  })
  
  # Taking subset of data to only include extremities of all columns
  data_minmax <- reactive({
    data() %>%
      filter(if_any(everything(), ~.x == max(.x) | .x == min(.x)))
  })
  
  
  # Updating dropdown menus as observer selects inputs
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


  # Defining a sample size based on observer input
  sample_size <- reactive({
    req(input$sample_size != "")
    as.numeric(input$sample_size)
    })
  
  
  
  ######################################################################
  #####################  GENERATING NEW SAMPLES  #######################
  #####################         BUTTONS          #######################
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
  
  # Starting a counter for the number of samples
  resample <- reactiveValues(counter = 0)
  
  # Updating counter every time one of the "Generate new sample" buttons are clicked
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
  
  # Resetting counter if any of the inputs are changed
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
  #######################  RESETTING THE APP  ##########################
  ######################################################################
  ######################################################################
  
  output$refresh  <- renderUI({
    req(resample$counter > 0)
    actionButton("refresh", "Reset all inputs")
  })
  output$refresh_rvf  <- renderUI({
    req(resample$counter > 0)
    actionButton("refresh_rvf", "Reset all inputs")
  })
  output$refresh_sl  <- renderUI({
    req(resample$counter > 0)
    actionButton("refresh_sl", "Reset all inputs")
  })
  output$refresh_qq  <- renderUI({
    req(resample$counter > 0)
    actionButton("refresh_qq", "Reset all inputs")
  })
  
  #====================================================================#
  
  observeEvent(input$refresh, {js$refresh_page();})
  
  observeEvent(input$refresh_rvf, {js$refresh_page();})
  
  observeEvent(input$refresh_sl, {js$refresh_page();})
  
  observeEvent(input$refresh_qq, {js$refresh_page();})
  
  ######################################################################
  ######################################################################
  ######################################################################
  
  # Create a linear fit for the full dataset
  mod_full <- reactive({
    lm(paste(input$y_var, "~", input$x_var), data())
  })
  
  # Find augmented data for the full data model
  aug_data_full <- reactive({
    augment(mod_full()) %>%
      mutate(sqrt_abs_std_res = sqrt(abs(.std.resid)))
  })
  
  # Subset augmented data to only include extremities  
  aug_full_minmax <- reactive({
    aug_data_full() %>%
      filter(if_any(everything(), ~.x == max(.x) | .x == min(.x)))
  })
  
  
  # Draw a sample using the given sample size
  reg_data <- reactive({
    req(input$x_var, input$y_var, input$x_var %in% names(data()))
    if (resample$counter > 0) {
      data_sample <- sample_n(data(), sample_size())
      return(data_sample[, c(input$x_var, input$y_var)])
      # return(data()[sample.int(nrow(data()), size = sample_size()), c(input$x_var, input$y_var)])
   }
  })

  # Fit a linear model on sample
  mod <- reactive({
    lm(paste(input$y_var, "~", input$x_var), reg_data())
  })
  
  # Obtain augmented data from the linear fit on the sample
  augData <- reactive({
    augment(mod()) %>%
      mutate(sqrt_abs_std_res = sqrt(abs(.std.resid)))
  })
  
  
  ######################################################################
  ######################################################################
  ########################  RVF PLOT HISTORY  ##########################
  ######################################################################
  
  # Empty dataframe to store history data
  rvf_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  # Clear dataframe when the sample counter is at 0
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      rvf_history_data$history <- data.frame(x = numeric(), y = numeric())
    }
  })
  
  # Create a skeleton for the history plot
  rvf_empty <- reactive({
    req(resample$counter > 0)
    ggplot(aug_full_minmax(), aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0) +
      geom_hline(yintercept = 0, linetype = 2, col = "black") +
      labs(title = "Residuals vs. Fitted History",
           x = "Fitted values",
           y = "Residuals")
  })
  
  # Function to draw the smoothing line from sample data
  rvf_current <- function(data) {
    data %>% ggplot(aes(x = .fitted, y = .resid)) +
      geom_point() +
      stat_smooth(formula = "y~x", method = "loess", se = FALSE, n = sample_size())
  }
  
  # Adding the current sample's line to the history dataframe
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      
      # Obtain data about the smoothing line
      rvf_new_line <- ggplot_build(rvf_current(augData()))$data[[2]][, c("x", "y")]
      
      # Group line by the sample it represents
      rvf_new_line$group <- nrow(rvf_history_data$history) / sample_size() + 1
    
      # Distinguish current sample's line from previous lines
      rvf_new_line$is_recent <- "TRUE"
      if (nrow(rvf_history_data$history) > 0) {
        rvf_history_data$history$is_recent <- "FALSE"
      }
      
      # Add current line to history data
      rvf_history_data$history <- rbind(rvf_history_data$history, rvf_new_line)
    
      # Render the history plot
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
  
  # Empty dataframe to store history data
  sl_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  # Clear dataframe when the sample counter is at 0
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      sl_history_data$history <- data.frame(x = numeric(), y = numeric())
    }
  })
  
  # Create a skeleton for the history plot
  sl_empty <- reactive({
    req(resample$counter > 0)
    ggplot(aug_full_minmax(), aes(x = .fitted, y = sqrt_abs_std_res)) +
      geom_point(alpha = 0) +
      labs(title = "Scale-Location History",
           x = "Fitted values",
           y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$"))
  })
  
  # Function to draw the smoothing line from sample data
  sl_current <- function(data) {
    data %>% ggplot(aes(x = .fitted, y = sqrt_abs_std_res)) +
      geom_point() +
      stat_smooth(formula = "y~x", method = "loess", se = FALSE, n = sample_size())
  }
  
  # Adding the current sample's line to the history dataframe
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      
      # Obtain data about the smoothing line
      sl_new_line <- ggplot_build(sl_current(augData()))$data[[2]][, c("x", "y")]
      
      # Group line by the sample it represents
      sl_new_line$group <- nrow(sl_history_data$history) / sample_size() + 1
      
      # Distinguish current sample's line from previous lines
      sl_new_line$is_recent <- "TRUE"
      if (nrow(sl_history_data$history) > 0) {
        sl_history_data$history$is_recent <- "FALSE"
      }
      
      # Add current line to history data
      sl_history_data$history <- rbind(sl_history_data$history, sl_new_line)
      
      # Render the history plot
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
  
  # Empty dataframe to store history data
  qq_history_data <- reactiveValues(history = data.frame(x = numeric(), y = numeric()))
  
  # Clear dataframe when the sample counter is at 0
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      qq_history_data$history <- data.frame(x = numeric(), y = numeric())
    }
  })
  
  # Create a skeleton for the history plot
  qq_empty <- reactive({
    req(resample$counter > 0)
    aug_data_full() %>% ggplot(aes(sample = .std.resid)) +
      stat_qq_line(linetype = 2) +
      labs(title = "History of Normal Q-Q Plot", 
           x = "N(0, 1) quantiles", 
           y = "Standardized residuals")
  })
  
  # Function to draw the smoothing line from sample data
  qq_current <- function(data) {
    data %>% ggplot(aes(sample = .std.resid)) +
      stat_qq_line() +
      stat_qq(geom = "path")
  }
  
  # Adding the current sample's line to the history dataframe
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      
      # Obtain data about the QQ line
      qq_new_line <- ggplot_build(qq_current(augData()))$data[[2]][, c("x", "y")]
      
      # Group line by the sample it represents
      qq_new_line$group <- nrow(qq_history_data$history) / sample_size() + 1
      
      # Distinguish current sample's line from previous lines
      qq_new_line$is_recent <- "TRUE"
      if (nrow(qq_history_data$history) > 0) {
        qq_history_data$history$is_recent <- "FALSE"
      }
      
      # Add current line to history data
      qq_history_data$history <- rbind(qq_history_data$history, qq_new_line)
      
      # Render the history plot
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
  
  # Empty dataframe to contain the intercepts and slopes of each line
  history_data <- reactiveValues(history = data.frame(intercept = numeric(), slope = numeric()))
  
  # Clear dataframe when the sample counter is at 0
  observeEvent(resample$counter, {
    if (resample$counter == 0) {
      history_data$history <- data.frame(intercept = numeric(), slope = numeric())
    }
  })
  
  # Obtain intercept and slope of the current linear fit
  observeEvent(resample$counter, {
    if (resample$counter > 0) {
      new_row <- data.frame(
        intercept = coef(mod())[1],
        slope = coef(mod())[2],
        
      # Distinguish current line from previous lines 
        is_recent = "TRUE"
      )
      if (nrow(history_data$history) > 0) {
        history_data$history$is_recent <- "FALSE"
      }
      
      # Add current line to history data
      history_data$history <- rbind(history_data$history, new_row)
      
      # Render the history plot
      output$fittedLineHoriginal <- 
        output$fittedLineHrvf <-
        output$fittedLineHsl <- 
        output$fittedLineHqq <- renderPlot({
          req(resample$counter > 0)
          ggplot(data = data_minmax(), mapping = aes_string(x = input$x_var, y = input$y_var)) +
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
        stat_smooth(formula = "y~x", method = "lm", se = FALSE, col="red", linewidth = 0.5)
        # geom_abline(intercept = coef(mod())[1], slope = coef(mod())[2], color = "red")
    })

  ######################################################################
  ######################################################################
  ######################################################################



  ######################################################################
  ######################################################################
  ######################  DIAGNOSTIC PLOTS  ############################
  ######################################################################
  
  # These plots are all rendered based on the switch input on the observer's side
  
  output$rvfPlot1 <- renderPlot({
    req(resample$counter > 0)
    rvfPlot <- switch(input$plot_options_rvf,
                      wRL = augData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        stat_smooth(formula = "y~x", col="red", method = "loess", se=FALSE, linewidth = 0.5, n = sample_size()) +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals"),
                      wCI = augData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        stat_smooth(formula = "y~x", col="red", method = "loess", se=!FALSE, linewidth = 0.5, n = sample_size()) +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals"),
                      woRL = augData() %>% ggplot(aes(x = .fitted, y = .resid)) +
                        geom_point(shape = 1) +
                        geom_hline(yintercept = 0, linetype = 2, color = "black") +
                        labs(title = "Residuals vs. Fitted",
                             x = "Fitted values",
                             y = "Residuals"))
    rvfPlot
    })

  output$qqPlot1 <- renderPlot({
    req(resample$counter > 0)
    qqPlot <- augData() %>%
      ggplot(aes(sample = .std.resid)) +
      stat_qq_line() +
      stat_qq() +
      labs(title = "Normal Q-Q Plot", x = "N(0, 1) quantiles", y = "Standardized residuals")
    qqPlot
  })

  output$slPlot1 <- renderPlot({
    req(resample$counter > 0)
    slPlot <- switch(input$plot_options_sl,
                     wRL = augData() %>% ggplot(aes(x = .fitted, y = sqrt_abs_std_res)) +
                       geom_point(shape = 1) +
                       stat_smooth(formula = "y~x", col="red", method = "loess", se=FALSE, linewidth = 0.5, n = sample_size()) +
                       labs(title= "Scale-Location",
                            x ="Fitted values",
                            y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                     wCI = augData() %>% ggplot(aes(x = .fitted, y = sqrt_abs_std_res)) +
                       geom_point(shape = 1) +
                       stat_smooth(formula = "y~x", col="red", method = "loess", se=!FALSE, linewidth = 0.5, n = sample_size()) +
                       labs(title= "Scale-Location",
                            x ="Fitted values",
                            y = TeX("$\\sqrt{|standardized\\,\\, residuals|}$")),
                     woRL = augData() %>% ggplot(aes(x = .fitted, y = sqrt_abs_std_res)) +
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