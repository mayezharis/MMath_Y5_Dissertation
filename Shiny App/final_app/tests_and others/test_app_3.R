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
  
  
  # CHOOSING DATASET BASED ON INPUT
  # data <- read.csv(glue("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/mmath_dissertation/Shiny App/Datasets/{input$DataSet}.csv"))
  
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
  

  observeEvent(input$resample, {
    tryCatch({
      new_row <- data.frame(
        intercept = coef(mod())[1],
        slope = coef(mod())[2]
      )
      history_data$history <- rbind(history_data$history, new_row)
      # Update the plot
      output$fittedLine2 <- renderPlotly({
        ggplot(data = history_data$history,
               aes(x = intercept, y = slope)) +
          geom_line() +
          labs(title = "History of Linear Fits") + 
          theme_minimal() +
          theme(legend.position = "none")
        })
      }, error = function(e) {
        print(paste("Error:", e$message))
      })
    })
  

  # plotData <- reactive({
  #   reactiveValues(regdata = reg_data(),
  #                  lines = data.frame(a = coef(mod())[1], b = coef(mod())[2]),
  #                  plot = ggplot(data = data.frame(x = Inf, y = Inf),
  #                                mapping = aes(input$x_var, input$y_var)) +
  #                    geom_point() +
  #                    theme_bw() +
  #                    labs(title = "History")
  #                  )})
  
  
  
  #######################################################################
  #######################################################################
  # lines <- reactive({data.frame(a = coef(mod())[1], b = coef(mod())[2])})
  #######################################################################
  #######################################################################
  
  
  
  # plotData <- reactive({
  #   reg_data() %>%
  #     ggplot(data = data.frame(),
  #            mapping = aes_string(x = input$x_var, y = input$y_var)) #+
  #     # labs(title = "History")
  #   })
  # 

  # output$fittedEqn <- renderTable({
  #   mod() %>%
  #     tidy() %>%
  #     select(term, estimate)
  # })
  
  
  
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
  
  
  
  
  
  # observeEvent(input$resample, {
  #   plotData()$plot <- plotData()$plot +
  #     geom_abline(intercept = plotData()$lines$a[1],
  #                 slope = plotData()$lines$b[1],
  #                 col = "grey60", linewidth = 0.75)
  #   plotData()$regdata <- reg_data()
  #   plotData()$lines <- rbind(coef(mod()), plotData()$lines) 
  # })
  
  # values <- reactiveValues(plot = ggplot())

  
  ##########################################################
  ##########################################################
  # output$fittedLine2 <- renderPlot({
  #   # input$x_var; input$y_var; input$resample
  #   reg_data() %>%
  #     ggplot(aes_string(x = input$x_var, y = input$y_var)) +
  #     geom_abline(intercept = lines()$a[1],
  #                 slope = lines()$b[1],
  #                 col = "cornflowerblue", linewidth = 0.75)
  #   # values$plot <- p
  #   })
  ##########################################################
  ##########################################################
  
  
  
  
  #########################################################
  ##########################################################
  # lineUpdate <- reactive({
  #   if(input$resample >= 0){
  #     rbind(coef(mod()), lines()) 
  #     }
  #   })
  #########################################################
  ##########################################################
  
  
  
  
  # observeEvent(values$plot,{ 
  #   output$fittedLine2 <- renderPlot({
  #     isolate(values$plot)
  #   })
  # })

  
  
   
  ########################################################
  ########################################################
  # output$fittedLine2 <- reactive({
  #   if(input$resample > 0) {
  #   output$fittedLine2 +
  #     geom_abline(intercept = lineUpdate()$a[1],
  #                 slope = lineUpdate()$b[1],
  #                 col = "seagreen", linewidth = 1)}
  #   })
  ########################################################
  ########################################################
  
  
  
  
  
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