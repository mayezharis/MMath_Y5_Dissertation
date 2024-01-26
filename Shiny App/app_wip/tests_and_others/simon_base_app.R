#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dataset_choices <- c("NBA Player Data" = "nba_data_cleaned",
                     "Cricket Batting Data" = "cric_bat_data",
                     "MLB Game Data" = "mlb_data_cleaned",
                     "Energy Consumption Data" = "energy_data_cleaned",
                     "Article Popularity Data" = "news_data_cleaned")


data_all <- list(
  nba_data_cleaned    = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/nba_data_cleaned.csv"),
  cric_bat_data       = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/cric_bat_data.csv"),
  mlb_data_cleaned    = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/mlb_data_cleaned.csv"),
  energy_data_cleaned = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/energy_data_cleaned.csv"),
  news_data_cleaned   = read.csv("C:/Users/mayez/OneDrive - University of Edinburgh/Year 5/Dissertation/Code/MMath_Y5_Dissertation/Shiny App/Datasets/final_cleaned_datasets/news_data_cleaned.csv")
)

#data <- data_all[["nba_data_cleaned"]]
#vars <- colnames(data)

# Define UI for application that draws a histogram

do_lm <- function(data,xvar,yvar,id){
  tmp_data <- data.frame(x = data[id,xvar], y = data[id,yvar])
  return(lm(y~x, data = tmp_data))
}

do_smooth <- function(x,y){
  #ok <- is.finite(x) & is.finite(y)
  #if (any(ok)) 
  #  lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
  stats::lowess(x, y, f = 2/3, iter = 3)
}


ui <- 
  fluidPage(
    h1("Model Diagnostics"),
    sidebarLayout(
      sidebarPanel(

        selectInput('dataset', 'Choose Dataset', dataset_choices),
        
        #See https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
        uiOutput('xcolumns'),        
        uiOutput('ycolumns'),
        
#        selectInput(inputId = "DataSet",
#                    label = "Choose dataset:",
#                    choices = dataset_choices),
#        selectInput(inputId = "y_var",
#                    label = "Y Variable:",
#                    choices = vars),  # drop-down for y variable
#        
#        selectInput(inputId = "x_var",
#                    label = "X Variable:",
#                    choices = vars),  # drop-down for x-variable
        
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  info <- reactiveValues(
    mydata = data_all[[1]],
    xcol = names(data_all[[1]])[1],
    ycol = names(data_all[[1]])[1],
    id = 1:250,
    fit = do_lm(data_all[[1]], names(data_all[[1]])[1], names(data_all[[1]])[1], 1:250),
    history_lines = list(intercept = NULL, slope = NULL)
#    history_resVfit = list(???)   ## do_smooth
#    history_SL = list(???)        ## do_smooth
#    history_qq = list(???)        
              ## tmp <- qqnorm(rnorm(20), plot=FALSE)
              ## sampleQ <- tmp$y[order(tmp$x)]  ##<< Store only this across history
              ## theoryQ <- qnorm((seq_len(input$sample_size)-0.5)/input$sample_size)
              ## plot(theoryQ, sampleQ)


  )
  
  
  observeEvent(input$dataset,{
    info$mydata = data_all[[input$dataset]]
    info$xcol = names(info$mydata)[1]    #Need to specify to avoid render warning message for plots as otherwise old col names do not correspond to new data.
    info$ycol = info$xcol
    info$fit = do_lm(info$mydata, info$xcol, info$ycol, info$id)
    info$history$intercept = NULL
    info$history$slope = NULL
  })
  
  output$xcolumns = renderUI({
    selectInput('column_x', 'X Variable', names(info$mydata))
  })
  
  output$ycolumns = renderUI({
    selectInput('column_y', 'Y Variable', names(info$mydata))
  })
  
  observeEvent(input$column_x,{
    info$xcol = input$column_x
    info$fit = do_lm(info$mydata, info$xcol, info$ycol, info$id)
    info$history$intercept = NULL
    info$history$slope = NULL
  })
  
  observeEvent(input$column_y,{
    info$ycol = input$column_y
    info$fit = do_lm(info$mydata, info$xcol, info$ycol, info$id)
    info$history$intercept = NULL
    info$history$slope = NULL
  })
  
  observeEvent(input$sample_size,{
    info$id = sample(nrow(info$mydata), size = input$sample_size, replace = FALSE)
    info$fit = do_lm(info$mydata, info$xcol, info$ycol, info$id)
    info$history$intercept = NULL
    info$history$slope = NULL
  })
  
  observeEvent(input$resample,{
    #store old values
    info$history$intercept = c(coef(info$fit)[1], info$history$intercept)
    info$history$slope = c(coef(info$fit)[2], info$history$slope)

    info$id = sample(nrow(info$mydata), size = input$sample_size, replace = FALSE)
    info$fit = do_lm(info$mydata, info$xcol, info$ycol, info$id)
  })

  output$fittedLine1 <- renderPlot({
    plot(info$mydata[info$id,info$xcol], 
         info$mydata[info$id,info$ycol],
         xlab = info$xcol, xlim=range(info$mydata[,info$xcol]),
         ylab = info$ycol, ylim=range(info$mydata[,info$ycol]))
    abline(a = coef(info$fit)[1], b = coef(info$fit)[2], col = "red")
    })
  
  output$origPlot1 <- renderPlot({
    if(input$plot_type == "resid.fitted"){
      plot(info$fit, which = 1, add.smooth = (input$plot_options != "woRL"))
    }else if(input$plot_type == "scale.location"){
      plot(info$fit, which = 3, add.smooth = (input$plot_options != "woRL"))
    }else{
      plot(info$fit, which = 2)
    }
  })
  
  output$fittedLine2 <- renderPlot({
    plot(info$mydata[info$id,info$xcol], 
         info$mydata[info$id,info$ycol],
         xlab = info$xcol, xlim=range(info$mydata[,info$xcol]),
         ylab = info$ycol, ylim=range(info$mydata[,info$ycol]),
         type = "n")
    for(i in seq_along(info$history$intercept)){
        abline(a = info$history$intercept[i], 
               b = info$history$slope[i], col = "grey60")
    }
    abline(a = coef(info$fit)[1], b = coef(info$fit)[2], col = "red")
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
