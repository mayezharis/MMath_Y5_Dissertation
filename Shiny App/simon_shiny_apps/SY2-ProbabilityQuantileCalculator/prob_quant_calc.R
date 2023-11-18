#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(


    titlePanel("Cumulative Probability and Quantile Value Calculator."),

    sidebarLayout(
        sidebarPanel(


            radioButtons("dist","Distribution:",c("Standard Normal" = "norm",
                                                  "Student's t" = "t",
                                                  "Chi-squared" = "chisq",
                                                  "F" = "f")),

            h4(withMathJax("\\(~~~\\)")),
            conditionalPanel(condition = "input.dist == 't'",
                             numericInput("T_df", "Degree of freedom:", value = 4, min = 1, step = 1)),
            conditionalPanel(condition = "input.dist == 'chisq'",
                             numericInput("C_df", "Degree of freedom:", value = 6, min = 1, step = 1)),
            conditionalPanel(condition = "input.dist == 'f'",
                             numericInput("F_df1", "First degree of freedom:", value = 6, min = 1, step = 1),
                             numericInput("F_df2", "Second degree of freedom:", value = 6, min = 1, step = 1)),

            h4(withMathJax("\\(~~~\\)")),
            radioButtons("fn", "Select the required function:", c("Cumulative Distribution Function" = "p", "Quantile Function, or Inverse CDF" = "q")),


            h4(withMathJax("\\(~~~\\)")),
            conditionalPanel(condition = "input.fn == 'p'",
                             numericInput("q_input", "Quantile value:",0,step = 0.1)),
            conditionalPanel(condition = "input.fn == 'q'",
                             numericInput("p_input", "Probability value:",0.5,step=0.05,min=0,max=1)),

            h4(withMathJax("\\(~~~\\)")),
            radioButtons("direction", "Upper or lower tailed probabilites?", c("Lower tail" = "lower", "Upper tail" = "upper")),


            h4(withMathJax("\\(~~~\\)")),
            hr(),
            h4(withMathJax("\\(~~~\\)")),

            h4("R code:"),
            verbatimTextOutput("rcode"),

            h4("Result:"),
            verbatimTextOutput("result"),


            width = 3

        ),


        # Show a plot of the generated distribution
        mainPanel(
                             plotOutput("Plots", height = "1000px")

        )
    )
)



server <- function(input, output, session) {

    output$rcode <- renderText({
        paste0(input$fn,input$dist,"(",
                 switch(input$fn, "q" = paste0("p = ",input$p_input), "p" = paste0("q = ",input$q_input)),
                 switch(input$dist,
                        "norm" = "",
                        "t" = paste0(", df = ",input$T_df),
                        "chisq" = paste0(", df = ",input$C_df),
                        "f" = paste0(", df1 = ", input$F_df1,", df2 = ",input$F_df2)),
                 switch(input$direction, "lower" = "", "upper" = ", lower.tail = FALSE"),
                 ")")
    })

    output$result <- renderText({
        txt <- paste0(input$fn,input$dist,"(",
               switch(input$fn, "q" = paste0("p = ",input$p_input), "p" = paste0("q = ",input$q_input)),
               switch(input$dist,
                      "norm" = "",
                      "t" = paste0(", df = ",input$T_df),
                      "chisq" = paste0(", df = ",input$C_df),
                      "f" = paste0(", df1 = ", input$F_df1,", df2 = ",input$F_df2)),
               switch(input$direction, "lower" = "", "upper" = ", lower.tail = FALSE"),
               ")")
        suppressWarnings(eval(parse(text = txt)))

    })


    output$Plots <- renderPlot({

        if(input$dist == "norm"){
            pdf <- function(x){dnorm(x)}
            cdf <- function(x){pnorm(x)}
            icdf <- function(x){qnorm(x)}
        }else if(input$dist == "t"){
            validate(need(input$T_df, "Missing degree of freedom parameter for the Student t-distribution"))
            if(input$T_df <= 0) validate("Degree of freedom parameter for the Student t-distribution must be a positive number")
            pdf <- function(x){dt(x,df=input$T_df)}
            cdf <- function(x){pt(x,df=input$T_df)}
            icdf <- function(x){qt(x,df=input$T_df)}
        }else if(input$dist == "chisq"){
            validate(need(input$C_df, "Missing degree of freedom parameter for the Chi-squared distribution"))
            if(input$C_df <= 0) validate("Degree of freedom parameter for the Chi-squared distribution must be a positive number")
            pdf <- function(x){pmin(dchisq(x,df=input$C_df),100)}
            cdf <- function(x){pchisq(x,df=input$C_df)}
            icdf <- function(x){qchisq(x,df=input$C_df)}
        }else{
            validate(need(input$F_df1, "Missing first degree of freedom parameter for the F-distribution"))
            if(input$F_df1 <= 0) validate("First degree of freedom parameter for the F-distribution must be a positive number")
            validate(need(input$F_df2, "Missing second degree of freedom parameter for the F-distribution"))
            if(input$F_df2 <= 0) validate("Second degree of freedom parameter for the F-distribution must be a positive number")
            pdf <- function(x){pmin(df(x,df1=input$F_df1,df2=input$F_df2),100)}
            cdf <- function(x){pf(x,df1=input$F_df1,df2=input$F_df2)}
            icdf <- function(x){qf(x,df1=input$F_df1,df2=input$F_df2)}
        }

        ylim_cdf <- c(0,1)
        if(input$dist %in% c("norm", "t")){
            ylim_pdf <- c(0,0.4)#c(0,pdf(0))
            xlim <- c(-6, 6)
            #ff <- function(x){pdf(icdf(x))/ylim_pdf[2] - 0.001}
            #plow <- 0.5;  while(ff(plow)>0){plow <- plow/10}
            #pupp <- 0.5;  while(ff(pupp)>0){pupp <- 0.9 + pupp/10}
            #xlim <- c(floor(icdf(plow)), ceiling(icdf(pupp)))
        }else{
            if(input$dist == "chisq"){
                ymax <- min(pdf(max(input$C_df-2,0)),20)
            }else if(input$dist == "f"){
                v <- max(0, (input$F_df1-2)*input$F_df2/(input$F_df1*(input$F_df2+2)))
                ymax <- min(20, pdf(v))
            }
            ylim_pdf <- c(0, ymax)
            ff <- function(x){pdf(icdf(x))/ylim_pdf[2] - 0.001}
            pupp <- 0.5;  while(ff(pupp)>0){pupp <- 0.9 + pupp/10}
            xlim <- c(0, ceiling(icdf(pupp)))
        }

        layout(cbind(1,2))
        curve(expr = cdf, from=xlim[1], to=xlim[2], ylim=ylim_cdf, n=1001, xlim=xlim, main="Cumulative Distribution Function",
              xlab = "Value", ylab = "Probability", lwd=2, cex.main = 2, cex.lab = 1.5)
        if(input$fn=="p"){
            xval <- input$q_input
            abline(v = xval)
            abline(h = cdf(xval))
        }else if(input$fn == "q" & input$direction == "lower"){
            xval <- icdf(input$p_input)
            abline(v = xval)
            abline(h = input$p_input)
        }else if(input$fn == "q" & input$direction == "upper"){
            xval <- icdf(1-input$p_input)
            abline(v = xval)
            abline(h = 1-input$p_input)
        }
        values <- curve(expr = pdf, from=xlim[1], to=xlim[2], n=1001, ylim=ylim_pdf, xlim=xlim, main="Probabiliy Density Function",
                        xlab = "Value", ylab = "Density", lwd=2, cex.main = 2, cex.lab = 1.5)
        if(xval >= xlim[1] & xval <= xlim[2]){
            if(input$direction == "lower"){
                L <- values$x <= xval
                xpoly <- c(xlim[1],values$x[L],xval,xval)
                ypoly <- c(0,values$y[L],pdf(xval),0)
            }else if(input$direction == "upper"){
                L <- values$x >= xval
                xpoly <- c(xval,xval, values$x[L], xlim[2])
                ypoly <- c(0, pdf(xval), values$y[L], 0)
            }
            polygon(xpoly,ypoly, density = 10)
        }





    })



}

shinyApp(ui, server)
