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
    
    
    titlePanel("Illustration of Likelihood-Based Statistical Inference."),
    
    sidebarLayout(
        sidebarPanel(
            
            h4("1) Select a population distribution:"),
            selectInput("Dist", "Population Distribution:", c("Bernoulli" = "B",
                                                      "Geometric" = "G",
                                                      "Uniform" = "U",
                                                      "Normal" = "N")),
            h4("2) Select the true value of the population parameter:"),
            conditionalPanel(condition = "input.Dist == 'B'",
                      sliderInput("thetaB", withMathJax("Population probability, \\(p\\)"), 0.01, 0.99, 0.5)),
            conditionalPanel(condition = "input.Dist == 'G'",
                     sliderInput("thetaG", withMathJax("Population probability, \\(p\\)"), 0.01, 0.99, 0.5)),
            conditionalPanel(condition = "input.Dist == 'U'",
                     sliderInput("thetaU", withMathJax("Population upper bound, \\(\\theta\\)"), 0.1, 2.0, 1.0)),
            conditionalPanel(condition = "input.Dist == 'N'",
                     sliderInput("thetaN", withMathJax("Population expectation, \\(\\mu\\)"), -1, 1, 0, step = 0.1),
                     sliderInput("sig2", withMathJax("Population variance, \\(\\sigma^2\\)"), 0.5, 1, 1)),
    
            h4("3) Select a sample size:"),
            selectInput("n", "Sample Size:", c("n=25" = "small",
                                               "n=100" = "med",
                                               "n=400" = "large")),
            
            h4(withMathJax("4) Click on 'Generate' to simulate \\(n=\\)"),textOutput("n",inline=TRUE),"independent samples from the population distribution:"),
            conditionalPanel(condition = "input.Dist == 'B'",
                             h4(withMathJax("\\(X\\sim\\text{Bernoulli}(p =\\)"),textOutput("thetaB",inline=TRUE),withMathJax("\\()\\)."))),
            conditionalPanel(condition = "input.Dist == 'G'",
                             h4(withMathJax("\\(X\\sim\\text{Geometric}(p = \\)"),textOutput("thetaG",inline=TRUE),withMathJax("\\()\\)."))),
            conditionalPanel(condition = "input.Dist == 'U'",
                             h4(withMathJax("\\(X\\sim\\text{Uniform}(0, \\theta = \\)"),textOutput("thetaU",inline=TRUE),withMathJax("\\()\\)."))),
            conditionalPanel(condition = "input.Dist == 'N'",
                             h4(withMathJax("\\(X\\sim\\text{Normal}(\\mu=\\)"),textOutput("thetaN",inline=TRUE),withMathJax("\\(, \\sigma^2=\\)"),textOutput("sig2",inline=TRUE),withMathJax("\\()\\)."))),

            actionButton("run", "Generate"),
            
            h4(withMathJax("\\(~~\\)")),
            
            h4("5) Use the slider(s) below to see how the blue probability mass/density function changes for different parameter values. Can you find a good match between the blue pm(d)f and the sample?"),            
            conditionalPanel(condition = "input.Dist == 'B'",
                             sliderInput("l_thetaB", withMathJax("Population probability, \\(p\\)"), 0, 1, 0.5)),
            conditionalPanel(condition = "input.Dist == 'G'",
                             sliderInput("l_thetaG", withMathJax("Population probability, \\(p\\)"), 0, 1, 0.5)),
            conditionalPanel(condition = "input.Dist == 'U'",
                             sliderInput("l_thetaU", withMathJax("Population upper bound, \\(\\theta\\)"), 0.05, 2.1, 1)),
            conditionalPanel(condition = "input.Dist == 'N'",
                             sliderInput("l_thetaN", withMathJax("Population expectation, \\(\\mu\\)"), -2, 2, 0, step = 0.1),
                             sliderInput("l_sig2", withMathJax("Population variance, \\(\\sigma^2\\)"), 0.5, 1.5, 1)),
            
            h4(withMathJax("\\(~~\\)")),

            h4("6) The calculated maximum likelihood estimate(s) for the generated sample are presented below. They occur where the likelihood function is maximised. Move the above slider(s) close to these values."),
            conditionalPanel(condition = "input.Dist == 'B'", withMathJax("\\(\\hat{p} = \\)"),textOutput("mleB",inline=TRUE)),
            conditionalPanel(condition = "input.Dist == 'G'", withMathJax("\\(\\hat{p} = \\)"),textOutput("mleG",inline=TRUE)),
            conditionalPanel(condition = "input.Dist == 'U'", withMathJax("\\(\\hat{\\theta} = \\)"),textOutput("mleU",inline=TRUE)),
            conditionalPanel(condition = "input.Dist == 'N'", withMathJax("\\(\\hat{\\mu} = \\)"),textOutput("mleN1",inline=TRUE), withMathJax("\\(,~~~\\hat{\\sigma}^2 = \\)"),textOutput("mleN2",inline=TRUE)),
            
            width = 3
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("Plots", height = "1000px")
        )
    )
)



server <- function(input, output, session) {
    
    r <- reactiveValues(seed = as.numeric(Sys.time()))
    
    observeEvent(input$run, {
        r$seed = as.numeric(Sys.time())
    })
    
    
    output$n <- renderText({switch(input$n,
                                   "small" = 25, 
                                   "med" = 100, 
                                   "large" = 400)})
    output$thetaB <- renderText({input$thetaB})
    output$thetaG <- renderText({input$thetaG})
    output$thetaU <- renderText({input$thetaU})
    output$thetaN <- renderText({input$thetaN})
    output$sig2 <- renderText({input$sig2})

    output$Plots <- renderPlot({
        set.seed(r$seed)
        
        n <- switch(input$n,
                    "small" = 25, 
                    "med" = 100, 
                    "large" = 400)
        #output$n <- n
        layout(cbind(1,2))
        if(input$Dist == "B"){
            x_sample <- rbinom(n, size = 1, prob = input$thetaB)
            t <- table(c(x_sample,0:1))-1
            y2 <- dbinom(0:1,size=1,prob = input$l_thetaB)*n
            b <- barplot(t,ylim=c(0,n),xlab = "x",ylab = "Frequency",main = "Sample", 
                         cex.main = 2, cex.lab = 1.5)
            points(b[,1], y2, pch = 16, cex = 2, col = 4)
            segments(b[,1], rep(0,length(y2)), b[,1], y2, lwd = 2, col = 4)
            
            theta_seq = seq(0,1,len = 1001)
            llike <- sapply(theta_seq,function(p){(sum(dbinom(x_sample,size=1,prob = p,log = TRUE)))})
            plot(theta_seq, exp(llike-max(llike)), type = "l", lwd = 2, xlab = "p",yaxt="n", 
                 ylab = "Likelihood", cex.main = 2, cex.lab = 1.5,
                 main = expression(paste("Likelihood function, L(p;",bold(x),")")))
            abline(v = input$l_thetaB,lwd=2, col = 4)
            
            output$mleB <- renderText({mean(x_sample)})
        }else if(input$Dist == "G"){
            x_sample <- rgeom(n, prob = input$thetaG)+1
            xmax <- max(qgeom(0.999,input$thetaG),6)+1
            while(any(x_sample > xmax)){
                x_sample <- x_sample[x_sample <= xmax]
                x_sample <- c(x_sample,1+rgeom(n-length(x_sample), prob = input$thetaG))
            }
            t <- table(c(x_sample,1:xmax))-1
            y2 <- dgeom(as.numeric(names(t))-1,prob = max(0.001,input$l_thetaG))*n
            b <- barplot(t,ylim=c(0,n),xlab = "x",ylab = "Frequency",main = "Sample", 
                         cex.main = 2, cex.lab = 1.5, col = "grey75")
            points(b[,1], y2, pch = 16, cex = 2, col = 4)
            segments(b[,1], rep(0,length(y2)), b[,1], y2, lwd = 2, col = 4)
            
            theta_seq = seq(0.001,0.999,len = 1001)
            llike <- sapply(theta_seq,function(p){(sum(dgeom(x_sample-1,prob = p,log = TRUE)))})
            plot(theta_seq, exp(llike-max(llike)), type = "l", lwd = 2, xlab = "p", yaxt="n",
                 ylab = "Likelihood", cex.main = 2, cex.lab = 1.5,
                 main = expression(paste("Likelihood function, L(p;",bold(x),")")))
            abline(v = input$l_thetaG,lwd=2, col = 4)
            output$mleG <- renderText({1/(mean(x_sample))})
        }else if(input$Dist == "U"){
            x_sample <- runif(n,  min = 0, max = input$thetaU)
            xlim <- c(0,2)
            h <- hist(x_sample,plot=FALSE)
            x2 <- seq(-0.1,2.1,len = 1001)
            y2 <- dunif(x2,min=0,max=max(0.001,input$l_thetaU))*h$count[1]/h$density[1]
            ylim <- c(0,max(max(h$counts,na.rm=TRUE),max(y2)))
            plot(h,ylim=ylim,xlab = "x",ylab = "Frequency",main = "Sample", 
                         cex.main = 2, cex.lab = 1.5, xlim = xlim, col = "grey75")
            lines(x2,y2,lwd = 2, col = 4)
            
            
            theta_seq = input$thetaU + seq(-1,2,len = 1001)/10
            llike <- sapply(theta_seq,function(p){
                if(p <= 0){
                    return(-Inf)
                }else{
                  sum(dunif(x_sample,min = 0, max = p,log = TRUE))
                }})
            plot(theta_seq, exp(llike-max(llike)), type = "l", lwd = 2, xlab = expression(theta), 
                 ylab = "Likelihood", cex.main = 2, cex.lab = 1.5, yaxt="n",
                 main = expression(paste("Likelihood function, L(",theta,";",bold(x),")")))
            abline(v = input$l_thetaU,lwd=2, col = 4)
            output$mleU <- renderText({max(x_sample)})
        }else if(input$Dist == "N"){
            x_sample <- rnorm(n,  mean = input$thetaN,sd = sqrt(input$sig2))
            while(abs(mean(x_sample))>1.5 | var(x_sample)*(n-1)/n > 1.5){
                x_sample <- rnorm(n,  mean = input$thetaN,sd = sqrt(input$sig2))
            }
            xlim <- range(c(x_sample,-3,3))
            h <- hist(x_sample,plot=FALSE)
            x2 <- seq(xlim[1],xlim[2],len = 1001)
            y2 <- dnorm(x2,mean = input$l_thetaN,sd=sqrt(input$l_sig2))*h$count[1]/h$density[1]
            ylim <- c(0,max(max(h$counts,na.rm=TRUE),max(y2)))
            plot(h,ylim=ylim,xlab = "x",ylab = "Frequency",main = "Sample", 
                 cex.main = 2, cex.lab = 1.5, xlim = xlim, col = "grey75")
            lines(x2,y2,lwd = 2, col = 4)

            theta_seq = seq(-1.5,1.5,len = 101)
            sig2_seq = seq(0.2,1.5,len = 101)
            llike <- matrix(NA,nrow = length(theta_seq), ncol=length(sig2_seq))
            for(i in seq_along(theta_seq)){
                for(j in seq_along(sig2_seq)){
                    llike[i,j] <- sum(dnorm(x_sample,mean=theta_seq[i],sd=sqrt(sig2_seq[j]),log=TRUE))
                }
            }
            contour(theta_seq, sig2_seq, exp(llike-max(llike)), xlab = expression(mu), drawlabels = FALSE,
                 ylab = "", cex.main = 2, cex.lab = 1.5,
                 main = expression(paste("Likelihood function, L(",mu,",",sigma^2,";",bold(x),")")))
            mtext(expression(sigma^2),side=2,line=2.5,cex=1.5)
            points(mean(x_sample),var(x_sample)*(n-1)/n,pch=4)
            points(input$l_thetaN,input$l_sig2,pch=16, col = 4, cex = 2)
            output$mleN1 <- renderText({mean(x_sample)})
            output$mleN2 <- renderText({var(x_sample)*(n-1)/n})
            
        }else{
            stop("Option not yet implemented")
        }
        
    })
    
    

}

shinyApp(ui, server)
