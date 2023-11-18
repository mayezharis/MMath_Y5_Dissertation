#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Published: https://uoe-maths.shinyapps.io/SY2-EstimatorTheory/


library(shiny)

data <- read.csv("Shiny App/diag_plots/share_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel(withMathJax("Diagnostic Plots.")),

    h4(withMathJax("This app illustrates the principal of repeated sampling and unbiaseness of the sample mean estimator, 
                   \\(\\bar{X}\\), for the population expectation parameter \\(\\mu\\). In reality, we can only ever have a 
                   single sample from the population (represented in green) and the calculated sample mean estimate, 
                   \\(\\bar{x}\\). However, the single sample is only one of many possible datasets obtained from the population 
                   distribution, each with their own sample mean estimate. We can examine the sampling distribution for 
                   \\(\\bar{X}\\) through simulations for different population distributioins and sample sizes, and assess how 
                   good the sample mean is for inferring \\(\\mu\\).")),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            
            selectInput("y_var", "Y Variable:", c("Age" = "age",
                                                  "BMI" = "bmi",
                                                  "Gender" = "female",
                                                  "Uniform, U(0,1)" = "U")),
            
            selectInput("n", "Sample Size:", c("n=10" = "small",
                                              "n=100" = "med",
                                              "n=1000" = "large")),

            #selectInput("alpha", "Confidence Interval:", c("95%" = "0.05",
            #                                   "90%" = "0.1",
            #                                   "80%" = "0.2")),
            
            radioButtons("clt", "Draw the CLT Distribution?",
                         c("No" = "N","Yes" = "Y")),
            
            
            actionButton("run", "Re-run"),
            
            h3("Key:"),
            h4("Green - Available information."),
            h4("Black - Typically unavailable."),
            
            width = 3
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plots", height = "1000px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    r <- reactiveValues(seed = as.numeric(Sys.time()))
    
    observeEvent(input$run, {
        r$seed = as.numeric(Sys.time())
    })
    
    output$Plots <- renderPlot({
        set.seed(r$seed)
        
        CI <- "alpha" %in% names(input)
        if(CI){
          layout(rbind(1,c(2,2,3,3,4,4,5),6,7))
        }else{
          layout(rbind(1,c(2,2,3,3,4,4,5),6))
        }
        
        ###Population plot
        if(input$Dist == "N"){
            x <- seq(-4,4,len=1000)
            y <- dnorm(x)
            ylim=c(-0.2,1)*max(y)
            plot(x,y,type="l",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",main="Population Distribution",
                 ylim=ylim,cex.main=2)
            polygon(x,y,density=10,col=1,border=1)
            abline(h=0)
            segments(0,0,0,.Machine$integer.max,col=1,lwd=2)
            text(x = 0, y=0.5*ylim[1], label = expression(mu),cex=2)
            text(-3,0.5*ylim[1],"-3",cex=2)
            text(3,0.5*ylim[1],"3",cex=2)
        }else if(input$Dist == "E"){
            x <- seq(0,6,len=1000)
            y <- dexp(x)
            ylim=c(-0.2,1)*max(y)
            plot(x,y,type="l",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",main="Population Distribution",
                 ylim=ylim,cex.main=2)
            polygon(c(0,x,max(x)),c(0,y,0),density=10,col=1,border=1)
            segments(0,0,0,.Machine$integer.max)
            segments(0,0,.Machine$integer.max,0)
            segments(1,0,1,.Machine$integer.max,col=1,lwd=2)
            text(x = 1, y=0.5*ylim[1], label = expression(mu),cex=2)
            text(0,0.5*ylim[1],"0",cex=2)
            text(max(x),0.5*ylim[1],max(x),cex=2)
        }else if(input$Dist == "B"){
            x <- 0:10
            y <- dbinom(x, 10, 1/3)
            ylim=c(-0.2,1)*max(y)
            plot(x,y,type="h",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",main="Population Distribution",
                 ylim=ylim,col=1,cex.main=2)
            points(x,y,pch=16,col=2)
            abline(h=0)
            text(0,0.5*ylim[1],"0",cex=2)
            text(10,0.5*ylim[1],"10",cex=2)
            segments(10*1/3,0,10*1/3,.Machine$integer.max,col=1,lwd=2)
            text(x = 10*1/3, y=0.5*ylim[1], label = expression(mu),cex=2)
        }else if(input$Dist == "U"){
            x <- seq(-0.5,1.5,len=1000)
            y <- dunif(x)
            ylim=c(-0.2,1)*max(y)
            plot(x,y,type="l",xaxt="n",yaxt="n",frame=FALSE,xlab="",ylab="",main="Population Distribution",
                 ylim=ylim,cex.main=2)
            polygon(x,y,density=10,col=1,border=1)
            abline(h=0)
            segments(0.5,0,0,.Machine$integer.max,col=1,lwd=2)
            text(x = 0.5, y=0.5*ylim[1], label = expression(mu),cex=2)
            text(0,0.5*ylim[1],"0",cex=2)
            text(1,0.5*ylim[1],"1",cex=2)
        }
        
        ##Sample plot
        n <- switch(input$n,
                    "small" = 10, 
                    "med" = 100, 
                    "large" = 1000)
        f <- switch(input$Dist,
                    "N" = function(n){rnorm(n)}, 
                    "B" = function(n){rbinom(n,size=10,prob=1/3)}, 
                    "E" = function(n){rexp(n)},
                    "U" = function(n){runif(n)})
        ftrim <- function(f,n,xlim){
            x <- f(n)
            x <- x[x>=xlim[1] & x <= xlim[2]]
            while(length(x)<n){
                x <- c(x,f(n-length(x)))
                x <- x[x>xlim[1] & x < xlim[2]]
            }
            return(x)
        }
        xlim <- switch(input$Dist,
                       "N" = c(-4,4), 
                       "B" = c(0,10), 
                       "E" = c(0,6),
                       "U" = c(0,1))
        xbar_store <- NULL
        
        for(i in 1:3){
            x <- ftrim(f,n,xlim)
            xbar_store <- c(xbar_store,mean(x))
            if(input$Dist=="B"){
              h <- hist(x,plot=FALSE)
              h$mids <- 0:10
              h$breaks <- seq(-0.5,10.5,by=1)
              h$counts <- as.numeric(table(c(x,0:10))-1)
              h$density <- h$counts/sum(diff(h$breaks)*h$counts)
            }else{
              h <- hist(x,plot=FALSE)  
            }
            plot(h, main = paste0("Sample ",i),xlim=xlim,ylim=c(0,max(h$counts)*1.1),density=10,col=1+2*(i==1),xlab="",cex.main=2,cex.lab=1.5)
            segments(mean(x),0,mean(x),.Machine$integer.max,lwd=2,col=1+2*(i==1))
            text(x = mean(x)+0.04*diff(xlim), y=max(h$counts)*1.1, label = expression(bar(x)),cex = 2)
        }
        plot(-1:1,rep(0,3),pch=16,cex=2,xlab="",ylab="",frame=FALSE,xaxt="n",yaxt="n",xlim=c(-2,2))
        
        #Estimator
        while(length(xbar_store) < 1000){
            #x <- ftrim(f,n,xlim)
            xbar_store <- c(xbar_store,mean(f(n)))
        }
        mu <- switch(input$Dist,
                     "N" = 0, 
                     "B" = 10*1/3, 
                     "E" = 1,
                     "U" = 0.5)
        h <- hist(xbar_store,plot=FALSE)
        xlim <- switch(input$Dist,
                       "N" = 0+c(-4,4)/sqrt(10), 
                       "B" = 10*1/3 + c(-4,4)*sqrt(2)/3, 
                       "E" = 1+c(-4,4)/sqrt(10),
                       "U" = 0.5+c(-4,4)/sqrt(120))
        
        plot(h, main = "Sampling Distribution", ylim=c(0,max(h$counts)*1.1),density=10,col=1,xlab="",xlim=xlim,cex.main=2,cex.lab=1.5)
        segments(xbar_store[1],0,xbar_store[1],.Machine$integer.max,lwd=2,col=3)
        segments(mu,0,mu,.Machine$integer.max,lwd=2,col=1)
        if(mu>xbar_store[1]){shift <- -1}else{shift <- +1}
        text(x = xbar_store[1]+shift*0.01*diff(xlim), y=max(h$counts)*1.1, label = expression(bar(x)),cex = 2)
        text(x = mu-shift*0.01*diff(xlim), y=max(h$counts)*1.1, label = expression(mu),cex = 2)
        
        if(input$clt == "Y"){
            a <- seq(xlim[1],xlim[2],len=1000)
            s <- switch(input$Dist,
                        "N" = 1/sqrt(n), 
                        "B" = sqrt(mu*(10-mu)/(10*n)), 
                        "E" = mu/sqrt(n),
                        "U" = 1/sqrt(12*n))
            scale <- h$counts[1]/h$density[1]
            lines(a,dnorm(a,mu,s)*scale,lty=1,col=1,lwd=2)
        }
        
        
        ##CI
        if(CI){
          maxidx <- 40#length(xbar_store)
          IDX <- 1:maxidx
          se_store <- switch(input$Dist,
                      "N" = rep(1/sqrt(n),length(xbar_store)), 
                      "B" = sqrt(xbar_store*(10-xbar_store)/(10*n)), 
                      "E" = xbar_store/sqrt(n),
                      "U" = rep(1/sqrt(12*n),length(xbar_store)))
          alpha <- as.numeric(input$alpha)
          LOW <- xbar_store[IDX] - qnorm(1-alpha/2)*se_store[IDX]
          UPP <- xbar_store[IDX] + qnorm(1-alpha/2)*se_store[IDX]
          pch <- 1 + 15*as.numeric(mu > LOW & mu < UPP)
          rng <- max(c(abs(LOW-mu),abs(UPP-mu)))
          ylim <- mu+c(-1,1)*rng
          plot(IDX,xbar_store[IDX],xlab="",ylab="",xaxt="n",ylim = ylim,
               frame=FALSE,pch=pch,cex=3,col=c(3,rep(1,length(IDX)-1)), 
               main = "Confidence Intervals",cex.main=2)
          segments(IDX,LOW,IDX,UPP,col=c(3,rep(1,length(IDX)-1)))
          abline(h=mu, col = 1)
          text(0,mu+0.1*rng,expression(mu),cex=2)
        }  
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
