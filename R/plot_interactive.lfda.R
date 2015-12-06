#' Interactive Plotting for Exploratory Data Analysis of Longtiudinal Functional Data
#' 
#' Produces an interactive, descriptive plot for longitudinally-observed functioanl data.
#' 
#' @param Y a matrix of which each row corresponds to one curve observed on a regular and dense grid 
#'          (dimension of N by m; N = total number of observed functions; m = number of grid points)
#' @param subject.index subject id; vector of length N with each element corresponding a row of Y
#' @param visit.index index for visits (repeated measures); vector of length N with each element corresponding a row of Y
#' @param obsT actual time of visits at which a function is observed; vector of length N with each element corresponding a row of Y
#' @param funcArg numeric; function argument
#'
#' @param ggplotsize.w width of downloaded PNG file; defaults to 27. 
#' @param ggplotsize.h height of downloaded PNG file; defaults to 20.
#' @param ggplotsize.unit Units in which \code{width} and \code{height} are given; defaults to "\code{cm}".
#' 
#' @author So Young Park \email{spark13@@ncsu.edu}, Ana-Maria Staicu
#' @export
#' 
#' @seealso \code{fpca.lfda} in the Refund package and \code{plot_interactive.lfpca} for estimation method 
#' @import shiny
#' @import ggplot2
#'  
#' @examples 
#'
#'   ###########################################################################################
#'   # data generation
#'   ###########################################################################################
#'   library(refund)
#'   library(rgl)
#'   set.seed(1)
#'   n <- 100 # number of subjects
#'   ss <- seq(0,1,length.out=101) 
#'   TT <- seq(0, 1, length.out=41)
#'   mi <- runif(n, min=6, max=15)
#'   ij <- sapply(mi, function(a) sort(sample(1:41, size=a, replace=FALSE)))
#'   
#'   # error variances
#'   sigma <- 0.1 
#'   sigma_wn <- 0.2
#'
#'   lambdaTrue <- c(1,0.5) # True eigenvalues
#'   eta1True <- c(0.5, 0.5^2, 0.5^3) # True eigenvalues
#'   eta2True <- c(0.5^2, 0.5^3) # True eigenvalues
#'   
#'   phi <- sqrt(2)*cbind(sin(2*pi*ss),cos(2*pi*ss))
#'   psi1 <- cbind(rep(1,length(TT)), sqrt(3)*(2*TT-1), sqrt(5)*(6*TT^2-6*TT+1))
#'   psi2 <- sqrt(2)*cbind(sin(2*pi*TT),cos(2*pi*TT))
#'   
#'   zeta1 <- sapply(eta1True, function(a) rnorm(n = n, mean = 0, sd = a))
#'   zeta2 <- sapply(eta2True, function(a) rnorm(n = n, mean = 0, sd = a))
#'   
#'   xi1 <- unlist(lapply(1:n, function(a) (zeta1 %*% t(psi1))[a,ij[[a]]] ))
#'   xi2 <- unlist(lapply(1:n, function(a) (zeta2 %*% t(psi2))[a,ij[[a]]] ))
#'   xi <- cbind(xi1, xi2)
#'   
#'   Tij <- unlist(lapply(1:n, function(i) TT[ij[[i]]] ))
#'   i <- unlist(lapply(1:n, function(i) rep(i, length(ij[[i]]))))
#'   j <- unlist(lapply(1:n, function(i) 1:length(ij[[i]])))
#'   
#'   X <- xi %*% t(phi)
#'   meanFn <- function(s,t){ 0.5*t + 1.5*s + 1.3*s*t}
#'   mu <- matrix(meanFn(s = rep(ss, each=length(Tij)), t=rep(Tij, length(ss)) ) , nrow=nrow(X))
#'
#'   Y <- mu +  X + 
#'      matrix(rnorm(nrow(X)*ncol(phi), 0, sigma), nrow=nrow(X)) %*% t(phi) + # correlated error process
#'      matrix(rnorm(length(X), 0, sigma_wn), nrow=nrow(X)) # white noise
#'
#'   # END: data generation
#'   
#'   plot_interactive.lfda(Y=Y, subject.index=i, visit.index=j, 
#'                         obsT=Tij, funcArg=ss)
#'   

plot_interactive.lfda <- function(Y, subject.index, visit.index, obsT, funcArg,
                                  ggplotsize.w=27, ggplotsize.h=20, ggplotsize.unit="cm"){


  myDat <<- list( y = Y, i = subject.index, j = visit.index, Tij = obsT, funcArg = funcArg)
  n <<- length(unique(myDat$i))
  numFuncArg <<- length(myDat$funcArg)
  numCurves <<- length(myDat$i)
  vecData <<- data.frame(curveID = rep(1:numCurves, each=numFuncArg),
                        i=rep(myDat$i, each = numFuncArg), 
                        j=rep(myDat$j,each = numFuncArg), 
                        y=as.vector(t(myDat$y)), 
                        funArg = rep(myDat$funcArg, numCurves), 
                        longTimes = rep(myDat$Tij, each = numFuncArg))
  overallMean <<- as.vector(colMeans(myDat$y) )
  mgin <<- diff(range(vecData$y))*0.01
  my.Ylim <<- c(range(vecData$y)[1] - mgin, range(vecData$y)[2] + mgin)
  
  shinyApp(
    
    ui = navbarPage(title = strong(style = "color: #ffc557; padding: 0px 0px 10px 10px; opacity: 0.95; ", "LFD Exploratory Plot"), windowTitle = "PlotInteractiveLFDA", 
                     collapsible = FALSE, id = "nav",
                     inverse = TRUE, header = NULL,

                     #################################
                     ## Tab 1: Data
                     #################################   
                     
                     tabPanel("Data", icon = icon("archive", lib = "font-awesome"),
                              column(3,
                                     checkboxInput("allDat", h4("Show all observations in background"), FALSE),
                                     hr(),
                                     checkboxInput("overallMean", h4("Show overall mean"), FALSE),
                                     hr(),
                                     selectInput('subj', h4('Select subject ID'), choices = unique(myDat$i), selected=1),
                                     hr(),
                                     helpText("Plot shows observed curves for the subject selected above."),
                                     
                                     br(), downloadButton('downloadPlotObsPDF', 'Download Plot as PDF'), br(), br(),
                                     downloadButton("downloadPlotObs", "Download Plot as Object", class = "plot-download")
                              ),
                              
                              column(9, h4("Plot of Longitudinal Functional Data"),
                                     plotOutput('Obs')
                              )
                     ),
                     
                     #################################
                     ## Tab 2: Data by subject
                     #################################   
                     
                     tabPanel("Data by Subject", icon = icon("caret-square-o-right", lib="font-awesome"),
                              column(3,
                                     selectInput('subj1', h4('Subject ID'), unique(myDat$i)),
                                     hr(),
                                     checkboxInput("allDatbySubj", h4("Show all observations from this subject"), TRUE), 
                                     hr(),
                                     checkboxInput("meanBySubj", h4("Show subject mean"), FALSE),
                                     hr(),
                                     checkboxInput("overallMean1", h4("Show overall mean"), FALSE),
                                     hr(),
                                     uiOutput("slider"),
#                                      sliderInput("longiTime", h4("actual time of visits (scaled)"),
#                                                  min = min(myDat$Tij), max = max(myDat$Tij),
#                                                  value = min(myDat$Tij), animate=animationOptions(interval=400)),
                                     hr(),
                                     helpText("Grey lines indicate observed curves for the subject selected above. 
                                               Solid blue line indicates subject-specific mean curve and black dashed line indicates overall mean."),
                                     
                                     #hr(),
                                     helpText("Play \'actual time of visits\' to see when each curve is observed (indicated with red)."),
                                      br(), downloadButton('downloadPlotObsbySubjPDF', 'Download Plot as PDF'), br(), br(),
                                      downloadButton("downloadPlotObsbySubj", "Download Plot as Object", class = "plot-download")

                                     
                              ),
                              
                              column(9, h4("Plot of Longitudinal Functional Observations by Subject"),
                                     plotOutput("bySubj")
                              )
                     ),
                     
                     ##############################################
                     ## Tab 3: Dist of Longitudinal Times
                     ##############################################   
                     
                     tabPanel("Distribution of Longitudinal Times", icon = icon("bar-chart", lib = "font-awesome"),
                              column(3,
                                     selectInput('subj3', h4('Subject ID'), unique(myDat$i)),
                                     hr(),
                                     checkboxInput("allDatbySubj2", h4("Show all observations from this subject"), TRUE),
                                     hr(),
                                     helpText("Empty circles indicate sampling time points of all curves. 
                                              Red dots indicate sampling time points of curves observed for the subject selected above. 
                                              Histogram shows the distribution of sampling time points for all subject."),
                                     br(), downloadButton('downloadPlotLongiTimePDF', 'Download Plot as PDF'),
                                     br(), br(),downloadButton('downloadPlotLongiHistPDF', 'Download Plot as PDF'), br(), br(),
                                     downloadButton("downloadPlotLongiTime", "Download Plot as Object", class = "plot-download"),
                                     br(), br(), downloadButton("downloadPlotLongiHist", "Download Plot as Object", class = "plot-download")
                                     
                              ),
                              
                              column(9, h4("Sampling Distribution of Longitudinal Times"),
                                     plotOutput("lonTime"),
                                     plotOutput("lonTimeHist")
                              )
                     )
     ),
   server <- function(input, output) {
       
       #################################
       ## Code for Tab 1: Data
       #################################   
       
       plotInputObs <- reactive({
         showAllDat <- input$allDat
         showOverallMean <- input$overallMean
         subjectChoice <- input$subj
         
         selectedData <- vecData[which(vecData$i==subjectChoice),]
         subjNumVisits <- max(unique(selectedData$j))
         myColors <- rainbow(max(vecData$j))[1:subjNumVisits]
         
         p <- ggplot(data=as.data.frame(vecData[which(vecData$i==1),]), aes(x=funArg, y=y, group=curveID)) + geom_line(linetype="blank") + theme_bw() + ylim(my.Ylim)
         
         if(showAllDat) p <- p + geom_line(aes(x=funArg, y=y, group=curveID), colour= "#b6b6b6", data=as.data.frame(vecData))
         if(showOverallMean) p <- p + geom_line(mapping=aes(x=x,y=y, group=rep(1,length(x))), data=data.frame(x=myDat$funcArg, y=overallMean), colour="black", linetype=2, size=1)
         
         p <- p + geom_line(aes(x=funArg, y=y, group=curveID), colour= rep(myColors, each=numFuncArg), data=as.data.frame(selectedData))
         p <- p + theme(legend.position="none") + xlab("s") + ylab("y") + 
            ggtitle(paste("Subject ", subjectChoice, "; Total of ", max(selectedData$j), " visits", sep=""))
   
         p
       })
       output$Obs <- renderPlot({
         print(plotInputObs())
       })
       
       output$downloadPlotObsPDF <- savePDF("all_observations.pdf", plotInputObs())  
       output$downloadPlotObs <- savePlot("all_observations.Rdata", plotInputObs())  
       
       #################################
       ## Code for Tab 2: Data by Subject
       ################################# 
       output$slider <- renderUI({
         
         subjectChoice <- input$subj1
         vecSelectedData <- vecData[which(vecData$i==subjectChoice),]
         TijSubj <- vecSelectedData$longTimes
         
         args <- list(inputId="longiTime", label=h4("actual time of visits (scaled)"), 
                            value = 0, 
                            animate=animationOptions(interval=2000), 
                            ticks = c(0, unique(TijSubj), 1))
         args$min = 1
         args$max = length(args$ticks)
        if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {
           
           ticks <- paste0(args$ticks, collapse=',')
           args$ticks <- TRUE
           html  <- do.call('sliderInput', args)
           html$children[[2]]$attribs[['data-values']] <- ticks;
           
        }else {
           html  <- do.call('sliderInput', args)     
         }
         
         html
       })
       
       plotInputDataBySubj <- reactive({
         
         subjectChoice <- input$subj1
         vecSelectedData <- vecData[which(vecData$i==subjectChoice),]
         indSubj <- which(myDat$i==input$subj1)
         obsBySubj <- myDat$y[indSubj,]
         TijSubj <- vecSelectedData$longTimes
         nrep <- length(unique(TijSubj))
         
         ind <- input$longiTime
         if(input$allDatbySubj){
           p <- ggplot(data=vecSelectedData, aes(x=funArg, y=y, group=curveID)) + geom_line(colour="#b6b6b6")  + theme_bw() + ylim(my.Ylim)
           if(any(c((ind==0), (ind==nrep+1), (length(ind) < 1)))){
             p <- p + xlab("s") + ylab("y")+ggtitle(paste("Subject ", subjectChoice, "; Total of ", nrep, " visits", sep=""))
           }else{
             p <- p + geom_line(data=vecSelectedData[which(vecSelectedData$j==ind), ], aes(x=funArg, y=y, group=curveID), colour="red")
             p <- p + xlab("s") + ylab("y") + 
               ggtitle(paste("Subject ", subjectChoice, "; Total of ", nrep, " visits; ", "Actual time of ", ind, "-th visit (red) = ",
                             unique(TijSubj)[ind], sep=""))
           }
           
         if(input$meanBySubj) p <- p + geom_line(mapping=aes(x=x,y=y, group=rep(1,length(myDat$funcArg))), data=data.frame(x=myDat$funcArg, y=colMeans(obsBySubj)), colour="blue", linetype=1, size=1)
         if(input$overallMean1) p <- p + geom_line(mapping=aes(x=x,y=y, group=rep(1,length(myDat$funcArg))), data=data.frame(x=myDat$funcArg, y=overallMean), colour="black", linetype=2, size=1)

           
           
         }else{
           p <- ggplot(data=vecSelectedData, aes(x=funArg, y=y, group=curveID)) + geom_line(linetype="blank")  + theme_bw() + ylim(my.Ylim)
           if(any(c((ind==0), (ind==nrep+1), (length(ind) < 1)))){
             p <- p + xlab("s") + ylab("y")+ggtitle(paste("Subject ", subjectChoice, "; Total of ", nrep, " visits", sep=""))
           }else{
             p <- p + geom_line(data=vecSelectedData[which(vecSelectedData$j==ind), ], aes(x=funArg, y=y, group=curveID), colour="red")
             p <- p + xlab("s") + ylab("y") + 
               ggtitle(paste("Subject ", subjectChoice, "; Total of ", nrep, " visits; ", "Actual time of ", ind, "-th visit (red) = ",
                             unique(TijSubj)[ind], sep=""))
           } 
           if(input$meanBySubj) p <- p + geom_line(mapping=aes(x=x,y=y, group=rep(1,length(myDat$funcArg))), data=data.frame(x=myDat$funcArg, y=colMeans(obsBySubj)), colour="blue", linetype=1, size=1)
           if(input$overallMean1) p <- p + geom_line(mapping=aes(x=x,y=y, group=rep(1,length(myDat$funcArg))), data=data.frame(x=myDat$funcArg, y=overallMean), colour="black", linetype=2, size=1)
           
         }
         p
       })
       
       output$bySubj <- renderPlot({
         print(plotInputDataBySubj())
       })
       
       output$downloadPlotObsbySubjPDF <- savePDF("obs_by_subj.pdf", plotInputDataBySubj())  
       output$downloadPlotObsbySubj <- savePlot("obs_by_subj.Rdata", plotInputDataBySubj())  
       
       ####################################################
       ## Code for Tab 3: Dist of Longitudinal Times
       ####################################################    
       
       plotInputLongTime <- reactive({
         
         subjectChoice1 <- as.numeric(input$subj3)
         index <- which(as.vector(myDat$i)==subjectChoice1)
         TijSubj1 <- as.vector(myDat$Tij[index])
         
         if(input$allDatbySubj2){
           p <- ggplot(mapping=aes(x=myDat$Tij, y=rep(1,length(myDat$Tij))))+geom_line() + geom_point(size=4,shape=21, fill="white")+theme_bw()
           p <- p + xlab("Longitudinal Times") + ylab("") + theme(axis.text.y=element_blank()) +
             ggtitle(paste("Longitudinal Times of Subject ", subjectChoice1, sep="")) + 
             geom_point(mapping=aes(x= x, y=y), data=data.frame(x=TijSubj1, y=rep(1, length(TijSubj1))), size=4, shape=21, fill="red")
         }else{
           p <- ggplot(mapping=aes(x=myDat$Tij, y=rep(1,length(myDat$Tij))))+theme_bw()+geom_line(linetype=0)
           p <- p + xlab("Longitudinal Times") + ylab("") + theme(axis.text.y=element_blank()) +
             ggtitle(paste("Longitudinal Times of Subject ", subjectChoice1, sep=""))
           p <- p + geom_line(mapping=aes(x= x, y = y), data=data.frame(x=TijSubj1, y=rep(1, length(TijSubj1))), linetype=1)
           p <- p + geom_point(mapping=aes(x= x, y = y), data=data.frame(x=TijSubj1, y=rep(1, length(TijSubj1))),
                               size=4, shape=21, fill="red")
         }
         p
       })
       
       plotInputLongTimeHist <- reactive({
         p <- ggplot(mapping=aes(x=myDat$Tij)) + theme_bw() 
         p <- p + geom_histogram(binwidth=0.05,  col="black", fill="#b6b6b6", alpha = 1) 
         p <- p + xlab("Longitudinal Times") + ylab("Frequency") + ggtitle("Histogram of Longitudinal Times")
         p  
       })
       output$lonTime <- renderPlot({
         print(plotInputLongTime())
       })
       output$lonTimeHist <- renderPlot({
         print(plotInputLongTimeHist())
       })
       
       output$downloadPlotLongiTimePDF <- savePDF("sampling_pts.pdf", plotInputLongTime())  
       output$downloadPlotLongiHistPDF <- savePDF("histogram.pdf", plotInputLongTimeHist())  
       
       output$downloadPlotLongiTime <- savePlot("sampling_pts.Rdata", plotInputLongTime())  
       output$downloadPlotLongiHist <- savePlot("histogram.Rdata", plotInputLongTimeHist())  
       })
}

