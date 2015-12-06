#' Interactive Plotting for Longtiudinal Functional Data Analysis using FPCA
#'
#' Produces an interactive plot illustrating longitudinal functioanl data analysis (Park and Staicu, 2015).
#' 
#' @param result_LFPCA output of fpca.lfda() function in the Refund package
#' @param ggplotsize.w width of downloaded PNG file; defaults to 27. 
#' @param ggplotsize.h height of downloaded PNG file; defaults to 20.
#' @param ggplotsize.unit Units in which \code{width} and \code{height} are given; defaults to "\code{cm}".
#' 
#' @author So Young Park \email{spark13@@ncsu.edu}, Ana-Maria Staicu
#' @export
#' 
#' @references Park, S.Y. and Staicu, A.M. (2015). Longitudinal functional data analysis. Stat 4 212-226.
#' 
#' @seealso \code{plot_interactive.lfda} for exploratory data analysis for longitudinally-observed functional data; \code{fpca.lfda} in the Refund package for estimation method; 
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
#'   ###########################################################################################
#'   # Illustration I : when covariance of scores from a mFPCA step is estimated using fpca.sc
#'   ###########################################################################################
#'   est <- fpca.lfda(Y = Y, 
#'                    subject.index = i,
#'                    visit.index = j,
#'                    obsT = Tij,
#'                    funcArg = ss,
#'                    numTEvalPoints = length(TT), newdata = data.frame(i = c(1:3), Ltime = c(Tij[1], 0.2, 0.5)), 
#'                    fbps.knots = 35, fbps.p = 3, fbps.m = 2,
#'                    LongiModel.method='fpca.sc',
#'                    mFPCA.pve = 0.95, mFPCA.knots = 35, mFPCA.p = 3, mFPCA.m = 2, mFPCA.npc = NULL,
#'                    sFPCA.pve = 0.95, sFPCA.nbasis = 10, sFPCA.npc = NULL,
#'                    gam.method = 'REML', gam.kT = 10)
#'                    
#'   # exploratory data analysis                  
#'   plot_interactive.lfda(Y=Y, subject.index=i, visit.index=j, 
#'                         obsT=Tij, funcArg=ss)
#'   # results of longitudinal functional data analysis using FPCA
#'   plot_interactive.lfpca(result_LFPCA = est)
#'   
#'   ###########################################################################################
#'   # Illustration II : when covariance of scores from a mFPCA step is estimated using lmer
#'   ###########################################################################################
#'   est.lme <- fpca.lfda(Y = Y, 
#'                        subject.index = i,
#'                        visit.index = j,
#'                        obsT = Tij,
#'                        funcArg = ss,
#'                        numTEvalPoints = length(TT), newdata = data.frame(i = c(1:3), Ltime = c(Tij[1], 0.2, 0.5)), 
#'                        fbps.knots = 35, fbps.p = 3, fbps.m = 2,
#'                        LongiModel.method='lme',
#'                        mFPCA.pve = 0.95, mFPCA.knots = 35, mFPCA.p = 3, mFPCA.m = 2, mFPCA.npc = NULL,
#'                        gam.method = 'REML', gam.kT = 10)
#' 
#'   plot_interactive.lfpca(result_LFPCA = est.lme)
  

#####################################################################################################################
#####################################################################################################################

plot_interactive.lfpca <- function(result_LFPCA, 
                                  ggplotsize.w=27, ggplotsize.h=20, ggplotsize.unit="cm"){
  
  result <<- result_LFPCA
  result$TT <<- seq(min(result$visitTime), max(result$visitTime), length.out=nrow(result$bivariateSmoothMeanFunc))
  
  b<-list()
  for(k in 1:result$mFPCA.npc){
    a <- do.call(rbind, lapply(result$sFPCA.xiHat.bySubj, function(i) i[,k]))
    b[[k]] <- a
  }
  result$sFPCA.xiHat <<- b
  result$mFPCA.pve <<- cumsum(result$mFPCA.scree.eval)/sum(result$mFPCA.scree.eval)
  
  shinyApp(
    
    
   ui = navbarPage(title = strong(style = "color: #ffc557; padding: 0px 0px 10px 10px; opacity: 0.95; ", "LFDA: FPCA Plot"), windowTitle = "PlotInteractiveLFDA.FPCA", 
                   collapsible = FALSE, id = "nav",
                   inverse = TRUE, header = NULL,
                   
                   
                   tabPanel("Mean Surface", icon = icon("check-square", lib = "font-awesome"),
                            column(3, 
                                   helpText("Plot shows estimated bivariate smooth mean function; x-axis represents visit time (T), y-axis represents functional argument (s), and color represents the estimated mean function. "),
                                   br(), downloadButton('downloadPlotmuhatPDF', 'Download Plot as PDF'), br(), br(),
                                   downloadButton("downloadPlotmuhat", "Download Plot as Object", class = "plot-download")
                                   
                            ),
                            
                            column(9, h4("Bivariate Mean Surface"),
                                   plotOutput('bivMean')
                            )
                   ),
                   
                   # estimated marginal COVARIANCE
                  tabPanel("Marginal Covariance", icon = icon("star", lib="font-awesome"),
                           column(3, 
                                  helpText("Plot shows estimated marginal covariance."),
                                  
                                  br(), downloadButton('downloadPlotMarginalCOVPDF', 'Download Plot as PDF'), br(), br(),
                                  downloadButton("downloadPlotMarginalCOV", "Download Plot as Object", class = "plot-download")
                                  
                                  ),

                           column(9, h4("Estimated Marginal Covariance"),
                                  plotOutput('MarginalCOV')
                           )            
                  ),
                  
                  # estimated marginal eigenfunctions
                  tabPanel("Marginal Eigenfunctions", icon=icon("eyedropper", lib="font-awesome"),
                           column(3,
                                  selectInput("PCchoiceMEIG", label = h4("Select FPC"), choices = seq_len(result$mFPCA.npc), selected = 1),
                                  hr(),
                                  helpText("Plot shows estimated marginal eigenfunction for the FPC selected above."),
                                  br(), downloadButton('downloadPlotMEIGPDF', 'Download Plot as PDF'), br(), br(),
                                  downloadButton("downloadPlotMEIG", "Download Plot as Object", class = "plot-download")
                                  
                                  
                                  
                           ),
                           
                           column(9, h4("Estimated Marginal Eigenfunction"),
                                  plotOutput('MEIG')
                           ) 
                           
                           
                           
                           
                           
                  ),
                   
                  tabPanel("Mean +/- mFPCs", icon = icon("stats", lib = "glyphicon"),
                            column(3, 
                                   selectInput("PCchoice", label = h4("Select FPC"), choices = seq_len(result$mFPCA.npc), selected = 1),
                                   hr(),
                                   helpText("Solid black line indicates population mean. For the FPC selected above, blue and red lines
                                            indicate the population mean +/- the FPC times 2 SDs of the associated marginal score distribution (square root of the corresponding marignal eigenvalue)."),
                                   br(), downloadButton('downloadPlotLinComPDF', 'Download Plot as PDF'), br(), br(),
                                   downloadButton("downloadPlotLinCom", "Download Plot as Object", class = "plot-download")
                                   
                                   
                            ),
                            
                            column(9, h4("Mean +/- marginal FPCs"),
                                   plotOutput('PCplot')
                            )
                            
                   ),
                   
                   tabPanel("Scree Plot", icon = icon("medkit"),
                            column(3, 
                                   helpText("Scree plots are displayed to the right. The first panel shows the plot of eigenvalues, and 
                                            the second plot shows the cumulative percent variance explained."),
                                   br(), downloadButton('downloadPlotScreePDF', 'Download Plot as PDF'), br(), br(),
                                   downloadButton("downloadPlotScree", "Download Plot as Object", class = "plot-download")
                                   
                                   ),
                            column(9, h4("Scree Plots (marginal FPCA)"), 
                                   plotOutput('Scree')
                            )     
                   ),
                   
                  # covariance of longitudinal dynamics (by marginal PCs)
                   tabPanel("Covariance of Longitudinal Dynamics", icon = icon("star", lib="font-awesome"),
                            column(3,  
                                   selectInput("PCchoiceLDCOV", label = h4("Select FPC"), choices = seq_len(result$mFPCA.npc), selected = 1),
                                   hr(),
                                   helpText("Plot shows estimated covariance of longitudinal dynamics for the marginal FPC selected above."),
                                   br(), downloadButton('downloadPlotLDCOVkPDF', 'Download Plot as PDF'), br(), br(),
                                   downloadButton("downloadPlotLDCOVk", "Download Plot as Object", class = "plot-download")
                                   
                                   ),       
                            column(9, h4("Estimated Covariance of Longitudinal Dynamics"), plotOutput('LDCOVk'))
                   ),
                  
                  tabPanel("Basis Coefficients (2nd FPCA)", icon=icon("user", lib = "font-awesome"),
                            column(3, hr(),
                                   checkboxInput("allSubject", h4("Show all subjects in background"), TRUE),
                                   hr(),
                                   selectInput('subject', h4('Select Subject ID'), unique(result$i)),
                                   hr(),
                                   selectInput("PCchoice1", label = h4("Select FPC"), choices = seq_len(result$mFPCA.npc), selected = 1),
                                   hr(),
                                   helpText("Grey lines indicate predicted basis coefficient functions of longitudinal time (T). 
                                            Red line highlights a predicted curve for the marginal FPC and subject selected above."),
                                   br(), downloadButton('downloadPlotScorePDF', 'Download Plot as PDF'), br(), br(),
                                   downloadButton("downloadPlotScore", "Download Plot as Object", class = "plot-download")
                                   
                            ),
                            column(9, h4("Predicted Basis Coefficients Plot"),
                                   plotOutput('xiHat_T')
                            )
                   ),
                   
                   tabPanel("Yhat", icon=icon("dot-circle-o", lib = "font-awesome"),
                            column(3, hr(),
                                   sliderInput("longiTime1", "Longitudinal Time",
                                               min = min(result$TT), max = max(result$TT), step = 2*(result$TT[2]-result$TT[1]),
                                               value = min(result$TT), animate=animationOptions(interval=800)),
                                   hr(),
                                   selectInput('subject1', h4('Select subject ID'), unique(result$i)),
                                   hr(),
                                   helpText("Plot shows predicted trajectory of the subject selected above. Play \'Longituidnal Time\' to see longitudinal change of the predicted trajectory. "),
                                   br(), downloadButton('downloadPlotPredictedYPDF', 'Download Plot as PDF'), br(), br(),
                                   downloadButton("downloadPlotPredicted", "Download Plot as Object", class = "plot-download")
                                   
                            ),
                            column(9, h4("Estimated individual Trajectory"),
                                   plotOutput('Yhat')
                            )
                  )
),
server <- function(input, output) {
  
  ##########################################
  ## Code for Tab 1: Bivariate Mean Surface 
  ##########################################
  plotBivMean <- reactive({
    p <- ggplot(data = data.frame(x=rep(result$funcArg, each = length(result$TT)), y= rep(result$TT, length(result$funcArg)), 
                                  MEAN=as.vector(result$bivariateSmoothMeanFunc)), 
                aes(x=x,y=y,fill=MEAN)) + geom_tile() + theme_bw() + xlab("Longitudinal Time (T)") + ylab("s") 
    p <- p  + scale_fill_gradient(low="red", high="white")
    p
  })
  
  output$bivMean <- renderPlot(
    print(plotBivMean())
  )
  
  output$downloadPlotmuhatPDF <- savePDF("biv_mean_surface.pdf", plotBivMean())  
  output$downloadPlotmuhat <- savePlot("biv_mean_surface.Rdata", plotBivMean())  
  
  
  ##############################################################
  ## Code for Tab 2.0.0: estimated marginal Covariance plot
  ##############################################################    
  
  plotInputMCOV <- reactive({
       
       p <- ggplot(data = data.frame(x=rep(result$funcArg, length(result$funcArg)), y= rep(result$funcArg, each=length(result$funcArg)), 
                                     COV=as.vector(result$mFPCA.covar)), 
                   aes(x=x,y=y,fill=COV)) + geom_tile() + theme_bw() + xlab("s") + ylab("s") 
       p <- p  + scale_fill_gradient(low="red", high="white")
       p
            
  })
  
  output$MarginalCOV <- renderPlot(
    print(plotInputMCOV())
  )
  
  output$downloadPlotMarginalCOVPDF <- savePDF("marginal_cov.pdf", plotInputMCOV())  
  output$downloadPlotMarginalCOV <- savePlot("marginal_cov.Rdata", plotInputMCOV())  
  
  ##############################################################
  ## Code for Tab 2.0.1: estimated marginal eigenfunctions
  ##############################################################  
  
  plotInputMEIG <- reactive({
    PCchoiceMEIG <- as.numeric(input$PCchoiceMEIG)
    p <- ggplot(data=data.frame(x=result$funcArg, y=result$mFPCA.efunctions[,PCchoiceMEIG]), aes(x=x, y=y)) + geom_line(lwd=1, colour="red") + theme_bw()
    p <- p + geom_line(data=data.frame(x=result$funcArg, y=rep(0, length(result$funcArg))), aes(x=x,y=y), colour="grey", lwd=1, linetype="dashed")
    p <- p + xlab("s") + ylab("")
    p
  })
  
  output$MEIG <- renderPlot(
    print(plotInputMEIG())
  )
  output$downloadPlotMEIGPDF <- savePDF("marginal_eigenfns.pdf", plotInputMEIG())  
  output$downloadPlotMEIG <- savePlot("marginal_eigenfns.Rdata", plotInputMEIG())  
  
  ##########################################
  ## Code for Tab 2.1: marginal FPCs plot
  ##########################################    
  
  plotInputPC <- reactive({
    
    PCchoice = as.numeric(input$PCchoice)
    
    p <- ggplot(data=data.frame(x=result$funcArg, y=colMeans(result$bivariateSmoothMeanFunc) ), aes(x=x, y=y)) + geom_line(lwd=1) + theme_bw()
    p <- p + geom_point(data = data.frame(x=result$funcArg, y=(colMeans(result$bivariateSmoothMeanFunc) + 2*sqrt(result$mFPCA.evalues[PCchoice])*result$mFPCA.efunctions[,PCchoice])),
                        color = "blue", size = 4, shape = '+')
    p <- p + geom_point(data = data.frame(x=result$funcArg, y=(colMeans(result$bivariateSmoothMeanFunc)  - 2*sqrt(result$mFPCA.evalues[PCchoice])*result$mFPCA.efunctions[,PCchoice])),
                        color = "red", size = 4, shape = '-')
    p <- p + ylim(range(result$fitted.values.all)) + xlab("s") + ylab("")+
      ggtitle(bquote(""~phi[.(PCchoice)](s) ~ "," ~.(100*round(result$mFPCA.pve[(PCchoice)],3)) ~ "% Variance explained"))   
    p
  })
  
  output$PCplot <- renderPlot(
    print(plotInputPC())  
  )

  output$downloadPlotLinComPDF <- savePDF("marginal_LinCom.pdf", plotInputPC())  
  output$downloadPlotLinCom <- savePlot("marginal_LinCom.Rdata", plotInputPC())  
  
  
  #################################
  ## Code for Tab 3: scree plot
  #################################
  
  plotInputScree <- reactive({
    scree <- data.frame(k = rep(1:(result$mFPCA.npc+1), 2), 
                        lambda = c(result$mFPCA.scree.eval[1:(result$mFPCA.npc+1)], 
                                   result$mFPCA.pve[1:(result$mFPCA.npc+1)]),
                        type = rep(c("Eigenvalue", "Percent Variance Explained"), each = (result$mFPCA.npc+1)))
    
    p <- ggplot(data=scree, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
      geom_point(size = 4, color = "black")+ theme_bw() + xlab("Principal Component") + ylab("") +
      facet_wrap(~type, scales = "free_y") + ylim(0, NA) 
    
    p
  })
  
  output$Scree <- renderPlot(
    print(plotInputScree())
  )
  
  output$downloadPlotScree <- downloadHandler(
    filename = function(){'screeplots_mFPCA.png' },
    content = function(file) {
      ggsave(file,plotInputScree())
    }
  )
  
  output$downloadPlotScreePDF <- savePDF("scree_plot.pdf", plotInputScree())  
  output$downloadPlotScree  <- savePlot("scree_plot.Rdata", plotInputScree())  
  
  ######################################################
  ## Code for Tab 4.0.0: covariance of longitudinal dynamics
  ######################################################
  
  plotInputLDCOVk <- reactive({
    PCchoice_LDCOVk = as.numeric(input$PCchoiceLDCOV)
    selectedCOV <- result$sFPCA.longDynCov.k[[PCchoice_LDCOVk]]
    p <- ggplot(data = data.frame(x=rep(result$TT, length(result$TT)), y= rep(result$TT, each=length(result$TT)), COV=as.vector(selectedCOV)), 
                aes(x=x,y=y,fill=COV)) + geom_tile() + theme_bw() + xlab("Longitudinal Time (T)") + ylab("Longitudinal Time (T)") 
   p <- p + scale_fill_gradient(low="red", high="white")
    p
    
  })
  
  output$LDCOVk <- renderPlot({
    print(plotInputLDCOVk())
  })
  
  output$downloadPlotLDCOVk <- downloadHandler(
    filename = function(){ paste('LDynamics_k', input$PCchoiceLDCOV, '.png', sep="") },
    content = function(file) {
      ggsave(file,plotInputLDCOVk())
    }
  )
  output$downloadPlotLDCOVkPDF <- savePDF("dynamics_cov.pdf", plotInputLDCOVk())  
  output$downloadPlotLDCOVk  <- savePlot("dynamics_cov.Rdata", plotInputLDCOVk())  
  
  #####################################################
  ## Code for Tab 4.1: basis coefficients plot
  #####################################################
  
  plotInputxiHat <- reactive({
    
    n <- length(unique(result$i))
    subjChoice <- input$subject
    kChoice <- as.numeric(input$PCchoice1)
    vecDat <- data.frame(y=as.vector(result$sFPCA.xiHat[[kChoice]]),
                         i = rep(unique(result$i), length(result$TT)),
                         x = rep(result$TT, each=n))
    mylim <- c(range(vecDat$y)[1] - diff(range(vecDat$y))*0.1, range(vecDat$y)[2]+diff(range(vecDat$y))*0.1)
    if(input$allSubject){
      p <- ggplot(data=vecDat, aes(x=x, y=y, group=i))  + geom_line(colour="#b6b6b6") + theme_bw() + ylim(mylim)
      p <- p+geom_line(data=vecDat[which(vecDat$i==subjChoice),], aes(x=x,y=y,group=i), colour="red", lwd=1)
      p <- p+xlab("Longitudinal Time (T)") + ylab(bquote(hat(xi)(T))) + 
        ggtitle(paste("Predicted Basis Coefficients for k =", kChoice,sep=""))
    }else{
      p <- ggplot(data=vecDat, aes(x=x, y=y, group=i))  + geom_line(linetype="blank") + theme_bw()
      p <- p+geom_line(data=vecDat[which(vecDat$i==subjChoice),], aes(x=x,y=y,group=i), colour="red", lwd=1)
      p <- p+xlab("Longitudinal Time (T)") + ylab(bquote(hat(xi)(T))) + 
        ggtitle(paste("Predicted Basis Coefficients for k =", kChoice,sep=""))
    }
    p 
  })
  
  output$xiHat_T <- renderPlot(
    print(plotInputxiHat())
  )
  
  output$downloadPlotScorePDF <- savePDF("score.pdf", plotInputxiHat())  
  output$downloadPlotScore  <- savePlot("score.Rdata", plotInputxiHat())  
  
  #################################
  ## Code for Tab 5: individual trajectory
  #################################
  
  plotInputYhat <- reactive({
    tempDat <- result$fitted.values.all[[as.numeric(input$subject1)]]
    v <- which(abs(result$TT-input$longiTime1)== min(abs(result$TT-input$longiTime1)))
    fixedT = result$TT[v]
    
    p <- ggplot(data.frame(x=result$funcArg, y=tempDat[v,]), aes(x=x,y=y))+geom_line(colour="red", lwd=1) + theme_bw()
    p <- p+xlab("s")+ylab(bquote(hat(Y)))+ggtitle(paste("Subject i = ", input$subject1))+ ylim(range(result$fitted.values.all))
    p
  })
  output$Yhat <- renderPlot({
    print(plotInputYhat())
  })
  output$downloadPlotYhat <- downloadHandler(
    filename = function(){paste('Yhat_Subject', input$subject1,"T",fixedT,'.png') },
    content = function(file) {
      ggsave(file,plotInputYhat())
    }
  )
  
  output$downloadPlotPredictedYPDF <- savePDF("predictedY.pdf", plotInputYhat())  
  output$downloadPlotPredictedY  <- savePlot("predictedY.Rdata", plotInputYhat())  
  
  
}
    

  )
  
  
}