#' Interactive Plotting for Multilevel Functional Principal Component Analysis
#' 
#' Produces an interactive plot illustrating a multilevel functional principal component 
#' analysis.
#' 
#' @param x mfpca object to be plotted. 
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param ... additional arguments passed to plotting functions
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu},
#' Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu} 
#' 
#' @seealso \code{\link{plot_shiny}}
#' @import shiny
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr mutate
#' @importFrom reshape2 melt
#' 
#' @export
#' 
#' 
plot_shiny.mfpca = function(x, xlab = "", ylab="", title = "", ...) {
    
  mfpca.obj <- x
  ################################
  ## code for processing tabs
  ################################
  npc = mfpca.obj$npc
  mu = data.frame(grid = 1:length(mfpca.obj$mu), values = mfpca.obj$mu)
  efunctions = mfpca.obj$efunctions; 
  sqrt.evalues = lapply(mfpca.obj$evalues, function(i) diag(sqrt(i)))      
  scaled_efunctions = lapply(1:2, function(i) efunctions[[i]] %*% sqrt.evalues[[i]])
  names(scaled_efunctions) <- names(sqrt.evalues) <- names(efunctions) <- c("level1", "level2")
  
  
  #################################
  ## Tab 3: mean +/- FPC plot
  #################################
  muPCtext = "Solid black line indicates population mean. For the FPC selected below, blue and red lines 
                                             indicate the population mean +/- the FPC times 2 SDs of the associated score distribution."
  
  #################################
  ## Tab 2: scree plot
  #################################
  scree <- lapply(names(npc), function(level) {
    data.frame(k = rep(1:npc[[level]], 2), 
               lambda = c(mfpca.obj$evalues[[level]], cumsum(mfpca.obj$evalues[[level]])/ sum(mfpca.obj$evalues[[level]])),
               type = rep(c("Eigenvalue", "Percent Variance Explained"), each = npc[[level]]))
  })
  names(scree) <- c("level1", "level2")  
 
  levelVariance = lapply(1:2, function(i) round(sum(mfpca.obj$evalues[[i]])/(sum(mfpca.obj$evalues[[1]])+sum(mfpca.obj$evalues[[2]])), 3)*100)

  #################################
  ## Tab 3:Linear combination of PCs
  #################################
  varpercents = lapply(c(1, 2, 12), function(i) varPercent(i, mfpca.obj)) 
  
  numSliders = 3
  calls <- mfpcaCalls(numSliders, mfpca.obj, varpercents)$calls
  PCs <- mfpcaCalls(numSliders, mfpca.obj, varpercents)$PCs
  
  #################################
  ## Tab 4: subject fits
  #################################
  ids = unique(mfpca.obj$Y.df$id)
  Y.df = mfpca.obj$Y.df
  Yhat.subj = mfpca.obj$Yhat.subject 
  Yhat = mfpca.obj$Yhat
  rownames(Yhat) = rownames(Yhat.subj) = rownames(Y.df)  ## set consistent rownames for grouping by visit in ggplot
  
  #################################
  ## Tab 5: score plots
  #################################
  
  ## from score plots
  scoreTextA = "Use the drop down menus to select FPCs for the X and Y axis. Plot shows observed 
    score scatterplot for selected FPCs; click and drag on the scatterplot to select subjects."
 
  ## repeat level 1 scores so they are the same dimension as input dataset  
  scores_new = list(mfpca.obj$scores$level1[rep(seq(nrow(mfpca.obj$scores$level1)), data.frame(table(Y.df$id))$Freq),], mfpca.obj$scores$level2)  
  Yhat.level = lapply(1:2, function(i) scores_new[[i]] %*% t(efunctions[[i]]))
  
  scoredata = as.list(rep(NA,2))
  for(i in 1:2){
    scoredata[[i]] = data.frame(scores_new[[i]])
    colnames(scoredata[[i]]) = c(paste0("PC", 1:npc[[i]]))
   
    scoredata[[i]]$Yhat.level = Yhat.level[[i]]
    scoredata[[i]]$Yhat = Yhat
  }
 
  Yhat.all.m = melt(mfpca.obj$Yhat)
  Yhat.level.all.m = lapply(1:2, function(i) melt(scoredata[[i]]$Yhat.level) )
  colnames(Yhat.level.all.m[[1]]) = colnames(Yhat.level.all.m[[2]]) = colnames(Yhat.all.m) = c("subj", "time", "value")
  
  ####### set some defaults for ggplot
  plotDefaults = list(theme_bw(), xlab(xlab), ylab(ylab), ylim(c(range(Yhat)[1], range(Yhat)[2])),
                      scale_x_continuous(breaks = seq(0, length(mfpca.obj$mu)-1, length=6),
                                         labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1))) )
  
  #################################
  ## App
  #################################
  
  shinyApp(
    
  #################################
  ## UI
  #################################
    
    ui = navbarPage(title = strong(style = "color: #ACD6FF; padding: 0px 0px 10px 10px; opacity: 0.95; ", "MFPCA Plot"), windowTitle = "refund.shiny", 
                    collapsible = FALSE, id = "nav",
                    inverse = TRUE, header = NULL,
                    tabPanel("Mean +/- FPCs", icon = icon("stats", lib = "glyphicon"),
                        tabsetPanel(
                             tabPanel("Level 1",
                                      column(3, helpText(muPCtext), hr(),
                                             selectInput("PCchoice1", label = ("Select Level 1 FPC"), choices = 1:npc$level1, selected = 1),hr(),
                                              downloadButton("downloadPDFMuPC1", "Download Plot as PDF"), br(), br(),
                                              downloadButton("downloadPlotMuPC1", "Download Plot as Object", class = "plot-download")
                                      ),
                                      column(9, h4("Mean and FPCs for Level 1"), plotOutput('muPCplot1') )
                             ),
                             tabPanel("Level 2", 
                                      column(3, helpText(muPCtext), hr(),
                                             selectInput("PCchoice2", label = ("Select Level 2 FPC"), choices = 1:mfpca.obj$npc$level2, selected = 1),
                                             hr(), downloadButton("downloadPDFMuPC2", "Download Plot as PDF"), br(), br(),
                                             downloadButton("downloadPlotMuPC2", "Download Plot as Object",  class = "plot-download")
                                      ),
                                      column(9,h4("Mean and FPCs for Level 2"),plotOutput('muPCplot2') )
                             )
                        )
                    ),
                    tabPanel("Scree Plot", icon = icon("medkit"),
                             tabsetPanel(
                               tabPanel("Level 1",
                                        column(3,helpText(paste0("Scree plots for level 1; the left panel shows the plot of eigenvalues and 
                                                          the right panel shows the cumulative percent variance explained. Level 1 accounts for ",
                                                                 levelVariance[[1]], "% of total variance." )), hr(),
                                               downloadButton("downloadPDFScree1", "Download Plot as PDF"), br(), br(), 
                                               downloadButton("downloadPlotScree1", "Download Plot as Object", class = "plot-download")                                               
                                        ),
                                        column(9, h4("Scree Plots"), tabPanel("Level 1", plotOutput('Scree1') ) )                                        
                               ),
                               tabPanel("Level 2",
                                        column(3,helpText(paste0("Scree plots for level 2; the left panel shows the plot of eigenvalues and 
                                                          the right panel shows the cumulative percent variance explained. Level 2 accounts for ",
                                                                 levelVariance[[2]], "% of total variance." )), hr(),
                                               downloadButton("downloadPDFScree2", "Download Plot as PDF"), br(), br(),                                               
                                               downloadButton("downloadPlotScree2", "Download Plot as Object", class = "plot-download")                                               
                                        ),
                                        column(9, h4("Scree Plots"),tabPanel("Level 2", plotOutput("Scree2") ) )
                               )
                              )  ## end tabsetPanel     
                    ),
                    tabPanel("Linear Combinations", icon = icon("line-chart"),
                             withMathJax(),
                             column(3, h4("Sliders for Levels 1 and 2"),
                                    helpText("Plot shows the linear combination of mean and FPCs with the scores 
                                             specified using the sliders below."), hr(),
                                    tabsetPanel(
                                      tabPanel("Level 1", eval(calls[[1]]) ),
                                      tabPanel("Level 2", eval(calls[[2]]) )
                                    ), hr(),
                                    downloadButton("downloadPDFLinCom", "Download Plot as PDF"), br(), br(),
                                    downloadButton("downloadPlotLinCom", "Download Plot as Object", class = "plot-download")                                                                                   
                             ),
                             column(9, h4("Linear Combination of Mean and FPCs"),  plotOutput('LinCom') )
                    ),
                    tabPanel("Subject Fits",  icon = icon("user"),
                             column(3,
                                    helpText("Plot shows observed data and fitted values for the subject selected below. Dotted blue curve is
                                             subject-specific mean and red curves are subject-visit specific fitted values."), hr(),
                                    selectInput("subject", label = ("Select Subject"), choices = ids, selected =ids[1]), hr(),
                                    checkboxInput("colVisit", label="Color by Visit", value =FALSE), 
                                    helpText("If 'Color by Visit' is selected, observed values and subject-visit specific fitted values
                                             are colored by visit number."), hr(),
                                    downloadButton("downloadPDFSubject", "Download Plot as PDF"), br(), br(),                                                                                   
                                    downloadButton("downloadPlotSubject", "Download Plot as Object", class = "plot-download") 
                                    
                                    ),
                             column(9, h4("Fitted and Observed Values for Selected Subject"),
                                      plotOutput("Subject")
                                    )
                    ),
                    tabPanel("Score Scatterplot",icon = icon("binoculars"),
                             tabsetPanel(
                               tabPanel("Level 1",
                                        fluidRow(                                          
                                          column(3,
                                                 helpText(scoreTextA), hr(),
                                                 selectInput("PCX1", label = ("Select X-axis FPC"), choices = 1:npc[[1]], selected = 1),
                                                 selectInput("PCY1", label = ("Select Y-axis FPC"), choices = 1:npc[[1]], selected = 2), hr()
                                          ), ## end column3
                                          column(9, h4("Score Scatterplot for Selected FPCs"),
                                                          plotOutput("ScorePlot1",
                                                                     brush=brushOpts(
                                                                       id = "ScorePlotL1_brush",
                                                                       resetOnNew = TRUE)
                                                          )            
                                          ) ## end column9          
                                        ),
                                        fluidRow(
                                          column(3, helpText("Black curves are fitted values for all subjects. Blue curves correspond 
                                                                  to subjects selected in the graph above.   If no points are selected, the mean 
                                                                  curve is shown.") 
                                          ),
                                          column(9, plotOutput("YhatPlot1"))
                                        )
                               ), ## end tabPanel
                               tabPanel("Level 2",
                                        fluidRow(                                          
                                          column(3,
                                                 helpText(scoreTextA), hr(),
                                                 selectInput("PCX2", label = ("Select X-axis FPC"), choices = 1:npc[[2]], selected = 1),
                                                 selectInput("PCY2", label = ("Select Y-axis FPC"), choices = 1:npc[[2]], selected = 2), hr()
                                          ), ## end column3
                                          column(9, h4("Score Scatterplot for Selected FPCs"),
                                                 plotOutput("ScorePlot2",
                                                            brush=brushOpts(
                                                              id = "ScorePlotL2_brush",
                                                              resetOnNew = TRUE)
                                                 )
                                          ) ## end column9          
                                        ),
                                        fluidRow(
                                          column(3, helpText("Black curves are Level 2 eigenvalues times Level 2 scores for subjects at
                                                        all visits. Blue curves correspond to observations selected in the graph above.") 
                                          ),
                                          column(9, plotOutput("YhatPlot2"))
                                        )
                               ) ## end tabPanel                               
                             ) ## end tabsetPanel
                    ) # end big tabPanel
                    
            ), ## end UI
    
    #################################
    ## Server
    #################################

    server = function(input, output){     
      
      #################################
      ## Code for mu PC plot
      #################################
      
      plotInputMuPC <- reactive({
        PCchoice = list(as.numeric(input$PCchoice1), as.numeric(input$PCchoice2))
        names(PCchoice) <- c("level1", "level2")
        scaled_efuncs = lapply(1:2, function(i) scaled_efunctions[[i]][,PCchoice[[i]]])
        
        p1 <- lapply(1:2, function(i){
          ggplot(mu, aes(x = grid, y = values)) + geom_line(lwd=1) + plotDefaults +
            geom_point(data = data.frame(grid =1:length(mfpca.obj$mu),values =  mfpca.obj$mu + 2*scaled_efuncs[[i]]), color = "blue", size = 4, shape = '+') +
            geom_point(data = data.frame(grid =1:length(mfpca.obj$mu), values = mfpca.obj$mu - 2*scaled_efuncs[[i]]), color = "indianred", size = 4, shape = "-") +
            ggtitle(bquote(psi[.(PCchoice[[i]])]~(t) ~ "," ~.(100*round(mfpca.obj$evalues[[i]][PCchoice[[i]]]/sum(mfpca.obj$evalues[[i]]),3)) ~ "% Variance"))   
        })   
      })
      
      output$muPCplot1 <- renderPlot(print(plotInputMuPC()[[1]]) )    ; output$muPCplot2 <- renderPlot(print(plotInputMuPC()[[2]]) )   
      
      output$downloadPDFMuPC1 <- savePDF("muPC1.pdf", plotInputMuPC()[[1]])
      output$downloadPlotMuPC1 <- savePlot("muPC1.RData", plotInputMuPC()[[1]])
   
      output$downloadPDFMuPC2 <- savePDF("muPC2.pdf", plotInputMuPC()[[2]]) 
      output$downloadPlotMuPC2 <- savePlot("muPC2.RData", plotInputMuPC()[[2]])
      
      #################################
      ## Code for scree plot
      #################################
      
      plotInputScree <- reactive({
        p2 <-screeplots <- lapply(scree, function(i){
          ggplot(i, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
            geom_point(size = 4, color = "black")+ theme_bw() + xlab("Principal Component") + ylab("") +
            facet_wrap(~type, scales = "free_y") + ylim(0, NA) 
        })      
      })
      
      output$Scree1 <- renderPlot( print(plotInputScree()[[1]]) )   ; output$Scree2 <- renderPlot(print(plotInputScree()[[2]]) )

      output$downloadPDFScree1 <- savePDF("scree1.pdf", plotInputScree()[[1]])
      output$downloadPlotScree1 <- savePlot("scree1.RData", plotInputScree()[[1]])
      
      output$downloadPDFScree2 <- savePDF("scree2.pdf", plotInputScree()[[2]])
      output$downloadPlotScree2 <- savePlot("scree2.RData", plotInputScree()[[2]])
      
      #################################
      ## Code for linear combinations
      #################################
      
      plotInputLinCom <- reactive({
               
        PCweights = lapply(1:2, function(i) rep(NA, numSliders)) ; 
        names(PCweights) <- c("level1", "level2")
        for(i in 1:numSliders){PCweights$level1[i] = input[[PCs$level1[i]]]}
        for(i in 1:numSliders){PCweights$level2[i] = input[[PCs$level2[i]]]}
        
        df = data.frame(1:length(mfpca.obj$mu), 
                                 as.matrix(mfpca.obj$mu)+efunctions$level1[,1:numSliders] %*% sqrt.evalues$level1[1:numSliders, 1:numSliders] %*% PCweights$level1
                                 + efunctions$level2[,1:numSliders] %*% sqrt.evalues$level2[1:numSliders, 1:numSliders] %*% PCweights$level2,
                                 as.matrix(mfpca.obj$mu) +efunctions$level1[,1:numSliders] %*% sqrt.evalues$level1[1:numSliders, 1:numSliders] %*% PCweights$level1 )
        
        names(df) = c("grid", "mu_visit", "mu_subj")
        p3 <- ggplot(mu, aes(x=grid, y=values))+geom_line(lwd=0.75, aes(color= "mu"))+ plotDefaults + theme(legend.key = element_blank())+
          geom_line(data = df, lwd = 1, aes(x=grid, y = mu_visit, color = "visit")) + 
          geom_line(data = df, lwd = 1.5, aes(x=grid, y = mu_subj, color = "subject")) + 
          scale_color_manual("Line Legend", values = c(mu = "gray", visit = "indianred",  subject = "cornflowerblue"), guide = FALSE)
   
      })
      
      output$LinCom <- renderPlot( print(plotInputLinCom()) )
      
      output$downloadPDFLinCom <- savePDF("LinCom.pdf", plotInputLinCom())
      output$downloadPlotLinCom <- savePlot("LinCom.RData", plotInputLinCom())
      
      #################################
      ## Code for subject plots
      #################################

      plotInputSubject <- reactive({
        id.cur = as.numeric(input$subject)
        
        df.obs = mutate(melt(as.matrix(subset(Y.df, id == id.cur, select = Y))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == id.cur))))
        df.Yhat.subj = mutate(melt(as.matrix(subset(Yhat.subj, Y.df$id == id.cur))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == id.cur))))
        df.Yhat = mutate(melt(as.matrix(subset(Yhat, Y.df$id == id.cur))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == id.cur))))
                
        names(df.obs) = names(df.Yhat.subj) = names(df.Yhat) = c("visit", "Ynames", "value", "time")
        
        
        p4 <- ggplot(df.obs, aes(x = time, y = value, group = visit)) + geom_point(col = "indianred", alpha = 1/5) + plotDefaults + 
          geom_line(data = mu, aes(x = grid, y = values, group=NULL), col="gray")+
          geom_path(data=df.Yhat, col = "indianred") + geom_path(data = df.Yhat.subj, col="cornflowerblue", lwd = 1.25) 
        
        if(input$colVisit) {p4 = p4 + geom_point(aes(col = factor(visit))) + geom_path(data=df.Yhat, aes(col = factor(visit)))+  theme(legend.position="none")                                                         
        } else{p4 = p4   }
        
       })
      
      output$Subject <- renderPlot( print(plotInputSubject()) )
      
      output$downloadPDFSubject <- savePDF("subject.pdf", plotInputSubject())
      output$downloadPlotSubject <- savePlot("subject.RData", plotInputSubject())
      
      #################################
      ## Code for score plots
      #################################              
                 
      PCX <- reactive({ list(level1 = paste0("PC", input$PCX1), level2 = paste0("PC", input$PCX2) ) }) 
      PCY <- reactive({ list(level1 = paste0("PC", input$PCY1), level2 = paste0("PC", input$PCY2) ) }) 
      
      
      ## Level 1 scatter plot of scores 
      output$ScorePlot1 <- renderPlot({ 
        ggplot(scoredata[[1]], aes_string(x = PCX()[[1]], y = PCY()[[1]]))+geom_point(color = "blue", alpha = 1/5, size = 3)+
          theme_bw()+xlab(paste("Scores for FPC", input$PCX1))+ylab(paste("Scores for FPC", input$PCY1))   
      })      
            
      #########################################
      # Store brush values as a data frame
      #######################################

      YhatPlots <- reactive({
        brushes <- list(input$ScorePlotL1_brush, input$ScorePlotL2_brush)
        plots = list()
        
        for (i in 1:2){
           if(!is.null(brushes[[i]])){           
            points = brushedPoints(scoredata[[i]], brushes[[i]], xvar=PCX()[[i]], yvar = PCY()[[i]])
            Yhat.m = melt(as.matrix(points$Yhat))
            Yhat.level.m = melt(as.matrix(points$Yhat.level))
            
            colnames(Yhat.level.m) <- c("subj", "time", "value")        
          }else{ Yhat.m = data.frame(1, 1:length(mfpca.obj$mu), mfpca.obj$mu) }  
          colnames(Yhat.m) <- c("subj", "time", "value") 
          
          plots[[i]] <- ggplot(Yhat.all.m, aes(x=time, y=value, group = subj)) + geom_line(alpha = 1/5, color="black") +  plotDefaults+
            geom_line(data = Yhat.m, aes(x=as.numeric(time), y=value, group = subj), color="cornflowerblue")    
           
          plots[[i+2]] <- ggplot(Yhat.level.all.m[[i]], aes(x=time, y=value, group = subj)) + geom_line(alpha = 1/5, color="black") +
            plotDefaults[-c(4)] + ylim(c(range(scoredata[[i]]$Yhat.level)[1], range(scoredata[[1]]$Yhat.level)[2]))
          
          if(!is.null(brushes[[i]])){
            plots[[i+2]] = plots[[i+2]] + geom_line(data = Yhat.level.m, aes(x=as.numeric(time), y=value, group = subj), color="cornflowerblue")
          } else{ plots[[i+2]] = plots[[i+2]] }                              
        }
                
        return(plots)   
      })
 
      output$YhatPlot1 <- renderPlot({
        grid.arrange(YhatPlots()[[1]],YhatPlots()[[3]], ncol = 2)
      })
      
      ####### Level 2 Tab
      
      ## Level 2 Plot 1
      output$ScorePlot2 <- renderPlot({ 
        ggplot(scoredata[[2]], aes_string(x = PCX()[[2]], y = PCY()[[2]]))+geom_point(color = "blue", alpha = 1/5, size = 3)+theme_bw()+
          xlab(paste("Scores for FPC", input$PCX2))+ylab(paste("Scores for FPC", input$PCY2))   
      })
      
      output$YhatPlot2 <- renderPlot({
        grid.arrange(YhatPlots()[[2]],YhatPlots()[[4]], ncol = 2)
      })
      
    } ## end server
  )
}

