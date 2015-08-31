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
  mu = as.data.frame(cbind(1:length(mfpca.obj$mu), mfpca.obj$mu))
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
  
  #################################
  ## Tab 3:Linear combination of PCs
  #################################
  varpercents = lapply(c(1, 2, 12), function(x) varPercent(x, mfpca.obj)) 
  
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
  scoredata = lapply(1:2, function(i) data.frame(mfpca.obj$scores[[i]]))       
  for(i in 1:2){colnames(scoredata[[i]]) = c(paste0("PC", 1:npc[[i]]))}
  
  
  
  Yhat.visit = mfpca.obj$scores[[2]] %*% t(efunctions[[2]])
  scoredata2 = as.data.frame(cbind(mfpca.obj$scores[[2]], Yhat.visit)) 
  colnames(scoredata2) = c(paste0("PC", 1:npc[[2]]), paste0("subj", 1:dim(mfpca.obj$Yhat)[2]))
  
  
  
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
                                             selectInput("PCchoice1", label = ("Select Level 1 FPC"), choices = 1:npc$level1, selected = 1),
                                             br(), br(), downloadButton('downloadPlotMuPC1', 'Download Level 1 Plot')
                                      ),
                                      column(9, h4("Mean and FPCs for Level 1"), plotOutput('muPCplot1') )
                             ),
                             tabPanel("Level 2", 
                                      column(3, helpText(muPCtext), hr(),
                                             selectInput("PCchoice2", label = ("Select Level 2 FPC"), choices = 1:mfpca.obj$npc$level2, selected = 1),
                                             br(), br(), downloadButton('downloadPlotMuPC2', 'Download Level 2 Plot')       
                                      ),
                                      column(9,h4("Mean and FPCs for Level 2"),plotOutput('muPCplot2') )
                             )
                        )
                    ),
                    tabPanel("Scree Plot", icon = icon("medkit"),
                             column(3,
                               fluidRow( helpText("Scree plots for level 1 and level 2; the left panel shows the plot of eigenvalues and 
                                             the right panel shows the cumulative percent variance explained."), hr(),
                                      br(), br(), downloadButton('downloadPlotScree1', 'Download Level 1 Plot')
                               ),
                               fluidRow( br(),br(), br(), downloadButton('downloadPlotScree2', 'Download Level 2 Plot') )
                             ),
                             column(9, h4("Scree Plots"),
                                tabsetPanel(
                                  tabPanel("Level 1", plotOutput('Scree1') ),
                                  tabPanel("Level 2", plotOutput('Scree2') )
                                )
                             )       
                    ),
                    tabPanel("Linear Combinations", icon = icon("line-chart"),
                             withMathJax(),
                             column(3, h4("Sliders for Levels 1 and 2"),
                                    helpText("Plot shows the linear combination of mean and FPCs with the scores 
                                             specified using the sliders below."), hr(),
                                    eval(calls),
                                    br(), br(), downloadButton('downloadPlotLinCom', 'Download Plot')
                             ),
                             column(9, h4("Linear Combination of Mean and FPCs"), 
                                    plotOutput('LinCom')
                             )
                    ),
                    tabPanel("Subject Fits",  icon = icon("user"),
                             column(3,
                                    helpText("Plot shows observed data and fitted values for the subject selected below. Dotted blue curve is
                                             subject-specific mean and red curves are subject-visit specific fitted values."), hr(),
                                    selectInput("subject", label = ("Select Subject"), choices = ids, selected =ids[1]), hr(),
                                    checkboxInput("colVisit", label="Color by Visit", value =FALSE), 
                                    helpText("If 'Color by Visit' is selected, observed values and subject-visit specific fitted values
                                             are colored by visit number."),
                                    br(), br(), downloadButton('downloadPlotSubject', 'Download Plot')
                                    ),
                             column(9, h4("Fitted and Observed Values for Selected Subject"),
                                      plotOutput("Subject")
                                    )
                    ),
                    tabPanel("Score Scatterplot",icon = icon("binoculars"),
                             tabsetPanel(
                               tabPanel("Level 1",
                                        column(3,
                                               fluidRow( 
                                                 helpText(scoreTextA), hr(),
                                                 selectInput("PCX1", label = ("Select X-axis FPC"), choices = 1:npc[[1]], selected = 1),
                                                 selectInput("PCY1", label = ("Select Y-axis FPC"), choices = 1:npc[[1]], selected = 2), hr()
                                               ),
                                               fluidRow( helpText("Black curves are fitted values for all subjects. Blue curves correspond 
                                                                  to subjects selected in the graph above.   If no points are selected, the mean 
                                                                  curve is shown.") )
                                        ),
                                        column(9,
                                               fluidRow(h4("Score Scatterplot for Selected FPCs"),
                                                        plotOutput("ScorePlot1_L1",
                                                                   brush=brushOpts(
                                                                     id = "ScorePlotL1_brush",
                                                                     resetOnNew = TRUE)
                                                        )            
                                               ),
                                               fluidRow( plotOutput("ScorePlot2_L1")
                                               )
                                        )
                               ), ## end tabPanel
                               tabPanel("Level 2",
                                        column(3,
                                               fluidRow(
                                                 helpText(scoreTextA), hr(),
                                                 selectInput("PCX2", label = ("Select X-axis FPC"), choices = 1:npc[[2]], selected = 1),
                                                 selectInput("PCY2", label = ("Select Y-axis FPC"), choices = 1:npc[[2]], selected = 2), hr()
     
                                               ),
                                               fluidRow( helpText("Black curves are Level 2 eigenvalues times Level 2 scores for subjects at
                                                                  all visits. Blue curves correspond to observations selected in the graph above.") )
                                        ),
                                        column(9,
                                               fluidRow( h4("Score Scatterplot for Selected FPCs"),
                                                         plotOutput("ScorePlot1_L2",
                                                                    brush=brushOpts(
                                                                      id = "ScorePlotL2_brush",
                                                                      resetOnNew = TRUE)
                                                         )
                                               ),
                                               fluidRow( plotOutput("ScorePlot2_L2") )
                                        )
                               )
                               
                             ) ## end tabsetPanel
                    ) ## end tabPanel
                    
                    
                    
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
          ggplot(mu, aes(x = V1, y = V2)) + geom_line(lwd=1) + plotDefaults +
            geom_point(data = as.data.frame(cbind(1:length(mfpca.obj$mu), mfpca.obj$mu + 2*scaled_efuncs[[i]])), color = "blue", size = 4, shape = '+')+
            geom_point(data = as.data.frame(cbind(1:length(mfpca.obj$mu), mfpca.obj$mu - 2*scaled_efuncs[[i]])), color = "red", size = 4, shape = "-")+
            ggtitle(bquote(psi[.(PCchoice[[i]])]~(t) ~ "," ~.(100*round(mfpca.obj$evalues[[i]][PCchoice[[i]]]/sum(mfpca.obj$evalues[[i]]),3)) ~ "% Variance"))   
        })   
      })
      
      output$muPCplot1 <- renderPlot(print(plotInputMuPC()[[1]]) )     
      output$muPCplot2 <- renderPlot(print(plotInputMuPC()[[2]]) )   
      
      output$downloadPlotMuPC1 <- downloadHandler(
        filename = function(){ 'mean_FPC1.png' },
        content = function(file) {
          ggsave(file,plotInputMuPC()[[1]])
        }
      )
      
      output$downloadPlotMuPC2 <- downloadHandler(
        filename = function(){ 'mean_FPC2.png' },
        content = function(file) {
          ggsave(file,plotInputMuPC()[[2]])
        }
      )
      
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
      
      output$Scree1 <- renderPlot(
        print(plotInputScree()[[1]])
      )
      
      output$Scree2 <- renderPlot(
        print(plotInputScree()[[2]])
      )
      output$downloadPlotScree1 <- downloadHandler(
        filename = function(){'screeplots1.png' },
        content = function(file) {
          ggsave(file,plotInputScree()[[1]])
        }
      ) 
      
      output$downloadPlotScree2 <- downloadHandler(
        filename = function(){'screeplots2.png' },
        content = function(file) {
          ggsave(file,plotInputScree()[[2]])
        }
      ) 
      
      #################################
      ## Code for linear combinations
      #################################
      
      plotInputLinCom <- reactive({
        PCweights = lapply(1:2, function(i) rep(NA, numSliders)) ; 
        names(PCweights) <- c("level1", "level2")
        for(i in 1:numSliders){PCweights$level1[i] = input[[PCs$level1[i]]]}
        for(i in 1:numSliders){PCweights$level2[i] = input[[PCs$level2[i]]]}
        
        df = as.data.frame(cbind(1:length(mfpca.obj$mu), 
                                 as.matrix(mfpca.obj$mu)+efunctions$level1[,1:numSliders] %*% sqrt.evalues$level1[1:numSliders, 1:numSliders] %*% PCweights$level1
                                 + efunctions$level2[,1:numSliders] %*% sqrt.evalues$level2[1:numSliders, 1:numSliders] %*% PCweights$level2,
                                 as.matrix(mfpca.obj$mu) +efunctions$level1[,1:numSliders] %*% sqrt.evalues$level1[1:numSliders, 1:numSliders] %*% PCweights$level1 ))
        
        names(df) = c("grid", "mu_visit", "mu_subj")
        p3 <- ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=1, aes(color= "mu"))+ plotDefaults + theme(legend.key = element_blank())+
          geom_line(data = df, lwd = 1.5, aes(x=grid, y = mu_visit, color = "visit")) + 
          geom_line(data = df, lwd = 1.5, aes(x=grid, y = mu_subj, color = "subject")) + 
          scale_color_manual("Line Legend", values = c(mu = "black", visit = "indianred",  subject = "cornflowerblue"), guide = FALSE)
      })
      
      output$LinCom <- renderPlot(  
        print(plotInputLinCom())   
      )
      
      output$downloadPlotLinCom <- downloadHandler(
        filename = function(){'FPC_LinearCombo.png' },
        content = function(file) {
          ggsave(file,plotInputLinCom())
        }
      )
      
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
          geom_line(data = mu, aes(x=V1, y=V2, group=NULL), col="gray")+
          geom_path(data=df.Yhat, col = "indianred") + geom_path(data = df.Yhat.subj, col="cornflowerblue", lty = 6, lwd = 1.25) 
        
        if(input$colVisit) {p4 = p4 + geom_point(aes(col = factor(visit))) + geom_path(data=df.Yhat, aes(col = factor(visit)))+  theme(legend.position="none")                                                         
        } else{p4 = p4   }
        
       })
      
      output$Subject <- renderPlot( 
        print(plotInputSubject())        
      )
      
      output$downloadPlotSubject <- downloadHandler(
        filename = function(){'subjectPlot.png' },
        content = function(file) {
          ggsave(file,plotInputSubject())
        }
      )
      
      #################################
      ## Code for score plots
      #################################              
      
      ###
      #
      # right now only plot 1 is shown... need to update so we see plot 2... maybe use code from the other plot where you
          ## melt Yhat values instead, and take Yhat.subject out of the scoredata matrix so you don't have dimension problem
          ## and don't have ot reduce the data in  weird way, which will naturally deal with the eta problem.
      #
      #scoredata = as.data.frame(cbind(mfpca.obj$scores[[1]], mfpca.obj$Yhat.subject)) 
      #colnames(scoredata) = c(paste0("PC", 1:mfpca.obj$npc[[1]]), paste0("subj", 1:dim(mfpca.obj$Yhat.subject)[2]))
      
 

      
      
      #################################################################################
      #
      # new score plot stuff
       
      ##### Level 1 Tab
      PCX1 <- reactive({ paste0("PC", input$PCX1) }); PCY1 <- reactive({ paste0("PC", input$PCY1) })
      
      ## Level 1 Plot 1
      output$ScorePlot1_L1 <- renderPlot({ 
        ggplot(scoredata[[1]], aes_string(x = PCX1(), y = PCY1()))+geom_point(color = "blue", alpha = 1/5, size = 3)+
          theme_bw()+xlab(paste("Scores for FPC", input$PCX1))+ylab(paste("Scores for FPC", input$PCY1))   
      })
      
      
      ## Level 1 Plot 2
      Yhat.all.m = melt(mfpca.obj$Yhat); colnames(Yhat.all.m) = c("subj", "time", "value")   
      baseplot = ggplot(Yhat.all.m, aes(x=time, y=value, group = subj)) + geom_line(alpha = 1/5, color="black") + 
        plotDefaults
      
      output$ScorePlot2_L1 <- renderPlot({
        
        brush <- input$ScorePlotL1_brush
        if(!is.null(brush)){           
          points = brushedPoints(scoredata[[1]], input$ScorePlotL1_brush, xvar=PCX1(), yvar = PCY1())
          Yhat.m = melt(as.matrix(points[,-c(1:mfpca.obj$npc[[1]])]))
          
        }else{
          Yhat.m = as.data.frame(cbind(1, 1:length(mfpca.obj$mu), mfpca.obj$mu))
        }
        
        colnames(Yhat.m) <- c("subj", "time", "value")  
        baseplot+geom_line(data= Yhat.m, aes(x=as.numeric(time), y=value, group = subj), color="blue")
        
      })
      
      ####### Level 2 Tab
      PCX2 <- reactive({ paste0("PC", input$PCX2) }); PCY2 <- reactive({ paste0("PC", input$PCY2) })
      
      ## Level 2 Plot 1
      output$ScorePlot1_L2 <- renderPlot({ 
        ggplot(scoredata2, aes_string(x = PCX2(), y = PCY2()))+geom_point(color = "blue", alpha = 1/5, size = 3)+theme_bw()+
          xlab(paste("Scores for FPC", input$PCX2))+ylab(paste("Scores for FPC", input$PCY2))   
      })
      
      ## Level 2 Plot 2
      Yhat.visit.all.m = melt(Yhat.visit); colnames(Yhat.visit.all.m) = c("subj", "time", "value")   
      baseplot = ggplot(Yhat.visit.all.m, aes(x=time, y=value, group = subj)) + geom_line(alpha = 1/5, color="black") 
      
      output$ScorePlot2_L2 <- renderPlot({
        
        brush <- input$ScorePlotL2_brush
        if(!is.null(brush)){           
          points = brushedPoints(scoredata2, input$ScorePlotL2_brush, xvar=PCX2(), yvar = PCY2())
          Yhat.m = melt(as.matrix(points[,-c(1:npc[[2]])]))
          
          colnames(Yhat.m) <- c("subj", "time", "value")  
          
          baseplot+geom_line(data= Yhat.m, aes(x=as.numeric(time), y=value, group = subj), color="blue")
          
        }else{
          #Yhat.m = as.data.frame(cbind(1, 1:length(mfpca.obj$mu), mfpca.obj$mu))
          baseplot
        }
        
         
      })
      
      
      
    } ## end server
  )
}

