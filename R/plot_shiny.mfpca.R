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
  levels = names(mfpca.obj$npc)
  
  ## Tab 2: scree plot
  
  scree <- lapply(levels, function(level) {
    data.frame(k = rep(1:mfpca.obj$npc[[level]], 2), 
               lambda = c(mfpca.obj$evalues[[level]], cumsum(mfpca.obj$evalues[[level]])/ sum(mfpca.obj$evalues[[level]])),
               type = rep(c("Eigenvalue", "Percent Variance Explained"), each = mfpca.obj$npc[[level]]))
  })
  names(scree) <- c("level1", "level2")  
  
  ## Tab 3: linear combination of PCs
  evalues.all = c(mfpca.obj$evalues$level1, mfpca.obj$evalues$level2)
  varpercent1 = lapply(evalues.all[1:mfpca.obj$npc$level1], function(i){100*round(i/sum(mfpca.obj$evalues$level1), 3)}) 
  varpercent2 = lapply(evalues.all[(mfpca.obj$npc$level1+1):length(evalues.all)], function(i){100*round(i/sum(mfpca.obj$evalues$level2) ,3)}) 
  #varpercent.all = lapply(evalues.all, function(i){100*round(k/sum(evalues.all), 3)})
  
  
  calls <- lapply(mfpca.obj$npc, function(x) as.list(rep(NA, 3)))
  PCs <- lapply(mfpca.obj$npc, function(x) rep(NA, 3))
  #names(calls) <- names(PCs) <- c("level1", "level2")
  
  for(i in 1:3){
    PCnum = paste("PC", " 1.", i, sep="")    
    calls$level1[[i]] =  eval(call("sliderInput", inputId= PCnum, label = paste(PCnum, ": ", varpercent1[[i]],  "% of Level 1 Variance", sep=""),
                                   min = -2, max = 2, step = .1, value = 0, post = " SD", animate = animationOptions(interval=400, loop=T)))
    PCs$level1[i] = PCnum
  }  
  
  for(i in 1:3){
    PCnum = paste("PC", " 2.", i, sep="")    
    calls$level2[[i]] =  eval(call("sliderInput", inputId= PCnum, label = paste(PCnum, ": ", varpercent2[[i]],  "% of Level 2 Variance", sep=""),
                                   min = -2, max = 2, step = .1, value = 0, post = " SD", animate = animationOptions(interval=400, loop=T)))
    PCs$level2[i] = PCnum
  }  
  
  ### Tab 4: subject fits
  ids = unique(mfpca.obj$Y.df$id)
  Y.df = mfpca.obj$Y.df; Yhat.subj = mfpca.obj$Yhat.subject; Yhat = mfpca.obj$Yhat
  rownames(Yhat) = rownames(Yhat.subj) = rownames(Y.df)  ## set consistent rownames for grouping by visit in ggplot
  
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
                             fluidRow(
                               column(3,
                                      helpText("Solid black line indicates population mean. For the FPC selected below, blue and red lines 
                                             indicate the population mean +/- the FPC times 2 SDs of the associated score distribution."), hr(),
                                      selectInput("PCchoice1", label = ("Select Level 1 FPC"), choices = 1:mfpca.obj$npc$level1, selected = 1),
                                      br(), br(), downloadButton('downloadPlotMuPC1', 'Download Level 1 Plot')
                               ),
                               column(9, h4("Mean and FPCs for Level 1"),
                                      plotOutput('muPCplot1')
                               )
                             ),
                             fluidRow(
                               column(3,
                                      selectInput("PCchoice2", label = ("Select Level 2 FPC"), choices = 1:mfpca.obj$npc$level2, selected = 1),
                                      br(), br(), downloadButton('downloadPlotMuPC2', 'Download Level 2 Plot')
                               ),
                               column(9, h4("Mean and FPCs for Level 2"),
                                      plotOutput('muPCplot2')
                               )
                             ) ## end fluidRow
                             
                    ),
                    tabPanel("Scree Plot", icon = icon("medkit"),
                             fluidRow(
                               column(3, 
                                      helpText("Scree plots for level1 and level2; the left panel shows the plot of eigenvalues and 
                                             the right panel shows the cumulative percent variance explained."), 
                                      br(), br(), downloadButton('downloadPlotScree1', 'Download Level 1 Plot')
                               ),
                               column(9, h4("Scree Plots: Level 1"), 
                                      plotOutput('Scree1')
                               )
                             ),
                             fluidRow(
                               column(3, 
                                      
                                      br(), br(), downloadButton('downloadPlotScree2', 'Download Level 2 Plot')
                               ),
                               column(9, h4("Scree Plots: Level 2"), 
                                      plotOutput('Scree2')
                               )
                             ) ## end fluidRow
                             
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
                    )
                    ),
    
    #################################
    ## Server
    #################################

    server = function(input, output){
      
      mu = as.data.frame(cbind(1:length(mfpca.obj$mu), mfpca.obj$mu))
      efunctions = mfpca.obj$efunctions; 
      sqrt.evalues = lapply(mfpca.obj$evalues, function(x) diag(sqrt(x)))      
      scaled_efunctions = lapply(1:2, function(x) efunctions[[x]] %*% sqrt.evalues[[x]])
      names(scaled_efunctions) <- names(sqrt.evalues) <- names(efunctions) <- c("level1", "level2")
      
      #################################
      ## Code for mu PC plot
      #################################
      
      plotInputMuPC <- reactive({
        PCchoice = list(as.numeric(input$PCchoice1), as.numeric(input$PCchoice2))
        names(PCchoice) <- c("level1", "level2")
        scaled_efuncs = lapply(1:2, function(x) scaled_efunctions[[x]][,PCchoice[[x]]])
        
        p1 <- lapply(1:2, function(x){
          ggplot(mu, aes(x = V1, y = V2)) + geom_line(lwd=1) + theme_bw() +
            geom_point(data = as.data.frame(cbind(1:length(mfpca.obj$mu), mfpca.obj$mu + 2*scaled_efuncs[[x]])), color = "blue", size = 4, shape = '+')+
            geom_point(data = as.data.frame(cbind(1:length(mfpca.obj$mu), mfpca.obj$mu - 2*scaled_efuncs[[x]])), color = "red", size = 4, shape = "-")+
            scale_x_continuous(breaks = seq(0, length(mfpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))+
            xlab(xlab) + ylab(ylab) + ylim(c(range(mfpca.obj$Yhat)[1], range(mfpca.obj$Yhat)[2])) +
            ggtitle(bquote(psi[.(PCchoice[[x]])]~(t) ~ "," ~.(100*round(mfpca.obj$evalues[[x]][PCchoice[[x]]]/sum(mfpca.obj$evalues[[x]]),3)) ~ "% Variance"))   
        })   
      })
      
      output$muPCplot1 <- renderPlot(
        print(plotInputMuPC()[[1]])
      )   
      
      output$muPCplot2 <- renderPlot(
        print(plotInputMuPC()[[2]])
      )   
      
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
        p2 <-screeplots <- lapply(scree, function(x){
          ggplot(x, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
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
        PCweights = lapply(1:2, function(x) rep(NA, 3)) ; 
        names(PCweights) <- c("level1", "level2")
        for(i in 1:3){PCweights$level1[i] = input[[PCs$level1[i]]]}
        for(i in 1:3){PCweights$level2[i] = input[[PCs$level2[i]]]}
        
        df = as.data.frame(cbind(1:length(mfpca.obj$mu), 
                                 as.matrix(mfpca.obj$mu)+efunctions$level1[,1:3] %*% sqrt.evalues$level1[1:3,1:3] %*% PCweights$level1
                                 + efunctions$level2[,1:3] %*% sqrt.evalues$level2[1:3,1:3] %*% PCweights$level2,
                                 as.matrix(mfpca.obj$mu) +efunctions$level1[,1:3] %*% sqrt.evalues$level1[1:3,1:3] %*% PCweights$level1 ))
        
        names(df) = c("grid", "mu_visit", "mu_subj")
        p3 <- ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=1, aes(color= "mu"))+theme_bw()+
          scale_x_continuous(breaks = seq(0, length(mfpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))+
          geom_line(data = df, lwd = 1.5, aes(x=grid, y = mu_visit, color = "visit")) + 
          geom_line(data = df, lwd = 1.5, aes(x=grid, y = mu_subj, color = "subject")) + 
          xlab(xlab) + ylab(ylab) + ggtitle(title)+
          scale_color_manual("Line Legend", values = c(mu = "black", visit = "indianred",  subject = "cornflowerblue"), guide = FALSE)+ 
          theme(legend.key = element_blank()) + ylim(c(range(mfpca.obj$Yhat)[1], range(mfpca.obj$Yhat)[2]))
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

      ### change axes so they are the same as fpca.sc

      plotInputSubject <- reactive({
        id.cur = as.numeric(input$subject)
        
        df.obs = mutate(melt(as.matrix(subset(Y.df, id == id.cur, select = Y))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == id.cur))))
        df.Yhat.subj = mutate(melt(as.matrix(subset(Yhat.subj, Y.df$id == id.cur))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == id.cur))))
        df.Yhat = mutate(melt(as.matrix(subset(Yhat, Y.df$id == id.cur))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == id.cur))))
        names(df.obs) = names(df.Yhat.subj) =  names(df.Yhat) = c("visit", "Ynames", "value", "time")
        
        p4 <- ggplot(df.obs, aes(x = time, y = value, group = visit)) + geom_point(col = "indianred") +theme_bw() + 
          scale_x_continuous(breaks = seq(0, length(mfpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))+
          xlab(xlab) + ylab(ylab) + ylim(c(range(mfpca.obj$Yhat)[1], range(mfpca.obj$Yhat)[2])) +
          geom_line(data = mu, aes(x=V1, y=V2, group=NULL), col="gray")+
          geom_path(data=df.Yhat, col = "indianred") + geom_path(data = df.Yhat.subj, col="cornflowerblue", lty = 6, lwd = 1.25) 
        
        if(input$colVisit) {p4 = p4 + geom_point(aes(col = factor(visit))) + geom_path(data=df.Yhat, aes(col = factor(visit)))+ theme(legend.position="none")
                                                                                       
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
            
    } ## end server
  )
}

