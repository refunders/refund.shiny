#' Interactive Plotting for Functional Principal Component Analysis
#'
#' Produces an interactive plot illustrating a functional principal component
#' analysis.
#'
#' @param obj fpca object to be plotted.
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
#' @importFrom reshape2 melt
#'
#' @export
#'
plot_shiny.fpca = function(obj, xlab = "", ylab="", title = "", ...) {

  fpca.obj <- obj

  ### NULLify global values called in ggplot
  V1 = V2 = V3 = V4 = k = lambda = value = subj = time = NULL

  ################################
  ## code for processing tabs
  ################################

  ## Tab 1:
  muPC.help = "Solid black line indicates population mean. For the FPC selected below, blue and red lines
                                             indicate the population mean +/- the FPC times 2 SDs of the associated score distribution."
  muPC.call = eval(call("selectInput", inputId = "PCchoice", label = ("Select FPC"), choices = 1:fpca.obj$npc, selected = 1))
    
  ## Tab 2: scree plot
  
  scree.help = "Scree plots; the left panel shows the plot of eigenvalues and the right panel shows the cumulative percent variance explained."
  scree = data.frame(k = rep(1:fpca.obj$npc, 2),
                      lambda = c(fpca.obj$evalues, cumsum(fpca.obj$evalues)/ sum(fpca.obj$evalues)),
                      type = rep(c("Eigenvalue", "Percent Variance Explained"), each = fpca.obj$npc))

  ## Tab 3: linear combination of PCs
  LinCom.help = "Plot shows the linear combination of mean and FPCs with the scores specified using the sliders below."
  varpercent = lapply(fpca.obj$evalues, function(i){100*round(i/sum(fpca.obj$evalues),3)}) # calculates percent variance explained
  LinCom.call <- as.list(rep(NA, fpca.obj$npc))
  PCs <- rep(NA, fpca.obj$npc)
  for(i in 1:fpca.obj$npc){

    PCnum = paste("PC", i, sep="")

    LinCom.call[[i]] =  eval(call("sliderInput", inputId= PCnum, label = paste(PCnum, ": ", varpercent[[i]],  "% Variance", sep=""),
                            min = -2, max = 2, step = .1, value = 0, post = " SD", animate = animationOptions(interval=400, loop=T)))

    PCs[i] = PCnum
  }

  ## Tab 4: subject fits
  subjects.help = "Plot shows observed data and fitted values for the subject selected below."
  subjects.call = eval(call("selectInput", inputId = "subject" ,label = ("Select Subject"), choices = 1:dim(fpca.obj$Yhat)[1], selected = 1))
  
  ## Tab 5: score plots
  score.help1 = "Use the drop down menus to select FPCs for the X and Y axis. Plot shows observed score
                                             scatterplot for selected FPCs; click and drag on the scatterplot to select subjects."
  score.help2 = "Black curves are fitted values for all subjects. Blue curves correspond to subjects
                                                  selected in the graph above. If no points are selected, the mean curve is shown."
  
  score.call = tagList(  selectInput("PCX", label = ("Select X-axis FPC"), choices = 1:fpca.obj$npc, selected = 1),
    selectInput("PCY", label = ("Select Y-axis FPC"), choices = 1:fpca.obj$npc, selected = 2) )
  
  #################################
  ## App
  #################################

  shinyApp(

  #################################
  ## UI
  #################################

    ui = navbarPage(title = strong(style = "color: #ACD6FF; padding: 0px 0px 10px 10px; opacity: 0.95; ", "FPCA Plot"), 
                    windowTitle = "refund.shiny", collapsible = FALSE, id = "nav", inverse = TRUE, header = NULL,
                    ##### start tabs
                    tabPanelModuleUI("muPC", tabTitle = "Mean +/- FPCs", icon("stats", lib = "glyphicon"), calls = muPC.call, 
                                     helperText = muPC.help ),
                    tabPanelModuleUI("screeplots", tabTitle = "Scree Plots", icon = icon("medkit"), calls = NULL, 
                                     helperText = scree.help),
                    tabPanelModuleUI("LinCom", tabTitle = "Linear Combinations", icon = icon("line-chart"), calls = LinCom.call,
                                     helperText = LinCom.help ),
                    tabPanelModuleUI("subjects",tabTitle = "Subject Fits", icon = icon("user"), calls = subjects.call,
                                     helperText = subjects.help ),
                    tabPanelModuleUI("scoreplots",tabTitle = "Score Scatterplot", icon = icon("binoculars"), calls = score.call,
                                     helperText = score.help1, twoPlots = TRUE, helperText2 = score.help2, trackBrush = TRUE)
                    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

      mu = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu))
      efunctions = fpca.obj$efunctions; sqrt.evalues = diag(sqrt(fpca.obj$evalues))
      scaled_efunctions = efunctions %*% sqrt.evalues

      plotDefaults = list(theme_bw(), xlab(xlab), ylab(ylab), ylim(c(range(fpca.obj$Yhat)[1], range(fpca.obj$Yhat)[2])),
                          scale_x_continuous(breaks = seq(0, length(fpca.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1))) )

      #################################
      ## Code for mu PC plot
      #################################

      plotInputMuPC <- reactive({
        PCchoice = as.numeric(input$PCchoice)
        scaled_efuncs = scaled_efunctions[,PCchoice]

        p1 <- ggplot(mu, aes(x = V1, y = V2)) + geom_line(lwd=1) + plotDefaults +
          geom_point(data = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu + 2*scaled_efuncs)), color = "blue", size = 4, shape = '+')+
          geom_point(data = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu - 2*scaled_efuncs)), color = "indianred", size = 4, shape = "-")+
          ggtitle(bquote(psi[.(input$PCchoice)]~(t) ~ "," ~.(100*round(fpca.obj$evalues[as.numeric(input$PCchoice)]/sum(fpca.obj$evalues),3)) ~ "% Variance"))
      })

      callModule(tabPanelModule, "muPC", plotObject = plotInputMuPC, plotName = "muPC")
      
      #################################
      ## Code for scree plot
      #################################

      plotInputScree <- reactive({
        p2 <-screeplots <- ggplot(scree, aes(x=k, y=lambda))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_point(size = 4, color = "black")+ theme_bw() + xlab("Principal Component") + ylab("") +
          facet_wrap(~type, scales = "free_y") + ylim(0, NA)
      })
      
      callModule(tabPanelModule, "screeplots", plotObject = plotInputScree, plotName = "screeplots")

      #################################
      ## Code for linear combinations
      #################################

      plotInputLinCom <- reactive({
        PCweights = rep(NA, length(PCs)); for(i in 1:length(PCs)){PCweights[i] = input[[PCs[i]]]}
        df = as.data.frame(cbind(1:length(fpca.obj$mu), as.matrix(fpca.obj$mu)+efunctions %*% sqrt.evalues %*% PCweights ))

        p3 <- ggplot(mu, aes(x=V1, y=V2))+geom_line(lwd=0.75, aes(color= "mu"))+  plotDefaults + theme(legend.key = element_blank()) +
          geom_line(data = df, lwd = 1.5, aes(color = "subject")) + xlab(xlab) + ylab(ylab) + ggtitle(title)+
          scale_color_manual("Line Legend", values = c(mu = "gray", subject = "cornflowerblue"), guide = FALSE)
      })

      callModule(tabPanelModule, "LinCom", plotObject = plotInputLinCom, plotName = "LinCom")
      
      #################################
      ## Code for subject plots
      #################################

      plotInputSubject <- reactive({
        subjectnum = as.numeric(input$subject)
        df = as.data.frame(cbind(1:length(fpca.obj$mu), fpca.obj$mu, fpca.obj$Yhat[subjectnum,], fpca.obj$Y[subjectnum,]))

        p4 <- ggplot(data = df, aes(x=V1,y=V2)) + geom_line(lwd=0.5, color = "gray") + plotDefaults +
          geom_line(data = df, aes(y=V3), size=1, color = "cornflowerblue") +
          geom_point(data = df, aes(y=V4), color = "blue", alpha = 1/3)
      })

      callModule(tabPanelModule, "subjects", plotObject = plotInputSubject, plotName = "subjects")

      
      #################################
      ## Code for score plots
      #################################

      fda.obj = fpca.obj
      
      scoredata = as.data.frame(cbind(fda.obj$scores, fda.obj$Yhat))
      colnames(scoredata) = c(paste0("PC", 1:fda.obj$npc), paste0("subj", 1:dim(fda.obj$Yhat)[2]))
      
      ## get PCs selected for X and Y axis
      PCX <- reactive({ paste0("PC", input$PCX) })
      PCY <- reactive({ paste0("PC", input$PCY) })
      
      
      ## first score plot
      plotInputScore1 <- reactive({
        p1 <-  ggplot(scoredata, aes_string(x = PCX(), y = PCY()))+geom_point(color = "blue", alpha = 1/5, size = 3)+theme_bw()+
          xlab(paste("Scores for FPC", input$PCX))+ylab(paste("Scores for FPC", input$PCY))
      })
      
      ### second score plot
      Yhat.all.m = melt(fda.obj$Yhat)
      colnames(Yhat.all.m) = c("subj", "time", "value")
      baseplot = ggplot(Yhat.all.m, aes(x=time, y=value, group = subj)) + geom_line(alpha = 1/5, color="black") + plotDefaults
      
      
      plotInputScore2 <- reactive({
        if(!is.null(input$brush)){
          points = brushedPoints(scoredata, input$brush, xvar=PCX(), yvar = PCY())
          Yhat.m = melt(as.matrix(points[,-c(1:fda.obj$npc)]))
          
        }else{
          Yhat.m = as.data.frame(cbind(1, 1:length(fda.obj$mu), fda.obj$mu))
        }
        
        colnames(Yhat.m) <- c("subj", "time", "value")
        p2 <- baseplot+geom_line(data= Yhat.m, aes(x=as.numeric(time), y=value, group = subj), color="cornflowerblue")
      })
      
      callModule(tabPanelModule, "scoreplots", plotObject = plotInputScore1, plotName = "scoreplots", plotObject2 = plotInputScore2)
      

    } ## end server
  )
}

