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
#' @importFrom reshape2 melt
#'
#' @export
#'
plot_shiny.fpca = function(obj, xlab = "", ylab="", title = "", ...) {

  fpca.obj <- obj

  ## NULLify global values called in ggplot
  V1 = V2 = k = lambda = value = subj = index = NULL

  ## establish inverse link function for plotting
  inv_link = createInvLink(family = fpca.obj$family)

  ################################
  ## convert matrices storing functional data to dfs
  ################################

  if (is.matrix(fpca.obj$Y)) {
  	Y_df = as_refundObj(fpca.obj$Y, index = fpca.obj$argvals)
  } else {
  	Y_df = fpca.obj$Y
  }

  if (is.matrix(fpca.obj$Yhat)) {
  	Yhat_df = as_refundObj(fpca.obj$Yhat, index = fpca.obj$argvals)
  } else {
  	Yhat_df = fpca.obj$Yhat
  }

  ################################
  ## code for processing tabs
  ################################

  ## Tab 1:
  muPC.help = "Solid black line indicates population mean. For the FPC selected below, blue and red lines
               indicate the population mean +/- the FPC times 2 SDs of the associated score distribution."
  muPC.call = as.list(NA)
  muPC.call[[1]] = selectInput(inputId = "PCchoice", label = ("Select FPC"), choices = 1:fpca.obj$npc, selected = 1)

  ## Tab 2: scree plot

  scree.help = "Scree plots; the left panel shows the plot of eigenvalues and the right panel shows the cumulative percent variance explained."
  scree = data.frame(k = rep(1:fpca.obj$npc, 2),
                      lambda = c(fpca.obj$evalues, cumsum(fpca.obj$evalues)/ sum(fpca.obj$evalues)),
                      type = rep(c("Score Variance", "Percent Variance Explained"), each = fpca.obj$npc))

  ## Tab 3: linear combination of PCs
  LinCom.help = "Plot shows the linear combination of mean and FPCs with the scores specified using the sliders below."
  varpercent = lapply(fpca.obj$evalues, function(i){100*round(i/sum(fpca.obj$evalues),3)}) # calculates percent variance explained
  LinCom.call <- as.list(rep(NA, fpca.obj$npc))
  PCs <- rep(NA, fpca.obj$npc)
  for (i in 1:fpca.obj$npc) {

    PCnum = paste("PC", i, sep = "")

    LinCom.call[[i]] =  eval(call("sliderInput", inputId = PCnum, label = paste(PCnum, ": ", varpercent[[i]],  "% Variance", sep = ""),
                            min = -2, max = 2, step = .1, value = 0, post = " SD", animate = animationOptions(interval = 400, loop = T)))

    PCs[i] = PCnum
  }

  ## Tab 4: subject fits
  subjects.help = "Plot shows observed data and fitted values for the subject selected below."
  subjects.call = eval(call("selectInput", inputId = "subject", label = ("Select Subject"), choices = unique(Yhat_df$id), selected = unique(Yhat_df$id)[1]))

  ## Tab 5: score plots
  score.help1 = "Use the drop down menus to select FPCs for the X and Y axis. Plot shows observed score
                                             scatterplot for selected FPCs; click and drag on the scatterplot to select subjects."
  score.help2 = "Black curves are fitted values for all subjects. Blue curves correspond to subjects
                                                  selected in the graph above. If no points are selected, the mean curve is shown."

  score.call = tagList(  selectInput("PCX", label = ("Select X-axis FPC"), choices = 1:fpca.obj$npc, selected = 1),
    selectInput("PCY", label = ("Select Y-axis FPC"), choices = 1:fpca.obj$npc, selected = 2) )

  ## add y-axis scale input if family is not gaussian
  if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian")) {
    muPC.call[[2]] = selectInput("muPC_scale", label = ("Select Y-axis Scale"), choices = c("Natural", "Response"), selected = "Natural")
    LinCom.call[[fpca.obj$npc + 1]] = selectInput("lincom_scale", label = ("Select Y-axis Scale"), choices = c("Natural", "Response"), selected = "Natural")
  }

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
                                     helperText = score.help1, twoPlots = TRUE, helperText2 = score.help2, is.plotly = TRUE)
                                     #brushName = "scorebrush",
                                     
                    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

      ## define global objects
      mu_df = as_refundObj(matrix(fpca.obj$mu, nrow = 1), index = fpca.obj$argvals)
      efunctions = fpca.obj$efunctions
      sqrt.evalues = diag(sqrt(fpca.obj$evalues))
      scaled_efunctions = efunctions %*% sqrt.evalues

      ## prep objects for plotting on response scale; used in subject plot tabs
      mu_df_inv_link = mu_df
      mu_df_inv_link = mutate(mu_df_inv_link, value = inv_link(value))

      Yhat_df_inv_link = Yhat_df
      Yhat_df_inv_link = mutate(Yhat_df_inv_link, value = inv_link(value))

      ## define plot defaults
      ## set y axes to be max(2 SDs from mu of PC1, fitted values)
      max.y = max(fpca.obj$mu + 2 * abs(scaled_efunctions[, 1])) 
      min.y = min(fpca.obj$mu - 2 * abs(scaled_efunctions[, 1])) 
            
      plotDefaults = list(theme = theme_bw(),
                          title = theme(plot.title = element_text(size=22)),
                          xlab = xlab(xlab),
                          ylab = ylab(ylab),
                          ylim = ylim(c(min(min.y, range(Yhat_df$value)[1]), max(max.y,range(Yhat_df$value)[2]))),
                          x_scale = scale_x_continuous(breaks = seq(0, length(fpca.obj$mu) - 1, length = 6), 
                                                       labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1))))

      #################################
      ## Code for mu PC plot
      #################################

      plotInputMuPC <- reactive({
        PCchoice = as.numeric(input$PCchoice)
        scaled_efuncs = scaled_efunctions[,PCchoice]

        df_plus = as_refundObj(matrix(fpca.obj$mu + 2 * scaled_efuncs, nrow = 1), index = fpca.obj$index)
        df_plus$id = 2

        df_minus = as_refundObj(matrix(fpca.obj$mu - 2 * scaled_efuncs, nrow = 1), index = fpca.obj$index)
        df_minus$id = 3

        plot_df = bind_rows(mu_df, df_plus, df_minus) %>%
          mutate(id = as.character(id))

        if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian") && input[["muPC_scale"]] == "Response") {
          plot_df = mutate(plot_df, value = inv_link(value))
          plotDefaults[["ylim"]] = ylim(c(range(Y_df$value)[1], range(Y_df$value)[2]))
        }

        p1 <- ggplot(filter(plot_df, id == "1"), aes(x = index, y = value)) + geom_line(lwd = 1) + plotDefaults +
          geom_point(data = filter(plot_df, id == "2"), color = "blue", size = 4, shape = '+') +
          geom_point(data = filter(plot_df, id == "3"), color = "indianred", size = 4, shape = "-") +
          ggtitle(bquote(psi[.(input$PCchoice)]~(t) ~ "," ~.(100*round(fpca.obj$evalues[as.numeric(input$PCchoice)]/sum(fpca.obj$evalues),3)) ~ "% Variance"))
      })

      callModule(tabPanelModule, "muPC", plotObject = plotInputMuPC, plotName = "muPC")

      #################################
      ## Code for scree plot
      #################################

      plotInputScree <- reactive({
        p2 = screeplots <- ggplot(scree, aes(x = k, y = lambda)) + geom_line(linetype = 1, lwd = 1.5, color = "black") +
          geom_point(size = 4, color = "black") + theme_bw() + xlab("Principal Component") + ylab("") +
          facet_wrap(~type, scales = "free_y") + ylim(0, NA)
      })

      callModule(tabPanelModule, "screeplots", plotObject = plotInputScree, plotName = "screeplots")

      #################################
      ## Code for linear combinations
      #################################

      plotInputLinCom <- reactive({
        PCweights = rep(NA, length(PCs))
        for (i in 1:length(PCs)) {PCweights[i] = input[[PCs[i]]]}
        df = as_refundObj(matrix(fpca.obj$mu, nrow = 1) + t(efunctions %*% sqrt.evalues %*% PCweights),
                          index = fpca.obj$argvals)
        df$id = 2
        plot_df = bind_rows(mu_df, df) %>%
          mutate(id = as.character(id))

        if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian") && input[["lincom_scale"]] == "Response") {
          plot_df = mutate(plot_df, value = inv_link(value))
          plotDefaults[["ylim"]] = ylim(c(range(Y_df$value)[1], range(Y_df$value)[2]))
        }

        p3 <- ggplot(plot_df, aes(x = index, y = value, color = id, lwd = id)) + geom_line() + plotDefaults +
          theme(legend.key = element_blank()) + xlab(xlab) + ylab(ylab) + ggtitle(title) +
          scale_color_manual("Line Legend", values = c("1" = "gray", "2" = "cornflowerblue"), guide = FALSE) +
          scale_size_manual("Line Legend", values = c("1" = 0.75, "2" = 1.3), guide = FALSE)

      })

      callModule(tabPanelModule, "LinCom", plotObject = plotInputLinCom, plotName = "LinCom")

      #################################
      ## Code for subject plots
      #################################

      plotInputSubject <- reactive({
        subjectnum = input$subject

        p4 = ggplot(data = mu_df_inv_link, aes(x = index, y = value)) + geom_line(lwd = 0.5, color = "gray") +
          geom_line(data = filter(Yhat_df_inv_link, id == subjectnum), size = 1, color = "cornflowerblue") +
          geom_point(data = filter(Y_df, id == subjectnum), color = "blue", alpha = 1/3) +
          theme_bw() + xlab(xlab) + ylab(ylab) + ylim(c(range(Y_df$value)[1], range(Y_df$value)[2])) +
          scale_x_continuous(breaks = seq(0, length(fpca.obj$mu) - 1, length = 6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))

      })

      callModule(tabPanelModule, "subjects", plotObject = plotInputSubject, plotName = "subjects")


      #################################
      ## Code for score plots
      #################################

      scoredata = as.data.frame(cbind(fpca.obj$scores))
      colnames(scoredata) = c(paste0("PC", 1:fpca.obj$npc))
      scoredata = mutate(scoredata, id = unique(Yhat_df$id))

      ## get PCs selected for X and Y axis
      PCX <- reactive({ paste0("PC", input$PCX) })
      PCY <- reactive({ paste0("PC", input$PCY) })

      ## Tab 5 Plot
      stuff <- reactive({
        gg1 <- ggplot(scoredata, aes_string(x = PCX(), y = PCY())) + geom_point(color = "blue", alpha = 1/5, size = 3, aes(text = id)) +
          xlab(paste("Scores for FPC", input$PCX)) + ylab(paste("Scores for FPC", input$PCY)) + theme_bw()
        
        ggplotly(gg1, source = "scoreplot") %>% layout(dragmode = "select")
      })

      ### second score plot
      baseplot = ggplot(Yhat_df, aes(x = index, y = value, group = id)) + geom_line(alpha = 1/5, color = "black", aes(text = id)) +
        plotDefaults

      
      stuff2 <- reactive({

        brush <- event_data("plotly_selected", source = "scoreplot")
        
        if(is.null(brush)){
          brushed_subjs = NULL
          baseplot.gg = baseplot
        }else{
          indices = as.numeric(brush$pointNumber)
          brushed_subjs = scoredata$id[indices]
          baseplot.gg = baseplot + geom_line(data = filter(Yhat_df, id %in% brushed_subjs), color = "cornflowerblue")
        }
        
        #if (!is.null(brush)) {
          #points = brushedPoints(scoredata, brush, xvar = PCX(), yvar = PCY())
        #  brushed_subjs = brush$key
        #} else {
        #  brushed_subjs = NULL
        #}

       
        ggplotly(baseplot.gg)

   
      })

      #callModule(tabPanelModule, "scoreplots", plotObject = stuff, plotName = "scoreplots", plotObject2 = stuff2)
      callModule(tabPanelModule, "scoreplots", plotObject = stuff, plotName = "scoreplots", plotObject2 = stuff2, is.plotly = TRUE)

    } ## end server
  )
}

