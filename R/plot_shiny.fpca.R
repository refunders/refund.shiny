#' Interactive Plotting for Functional Principal Component Analysis
#'
#' Produces an interactive plot illustrating a functional principal component
#' analysis.
#'
#' @param obj fpca object to be plotted.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param thin_data If TRUE data is thinned for each subject to make plotting faster. Defaults to FALSE.
#' @param ... additional arguments passed to plotting functions
#'
#' @author Julia Wrobel \email{julia.wrobel@@cuanschutz.edu},
#' Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu}
#'
#' @seealso \code{\link{plot_shiny}}
#' @importFrom reshape2 melt
#' @importFrom plotly ggplotly event_data layout as.widget
#'
#' @export
#' @return No object is returned. This function takes in objects of class 'fpca' and outputs a shiny application for that object.
#'
plot_shiny.fpca = function(obj, xlab = "", ylab="", title = "", thin_data = FALSE, ...) {

  fpca.obj <- obj

  ## NULLify global values called in ggplot
  PCX = PCY = V1 = V2 = k = lambda = value = subj = index = pop_mean = yhat_inv_link = Y.hat = NULL

  ## establish inverse link function for plotting
  inv_link = createInvLink(family <- fpca.obj$family)

  ################################
  ## convert matrices storing functional data to dfs
  ################################

  if (is.matrix(fpca.obj$Y)) {
  	Y = as_refundObj(fpca.obj$Y, index = fpca.obj$argvals)
  } else {
  	Y = fpca.obj$Y
  }

  if (is.matrix(fpca.obj$Yhat)) {
  	Yhat_df = as_refundObj(fpca.obj$Yhat, index = fpca.obj$argvals)
  } else {
  	Yhat_df = fpca.obj$Yhat
  }

  Yhat_df = mutate(Yhat_df,
                   yhat_inv_link = inv_link(value),
                   pop_mean = rep(inv_link(fpca.obj$mu), length.out = dim(Yhat_df)[1]))

  if(thin_data){
    Y = thin_functional_data(Y)
    Yhat_df = thin_functional_data(Yhat_df)
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
  subjects.call = eval(call("selectInput", inputId = "subject", label = ("Select Subject"), choices = unique(Y$id), selected = unique(Y$id)[1]))

  ## Tab 5: score plots
  score.help1 = "Use the drop down menus to select FPCs for the X and Y axis. Plot shows observed score
                                             scatterplot for selected FPCs; click and drag on the scatterplot to select subjects."
  score.help2 = "Blue curves are fitted values for all subjects. Orange curves correspond to subjects
                                                  selected in the graph above."

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
                    tabPanelModuleUI("LinCom", tabTitle = "Linear Combinations", icon = icon("chart-line"), calls = LinCom.call,
                                     helperText = LinCom.help ),
                    tabPanelModuleUI("subjects",tabTitle = "Subject Fits", icon = icon("user"), calls = subjects.call,
                                     helperText = subjects.help ),
                    tabPanelModuleUI("scoreplots",tabTitle = "Score Scatterplot", icon = icon("binoculars"), calls = score.call,
                                     helperText = score.help1, twoPlots = TRUE, helperText2 = score.help2, is.plotly = TRUE)
                    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

      #################################
      ## Code for mu PC plot
      #################################

      fpca.obj$Y = Y
      fpca.obj$Yhat = Yhat_df
      plotInputMuPC <- reactive({
        PCchoice = as.numeric(input$PCchoice)

        if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian") && input[["muPC_scale"]] == "Response") {
          response_scale = TRUE
        }else{
          response_scale = FALSE
        }

        p1 <- make_muPC(fpca.obj, PCchoice, response_scale)
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

        if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian") && input[["lincom_scale"]] == "Response") {
          response_scale = TRUE
        }else{
          response_scale = FALSE
        }

        p3 <- make_linCom(fpca.obj, PCweights, response_scale)

      })

      callModule(tabPanelModule, "LinCom", plotObject = plotInputLinCom, plotName = "LinCom")

      #################################
      ## Code for subject plots
      #################################

      plotInputSubject <- reactive({
        subjectnum = as.numeric(input$subject)

        Y_sub = filter(Y, id == subjectnum)

        p4 = ggplot(Y_sub, aes(index, value)) +
          geom_point(color = "blue", alpha = 1/3) +
          geom_line(data = filter(Yhat_df, id == subjectnum),
                    aes(y = pop_mean), lwd = 0.5, color = "gray") +
          geom_line(data = filter(Yhat_df, id == subjectnum),
                    aes(y = yhat_inv_link), size = 1, color = "cornflowerblue") +
          theme_bw() + xlab(xlab) + ylab(ylab) +
          ylim(min(fpca.obj$Y$value), max(fpca.obj$Y$value))
      })

      callModule(tabPanelModule, "subjects", plotObject = plotInputSubject, plotName = "subjects")


      #################################
      ## Code for score plots
      #################################

      scoredata = as.data.frame(cbind(fpca.obj$scores))
      colnames(scoredata) = c(paste0("PC", 1:fpca.obj$npc))
      scoredata = mutate(scoredata, id = unique(Y$id))

      scoredata_new <- reactive({
        PCX_num = as.numeric(input$PCX)
        PCY_num = as.numeric(input$PCY)
        data.frame(PCX = scoredata[, PCX_num], PCY = scoredata[, PCY_num], id = scoredata$id)
      })

      ## Tab 5 Plot
      scoreplot1Input <- reactive({
        key = scoredata_new()$id
        p = plot_ly(data = scoredata_new(), x = ~PCX, y = ~PCY, type = "scatter",
                    mode = 'markers', source = "scoreplot", key = ~key,
                    hoverinfo = 'text', text = ~paste('Id: ', id)) %>%
          layout(dragmode = "select")
        p$elementId <- NULL
        p
      })

      ### second score plot
      baseplot = plot_ly(data = group_by(Yhat_df, id), x = ~index, y = ~value, type = "scatter",
                         mode = 'lines', alpha = 0.15, hoverinfo = 'text', text = ~paste('Id: ', id))

      scoreplot2Input <- reactive({
        clicked <- event_data("plotly_click", source = "scoreplot")
        brushed <- event_data("plotly_selected", source = "scoreplot")
        selected_ids = c(clicked$key, brushed$key)

        if(!is.null(selected_ids)){
          Y.clicked = filter(Yhat_df, id %in% selected_ids)

          baseplot = baseplot %>%
            add_trace(data = group_by(Y.clicked, id), x = ~index, y = ~value, mode = 'lines', alpha = 0.75)
        }
        baseplot$elementId <- NULL
        baseplot %>% layout(showlegend = FALSE)

      })


      callModule(tabPanelModule, "scoreplots", plotObject = scoreplot1Input, plotName = "scoreplots", plotObject2 = scoreplot2Input, is.plotly = TRUE)

    } ## end server
  )
}

