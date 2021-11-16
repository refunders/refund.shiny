#' Interactive Plotting for Registration Objects
#'
#' Produces an interactive plot illustrating functional data before and after registration.
#' Our registration method uses FPCA, the FPCA is plotted as well.
#'
#' @param obj registration object to be plotted.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param thin_data If TRUE data is thinned for each subject to make plotting faster. Defaults to FALSE.
#' @param ... additional arguments passed to plotting functions
#'
#' @author Julia Wrobel \email{julia.wrobel@@cuanschutz.edu}
#'
#' @seealso \code{\link{plot_shiny}}
#' @importFrom gridExtra grid.arrange
#' @importFrom plotly plot_ly event_data layout add_trace
#'
#' @export
plot_shiny.registration = function(obj, xlab = "", ylab="", title = "", thin_data = FALSE, ...){

  fpca.obj <- obj$fpca_obj

  ## NULLify global values called in ggplot
  iteration = value = tstar = t_hat = index = pop_mean = Y.hat = yhat_inv_link = NULL

  ## establish inverse link function for plotting
  inv_link = createInvLink(family <- fpca.obj$family)

  ## data management. Probably should think about generalizing this to other distributions.
  Y = obj$Y
  Y = mutate(Y, pop_mean = rep(inv_link(fpca.obj$mu), length.out = dim(Y)[1]),
             Y.hat = fpca.obj$Yhat$value,
             yhat_inv_link = inv_link(Y.hat))

  if(thin_data){
    Y = thin_functional_data(Y)
  }
  ################################
  ## code for processing tabs
  ################################

  #### registration
  ## curves
  curves.help =if(obj$family == "gaussian"){
    paste0("Unregistered (left) and registered (right) data from the ", obj$family, " exponential family.
    Each curve is a subject.")
  }else{
    paste0("Unregistered (left) and registered data (right) from the ", obj$family, " exponential family.
    Each row is a subject. Light and dark blue represent values of 0 and 1, respectively.")
  }

  ## warps
  warp.help1 = "Plot shows warping functions for all subjects; click on a specific curve to select a subject."
  warp.help2 = "Plot shows observed data and fitted values for selected subject.
  Green curve is population mean.
  If no subjects are selected then first subject in dataset is shown."

  #### fpca

  # muPC plot
  muPC.help = "Solid black line indicates population mean. For the FPC selected below, blue and red lines
               indicate the population mean +/- the FPC times 2 SDs of the associated score distribution."
  muPC.call = as.list(NA)
  muPC.call[[1]] = selectInput(inputId = "PCchoice", label = ("Select FPC"), choices = 1:fpca.obj$npc, selected = 1)

  ## add y-axis scale input if family is not gaussian
  if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian")) {
    muPC.call[[2]] = selectInput("muPC_scale", label = ("Select Y-axis Scale"), choices = c("Natural", "Response"), selected = "Natural")
  }

  # subject fits plot
  subjects.help = "Plot shows observed data and fitted values for the subject selected below."
  subjects.call = eval(call("selectInput", inputId = "subject", label = ("Select Subject"),
                            choices = unique(Y$id), selected = unique(Y$id)[1]))

  # scoreplot (need to edit this in both versions)
  scoredata = as.data.frame(fpca.obj$scores)
  colnames(scoredata) = c(paste0("PC", 1:fpca.obj$npc))
  scoredata = mutate(scoredata, id = unique(Y$id))

  score.help1 = "Plot shows observed score scatterplot for first and second FPC; click on the scatterplot to select a subject."
  score.help2 = "Plot shows observed data and fitted values for selected subject.
  Green curve is population mean."

  #################################
  ## App
  #################################

  shinyApp(

    #################################
    ## UI
    #################################
    ui = navbarPage(title = strong(style = "color: #ACD6FF; padding: 0px 0px 10px 10px; opacity: 0.95; ", "Registration Plot"),
                    windowTitle = "refund.shiny", collapsible = FALSE, id = "nav", inverse = TRUE, header = NULL,
                    ##### start tabs
                    tabPanel("registration", icon = icon("transfer", lib = "glyphicon"),
                             tabsetPanel(
                               tabPanelModuleUI("curves", tabTitle = "registered curves", helperText = curves.help),
                               tabPanelModuleUI("warps", tabTitle = "warping functions", helperText = warp.help1,
                                                twoPlots = TRUE, is.plotly = TRUE, helperText2 = warp.help2)
                             )
                             ),
                    tabPanel("fpca", icon = icon("stats", lib = "glyphicon"),
                             tabsetPanel(
                               tabPanelModuleUI("muPC", tabTitle = "Mean +/- FPCs", icon("stats", lib = "glyphicon"),
                                                calls = muPC.call, helperText = muPC.help ),
                               tabPanelModuleUI("subjects",tabTitle = "Subject Fits", icon = icon("user"), calls = subjects.call,
                                                helperText = subjects.help ),
                               tabPanelModuleUI("scoreplots",tabTitle = "Score Scatterplot", icon = icon("binoculars"), calls = NULL,
                                                helperText = score.help1, twoPlots = TRUE, helperText2 = score.help2, is.plotly = TRUE)
                             )
                             ) # end tabPanel
    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

      #################################
      ## Code for curves plot
      #################################
      plotInputCurves <- reactive({
        if(family == "binomial"){
          curvesPlots = registerLasagna(Y)
          grid.arrange(curvesPlots[[1]],curvesPlots[[2]], ncol = 2)
        }else if(family == "gaussian"){
          unreg = ggplot(Y, aes(x = tstar, y = value, group = id)) +
            geom_path(alpha = .25) + theme_bw() +
            labs(x = "t_star", y = "Prob(Y = 1)")

          reg = ggplot(Y, aes(x = t_hat, y = value, group = id)) +
            geom_path(alpha = .25) + theme_bw() +
            labs(x = "t_hat", y = "Prob(Y = 1)")

          grid.arrange(unreg, reg, ncol = 2)
        }else{
          stop("Package currently handles only 'binomial' or 'gaussian' families.")
        }

      })

      callModule(tabPanelModule, "curves", plotObject = plotInputCurves, plotName = "curves", is.grid = TRUE)

      #################################
      ## Code for plot of warping functions
      #################################

      plotInputWarps <- reactive({
        key = Y$id
        p = plot_ly(data = group_by(Y, id), x = ~t_hat, y = ~tstar, type = "scatter",
                mode = 'lines', alpha = 0.5, source = "timewarps", key = ~key,
                hoverinfo = 'text', text = ~paste('Id: ', id)) %>% layout(dragmode = "select")

        p$elementId <- NULL
        p
      })


      # for selected subjects plot observed data and fitted value
      plotInputWarpSelect <- reactive({
        clicked <- event_data("plotly_click", source = "timewarps")

        if(!is.null(clicked)){
          Y.clicked = filter(Y, id %in% clicked$key)
          p = plot_ly(data = group_by(Y.clicked, id), x = ~t_hat, y = ~value, type = "scatter",
                  alpha = 0.25, mode = 'markers') %>%
            add_trace(y = ~yhat_inv_link, mode = 'lines') %>%
            add_trace(x = ~tstar, y = ~pop_mean, mode = 'lines') %>%
            layout(dragmode = "select", showlegend = FALSE)

          p$elementId <- NULL
          p

        }else{
          p = plot_ly(data = filter(Y, id == first(Y$id)), x = ~t_hat, y = ~value,
                      type = "scatter",
                  alpha = 0.25, mode = 'markers') %>%
            add_trace(y = ~yhat_inv_link, mode = 'lines') %>%
            add_trace(x = ~tstar, y = ~pop_mean, mode = 'lines') %>%
            layout(dragmode = "select", showlegend = FALSE)

          p$elementId <- NULL
          p
        }
      })

      callModule(tabPanelModule, "warps", plotObject = plotInputWarps, plotName = "warps",
                 plotObject2 = plotInputWarpSelect, is.plotly = TRUE)


      #################################
      ## Code for plot of fpca mean +/- PCs
      #################################

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
      ## Code for fpca subject plots
      #################################

      plotInputSubject <- reactive({
        subjectnum = as.numeric(input$subject)

        Y_sub = filter(Y, id == subjectnum)

        p4 = ggplot(Y_sub, aes(index, pop_mean)) +
          geom_line(lwd = 0.5, color = "gray") +
          geom_line(aes(y = yhat_inv_link), size = 1, color = "cornflowerblue") +
          geom_point(aes(y = value), color = "blue", alpha = 1/3) +
          theme_bw() + xlab(xlab) + ylab(ylab)
      })

      callModule(tabPanelModule, "subjects", plotObject = plotInputSubject, plotName = "subjects")

      #################################
      ## Code for fpca score plots
      #################################
      ## score plots
      if(fpca.obj$npc == 1){scoredata$PC2 = scoredata$PC1}

      plotInputScore <- reactive({
        key = scoredata$id
        p = plot_ly(data = scoredata, x = ~PC1, y = ~PC2, type = "scatter",
                    mode = 'markers', source = "scoreplot", key = ~key,
                    hoverinfo = 'text', text = ~paste('Id: ', id)) %>%
          layout(dragmode = "select")
        p$elementId <- NULL
        p
      })


      plotInputScoreSelect <- reactive({
        clicked <- event_data("plotly_click", source = "scoreplot")

        if(!is.null(clicked)){
          Y.clicked = filter(Y, id %in% clicked$key)
          p = plot_ly(data = group_by(Y.clicked, id), x = ~t_hat, y = ~value, type = "scatter",
                      alpha = 0.25, mode = 'markers') %>%
            add_trace(y = ~yhat_inv_link, mode = 'lines') %>%
            add_trace(x = ~tstar, y = ~pop_mean, mode = 'lines') %>%
            layout(dragmode = "select", showlegend = FALSE)

          p$elementId <- NULL
          p

        }else{
          p = plot_ly(data = filter(Y, id == first(Y$id)), x = ~t_hat,
                      y = ~value, type = "scatter",
                      alpha = 0.25, mode = 'markers') %>%
            add_trace(y = ~yhat_inv_link, mode = 'lines') %>%
            add_trace(x = ~tstar, y = ~pop_mean, mode = 'lines') %>%
            layout(dragmode = "select", showlegend = FALSE)

          p$elementId <- NULL
          p
        }
      })


      callModule(tabPanelModule, "scoreplots", plotObject = plotInputScore,
                 plotObject2 = plotInputScoreSelect, is.plotly = TRUE)

    } # end server



  ) # end shiny app

} # end overall funtion
