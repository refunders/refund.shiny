#' Interactive Plotting for Registration Objects
#'
#' Produces an interactive plot illustrating functional data before and after registration.
#' Our registration method uses FPCA, the FPCA is plotted as well.
#'
#' @param obj registration object to be plotted.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param ... additional arguments passed to plotting functions
#'
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu},
#' Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu}
#'
#' @seealso \code{\link{plot_shiny}}
#' @importFrom gridExtra grid.arrange
#' @importFrom plotly plot_ly ggplotly event_data layout
#'
#' @export
#'
plot_shiny.registration = function(obj, xlab = "", ylab="", title = "", ...){
  reg.obj <- obj$reg_obj
  fpca.obj <- obj$fpca_obj

  ## data management. Probably should think about generalizing this to other distributions.
  Y <- reg.obj$Y
  Y$t.star <- obj$time_warps[[1]]
  Y$t.hat <- Y$index
  Y$Y.hat <- fpca.obj$Yhat$value
  Y$pi.hat <- inv.logit(Y$Y.hat)

  ## establish inverse link function for plotting
  inv_link = createInvLink(family = fpca.obj$family)

  ## add y-axis scale input if family is not gaussian
  #if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian")) {
    #muPC.call[[2]] = selectInput("muPC_scale", label = ("Select Y-axis Scale"), choices = c("Natural", "Response"), selected = "Natural")
    #LinCom.call[[fpca.obj$npc + 1]] = selectInput("lincom_scale", label = ("Select Y-axis Scale"), choices = c("Natural", "Response"), selected = "Natural")
  #}

  ################################
  ## code for processing tabs
  ################################

  #### registration
  ## curves
  curves.help = "Registered (left) and unregistered (right) binary data.
    Each row is a subject. Light and dark blue represent values of 0 and 1, respectively."

  ## loss
  loss.help = "Loss is calculated with respect to mean from previous iteration FPCA decomposition."

  ## warps
  warp.help1 = "Plot shows warping functions for all subjects; click on a specific curve to select a subject."
  warp.help2 = "Plot shows observed data and fitted values for selected subject.
  If no subjects are selected then first subject in dataset is shown."

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
                                                twoPlots = TRUE, is.plotly = TRUE, helperText2 = warp.help2),
                               tabPanelModuleUI("loss", tabTitle = "loss function", helperText = loss.help)
                             )
                             ),
                    tabPanel("fpca", icon = icon("stats", lib = "glyphicon"))
    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

      #################################
      ## Code for curves plot
      #################################
      plotInputCurves <- reactive({
        curvesPlots = registerLasagna(Y)
        grid.arrange(curvesPlots[[1]],curvesPlots[[2]], ncol = 2)
      })

      callModule(tabPanelModule, "curves", plotObject = plotInputCurves, plotName = "curves", is.grid = TRUE)

      #################################
      ## Code for plot of warping functions
      #################################

      plotInputWarps <- reactive({
        key = Y$id
        plot_ly(data = group_by(Y, id), x = ~t.star, y = ~t.hat, type = "scatter",
                mode = 'lines', alpha = 0.5, source = "timewarps", key = ~key,
                hoverinfo = 'text', text = ~paste('Id: ', id)) %>% layout(dragmode = "select")
      })


      # for selected subjects plot observed data and fitted value
      plotInputWarpSelect <- reactive({
        clicked <- event_data("plotly_click", source = "timewarps")


        if(!is.null(clicked)){
          ## might want to look at this plot relative to an average subject (subject with scores closest to zero)
          Y.clicked = filter(Y, id %in% clicked$key)
          plot_ly(data = group_by(Y.clicked, id), x = ~t.star, y = ~value, type = "scatter",
                  alpha = 0.25, mode = 'markers') %>%
            add_trace(y = ~pi.hat, mode = 'lines') %>%
            layout(dragmode = "select", showlegend = FALSE)

        }else{
          plot_ly(data = filter(Y, id == first(Y$id)), x = ~t.star, y = ~value, type = "scatter",
                  alpha = 0.25, mode = 'markers') %>%
            add_trace(y = ~pi.hat, mode = 'lines') %>%
            layout(dragmode = "select", showlegend = FALSE)

          #warps2 = ggplot(filter(Y, id == 1), aes(x = t.star, y = value, group = id)) + theme_bw() +
           # geom_point(color = "indianred", alpha = 0.5, size = 0.5) +
            #geom_path(aes(x = t.hat, y = inv.logit(Y.hat)), color = "cornflowerblue")

          #ggplotly(warps2)

        }



      })

      callModule(tabPanelModule, "warps", plotObject = plotInputWarps, plotName = "warps",
                 plotObject2 = plotInputWarpSelect, is.plotly = TRUE)



      #################################
      ## Code for plot of loss
      #################################

      plotInputLoss <- reactive({
        loss.df = data.frame(loss = obj$loss, iteration = 0:(length(obj$loss) - 1))

        loss = ggplot(loss.df, aes(x = iteration, y = loss)) + theme_bw() +
          labs(x = "iteration number", y = "loss") +
          geom_point(size = 3) + geom_line()
      })

      callModule(tabPanelModule, "loss", plotObject = plotInputLoss, plotName = "loss")

    } # end server



  ) # end shiny app

} # end overall funtion
