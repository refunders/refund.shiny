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
#'
#' @export
#'
plot_shiny.registration = function(obj, xlab = "", ylab="", title = "", ...){
  reg.obj <- obj$reg_obj
  fpca.obj <- obj$fpca_obj

  Y <- reg.obj$Y
  Y$t.star <- obj$time_warps[[1]]
  Y$t.hat <- Y$index

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
    Each row is a subject. Dark and light blue represent values of 0 and 1, respectively."

  ## loss
  loss.help = "Loss is calculated with respect to mean from previous iteration FPCA decomposition."

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
                               tabPanelModuleUI("warps", tabTitle = "warping functions"),
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
        warps = ggplot(Y, aes(x = t.star, y = t.hat, group = id)) + theme_bw() +
          labs(x = "observed time", y = "registered time") +
          geom_path()
      })

      callModule(tabPanelModule, "warps", plotObject = plotInputWarps, plotName = "warps")


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
