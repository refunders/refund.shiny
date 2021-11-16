#' Interactive Plotting for Functional Linear Concurrent regression
#'
#' Produces an interactive plot illustrating a functional linear concurrent
#' regression analysis.
#'
#' @param obj fosr object to be plotted.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param ... additional arguments passed to plotting functions
#'
#' @author Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu},
#' Julia Wrobel \email{julia.wrobel@@cuanschutz.edu}
#'
#' @seealso \code{\link{plot_shiny}}
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom stats model.matrix terms
#'
#' @export

#utils::globalVariables(c("value", "subj", "covariate"))


plot_shiny.flcm = function(obj, xlab = "", ylab="", title = "", ...) {

  flcm.obj <- obj

  ### NULLify global values called in ggplot
  value = subj = covariate = UB = LB = residual = depth.rank = coef = index = x = y = NULL

  ################################
  ## code for processing tabs
  ################################

  p = dim(flcm.obj$beta.pm)[2] - 1

  ## Tab 1: covariate choice
  covar.list = names(attributes(terms(flcm.obj$terms))$dataClasses)
  covarInputValues = 1:length(covar.list)
  names(covarInputValues) = covar.list
  observed.help = "Observed response and covariate curves."
  observed.call = eval(call("selectInput", inputId = "CovarChoice", label = ("Select Covariate"), choices = covarInputValues, selected = 1))

  ## Tab 3: coefficient functions
  coef.list = colnames(model.matrix(flcm.obj$terms, flcm.obj$data.model[1,]))
  coefInputValues = 1:(p + 1)
  names(coefInputValues) = c("Show all", coef.list)
  coef.help = "Coefficient function for the predictor selected below."
  coef.call = eval(call("selectInput", inputId = "CoefChoice", label = ("Select Predictor"), choices = coefInputValues, selected = 1))

  ## Tab 4: plot of residual curves
  residuals.help = "Plot of residual curves. For a more in-depth look at residuals, use plot_shiny for the fpca.obj included in the model
                    output."
  residuals.call = NULL

  #################################
  ## App
  #################################

  shinyApp(

  #################################
  ## UI
  #################################

    ui = navbarPage(title = strong(style = "color: #ACD6FF; padding: 0px 0px 10px 10px; opacity: 0.95; ", "FoSR Plot"),
                    windowTitle = "refund.shiny", collapsible = FALSE, id = "nav", inverse = TRUE, header = NULL,
                    ##### start tabs
                    tabPanelModuleUI("observed", tabTitle = "Observed Data", icon("stats", lib = "glyphicon"), calls = observed.call,
                                     helperText = observed.help, twoPlots = TRUE, title2 = "Lasagna Plot"),
                    tabPanelModuleUI("coef", tabTitle = "Coefficient Functions", icon("area-chart"), calls = coef.call, helperText = coef.help),
                    tabPanelModuleUI("residuals", tabTitle = "Residuals", icon("medkit"), calls = residuals.call,helperText = residuals.help )
                    ##### end tabs
                    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

      #################################
      ## Code for observed data tab
      #################################

      plotInputObsData <- reactive({

        CovarChoice = as.numeric(input$CovarChoice)
        selected = covar.list[CovarChoice]
        data_tab_plot_df = data.frame(id = flcm.obj$data.model[[flcm.obj$id.var]],
                             index = flcm.obj$data.model[[flcm.obj$time.var]],
                             value = flcm.obj$data.model[[selected]])

        data_tab_plot <- ggplot(data_tab_plot_df, aes(x = index, y = value, group = id)) + geom_line(alpha = .3) +
            theme_bw() + xlab("") + ylab("") + theme(legend.position = "bottom", legend.title = element_blank())

      })

      plotInputLasagna <- reactive({
        ## this is where we will add code for lasagna plot
        df = data.frame(x = 1:100, y = 1:100)
        p <- ggplot(df, aes(x = x, y = y)) + geom_point(color = rainbow(100)) + theme_bw()
      })

      callModule(tabPanelModule, "observed", plotObject = plotInputObsData, plotName = "observed", plotObject2 = plotInputLasagna)


      #################################
      ## Code for CoefFunc Tab
      #################################

      plotInputCoefFunc <- reactive({

        CoefChoice = as.numeric(input$CoefChoice)
        selected = c("Show all", coef.list)[CoefChoice]
        if (selected == "Show all") {
          coef_tab_plot_df <- gather_(flcm.obj$beta.pm, key_col = "id", value_col = "value", gather_cols = coef.list) %>%
            rename_(.dots = setNames(list(flcm.obj$time.var), "index")) %>%
            arrange(id, index)

          coef_tab_plot <- ggplot(coef_tab_plot_df, aes(x = index, y = value, group = id)) +
            geom_line(linetype = 1, alpha = .5, color = "black") +
            theme_bw() + xlab("") + ylab("")
        } else {
          coef_tab_plot_df <- data.frame(index = flcm.obj$beta.pm[[flcm.obj$time.var]],
                                         value = flcm.obj$beta.pm[[selected]]) %>% arrange(index)

          coef_tab_plot <- ggplot(coef_tab_plot_df, aes(x = index, y = value)) +
            geom_line(linetype = 1, lwd = 1.5, color = "black") +
            theme_bw() + xlab("") + ylab("")
        }

      })

      callModule(tabPanelModule, "coef", plotObject = plotInputCoefFunc, plotName = "coef")

      #################################
      ## Code for Residual plot
      #################################

      response = flcm.obj$data.model[,names(attributes(terms(flcm.obj$terms))$dataClasses)[1]]
      resid = response - flcm.obj$Yhat
      resid_tab_plot_df = data.frame(id = flcm.obj$data.model[[flcm.obj$id.var]],
                                     index = flcm.obj$data.model[[flcm.obj$time.var]],
                                     value = response - flcm.obj$Yhat)

      plotInputResid <- reactive({
        residPlot = ggplot(resid_tab_plot_df, aes(x = index, y = value, group = id)) +
          theme_bw() + geom_line(alpha = .3, color = "black") + xlab("") + ylab("")
      })

      callModule(tabPanelModule, "residuals", plotObject = plotInputResid, plotName = "residuals")

      ## add subject number

    } ## end server
  )
}

