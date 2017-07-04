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
#' @importFrom plotly ggplotly event_data layout as.widget
#'
#' @export
#'
plot_shiny.registration = function(obj, xlab = "", ylab="", title = "", ...){
  reg.obj <- obj$reg_obj
  fpca.obj <- obj$fpca_obj

  ################################
  ## code for processing tabs
  ################################


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
                    tabPanel("registration", icon = icon("stats", lib = "glyphicon"),
                             tabPanelModuleUI("results", tabTitle = "results"),
                             tabPanelModuleUI("warps", tabTitle = "warping functions")
                             ),
                    tabPanel("fpca")
    ),

    #################################
    ## Server
    #################################

    server = function(input, output){

    } # end server



  ) # end shiny app

} # end overall funtion
