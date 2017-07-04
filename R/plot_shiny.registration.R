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

}
