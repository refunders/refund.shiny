#' download Plot as PDF or ggplot Object, modularized UI
#'
#' Internal method that creates UI with buttons to download a plot as a PDF or ggplot object.
#'
#' @param id name of module. Allows each call of this module to be uniquely identified.
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
downloadModuleUI <-function(id) {
ns <- NS(id)
tagList(
  downloadButton(ns("downloadPDF"), "Download Plot as PDF"), br(), br(),
  downloadButton(ns("downloadPlot"), "Download Plot as Object", class = "plot-download")
)
}

#' download Plot as PDF or ggplot Object, modularized server
#'
#' Internal method that creates UI with buttons to download a plot as a PDF or ggplot object.
#'
#' @param input gets user input from UI
#' @param output designates output for UI
#' @param session Shiny variable for server modules
#' @param plotObject Reactive plot object defined elsewhere in the server function.
#' @param plotName Character string designating name of the plot for PDF output.
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
downloadModule <- function(input, output, session, plotObject, plotName){
  ## actual plot is defined elsewhere
  output$downloadPDF <- savePDF(paste0(plotName, ".pdf"), plotObject())
  output$downloadPlot <- savePlot(paste0(plotName, ".RData"), plotObject())
}
