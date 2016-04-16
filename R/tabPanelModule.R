#' modularized UI for creating a new tab
#'
#' Creates a UI tab with helptext, widgets for user input, a plot, and standardized layout. This function creates a tab with one plot. 
#' To create a UI tab with two plots mutually interactive brushable plots, see \code{\link{brushTabPanelModuleUI}}
#'
#' @param id Name of module. Allows each call of this module to be uniquely identified.
#' @param tabTitle Title of the tab, visible in UI
#' @param icon Optional icon to appear on the tab. This attribute is only valid when using a tabPanel within a navbarPage.
#' @param calls Unevaluated expression that stores Shiny widgets (for example, a call to a sliderInput function) for the tab. 
#' @param helperText Optional help text for the tab.
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
tabPanelModuleUI <- function(id, tabTitle, icon = NULL, calls = NULL, helperText = NULL){
  ns <- NS(id)
  
  tabPanel(tabTitle, icon = icon,
           column(3,
                  helpText(helperText), 
                  hr(),
                  eval(calls), 
                  if(is.null(calls)){}, ## fix this so it doesn't give a warning!
                  downloadModuleUI(ns("download")) 
           ),
           column(9,
                  h4(tabTitle), ## we may end  up wanting to personalize this a little more
                  plotOutput(ns("plot"))
           )
  )
  
}

#' download Plot as PDF or ggplot Object, modularized server
#'
#' Internal method that creates UI with buttons to download a plot as a PDF or ggplot object.
#'
#' @param input gets user input from UI
#' @param output designates output for UI.
#' @param session Shiny variable for server modules.
#' @param plotObject Reactive plot object defined elsewhere in the server function.
#' @param plotName Character string designating name of the plot for PDF output.
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
tabPanelModule <- function(input,output, session, plotObject = NULL, plotName = NULL){
  
  output$plot <- renderPlot(
    print(plotObject())
  )
  # maybe return the user input stuff so you can use it later
  #callModule(downloadModule, "download", plot = plotObject, title = plotName)
}