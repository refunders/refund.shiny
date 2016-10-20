#' modularized UI for creating a new tab
#'
#' Creates a UI tab with helptext, widgets for user input, a plot, and standardized layout. The default is to create one plot, but if the 
#' argument 'twoPlots' is set to TRUE, then the layout allows for two plots, where each can have separate helper text and Shiny widget calls.
#'
#' @param id Name of module. Allows each call of this module to be uniquely identified.
#' @param tabTitle Title of the tab, visible in UI
#' @param icon Optional icon to appear on the tab. This attribute is only valid when using a tabPanel within a navbarPage.
#' @param calls Unevaluated expression that stores Shiny widgets (for example, a call to a sliderInput function) for the tab. 
#' @param helperText Optional help text for the tab.
#' @param twoPlots defaults to FALSE, and layout is generated for one plot. If TRUE, layout is generated for two plots
#' @param calls2 Unevaluated expression that stores Shiny widgets for the (optional) second plot
#' @param helperText2 Optional help text for the (optional) second plot
#' @param title2 plot title for the (optional) second plot
#' @param brushName character vector indicating the name of brush if you want brushing for the plot. For use in score scatterplots 
#' for \code{plot_shiny.fpca()} and \code{plot_shiny.mfpca()}. 
#' @param is.plotly Indicates if plots are plotly generated. Defaults to FALSE.
#' 
#' @importFrom plotly plotlyOutput renderPlotly as.widget
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
tabPanelModuleUI <- function(id, tabTitle, icon = NULL, calls = NULL, helperText = NULL, twoPlots = FALSE, calls2 = NULL, helperText2 = NULL,
                             title2 = NULL, brushName = NULL, is.plotly = FALSE){
  ns <- NS(id)
  
  plot2.layout <- tagList(
    column(3,
           helpText(helperText2), 
           hr(),
           eval(calls2), 
           downloadModuleUI(ns("download2")) 
    ),
    column(9,
           h4(title2), 
           if(is.plotly){
             plotlyOutput(ns("plot2"))
           }else{
             plotOutput(ns("plot2"))
           }
    )
  )

  if(is.plotly){
    plotTag <- tagList(plotlyOutput(ns("plot") ) )
  }else{
    plotTag <- ifelse(is.null(brushName), 
                      tagList(plotOutput(ns("plot") ) ),
                      tagList(plotOutput(ns("plot"), brush = brushOpts(id = brushName, resetOnNew = TRUE))) 
    )
  }
  
  
  
  # set up the tab panel
  tabPanel(tabTitle, icon = icon,
           fluidRow(
             column(3,
                    helpText(helperText), 
                    hr(),
                    eval(calls), 
                    downloadModuleUI(ns("download")) 
             ),
             column(9,
                    h4(tabTitle), 
                    plotTag
             )
           ),
           fluidRow(
             if(twoPlots){
               plot2.layout
             }
           ) 
  ) ## end tabPanel
  
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
#' @param plotObject2 Reactive plot object for the (optional) second plot.
#' @param plotName2 Character string designating name of the (optional) second plot for the PDF output
#' @param is.plotly Indicates if plots are plotly generated. Defaults to FALSE.
#' 
#' @importFrom plotly plotlyOutput renderPlotly as.widget
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
tabPanelModule <- function(input,output, session, plotObject = NULL, plotName = NULL, plotObject2 = NULL, plotName2 = NULL,
                           is.plotly = FALSE){
  if(is.plotly){
    output$plot <- renderPlotly(
      print(plotObject())
    )
    
    output$plot2 <- renderPlotly(
      print(plotObject2())
    )
    
    
  }else{
    output$plot <- renderPlot(
      print(plotObject())
    )
    
    output$plot2 <- renderPlot(
      print(plotObject2())
    )
  }
  
  #callModule(downloadModule, "download", plot = plotObject, title = plotName)
}
