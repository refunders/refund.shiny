#' modularized UI for creating a new tab with two plots and brushing
#'
#' Creates a UI tab with two plots mutually interactive brushable plots and standardized layout. 
#' To create a UI tab with one plots, see \code{\link{tabPanelModuleUI}}
#'
#' @param id Name of module. Allows each call of this module to be uniquely identified.
#' @param tabTitle Title of the tab, visible in UI
#' @param icon Optional icon to appear on the tab. This attribute is only valid when using a tabPanel within a navbarPage.
#' @param helperText1 Optional help text for the tab.
#' @param helperText2 Optional help text for the tab.
#' @param fda.obj Name of analysis object fed into the overall function for example, fpca.obj.
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
brushTabPanelModuleUI <- function(id, tabTitle, icon = NULL, helperText1 = NULL, helperText2 = NULL, fda.obj){
  ns <- NS(id)
  tabPanel(tabTitle, icon = icon,
           fluidRow(
             column(3,
                    helpText(helperText1), 
                    hr(),
                    selectInput(ns("PCX"), label = ("Select X-axis FPC"), choices = 1:fda.obj$npc, selected = 1),
                    selectInput(ns("PCY"), label = ("Select Y-axis FPC"), choices = 1:fda.obj$npc, selected = 2)
             ),
             column(9,
                    h4(tabTitle), 
                    plotOutput(ns("plot1")),
                    brush=brushOpts(
                      id = ns("brush"), ## probably can do this differently as well. maybe be able to call the brush outside of module though
                      resetOnNew = TRUE)
             )
           ),
           fluidRow(
             column(3, hr(),
                    helpText(helperText2)
             ),
             column(9,
                    plotOutput(ns("plot2"))
             )
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
#' @param fda.obj Name of analysis object fed into the overall function for example, fpca.obj.
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
brushTabPanelModule <- function(input,output, session, fda.obj){
  plotDefaults = list(theme_bw(), xlab(xlab), ylab(ylab), ylim(c(range(fda.obj$Yhat)[1], range(fda.obj$Yhat)[2])),
                      scale_x_continuous(breaks = seq(0, length(fda.obj$mu)-1, length=6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1))) )
  
  
  scoredata = as.data.frame(cbind(fda.obj$scores, fda.obj$Yhat))
  colnames(scoredata) = c(paste0("PC", 1:fda.obj$npc), paste0("subj", 1:dim(fda.obj$Yhat)[2]))
  
  ## get PCs selected for X and Y axis
  PCX <- reactive({ paste0("PC", input$PCX) })
  PCY <- reactive({ paste0("PC", input$PCY) })
  
  
  ## first score plot
  output$plot1 <- renderPlot({
    ggplot(scoredata, aes_string(x = PCX(), y = PCY()))+geom_point(color = "blue", alpha = 1/5, size = 3)+theme_bw()+
      xlab(paste("Scores for FPC", input$PCX))+ylab(paste("Scores for FPC", input$PCY))
  })

  ### second score plot
  Yhat.all.m = melt(fda.obj$Yhat)
  colnames(Yhat.all.m) = c("subj", "time", "value")
  baseplot = ggplot(Yhat.all.m, aes(x=time, y=value, group = subj)) + geom_line(alpha = 1/5, color="black") + plotDefaults
  
  output$plot2 <- renderPlot({
    
    if(!is.null(input$brush)){
      points = brushedPoints(scoredata, input$brush, xvar=PCX(), yvar = PCY())
      Yhat.m = melt(as.matrix(points[,-c(1:fda.obj$npc)]))
      
    }else{
      Yhat.m = as.data.frame(cbind(1, 1:length(fda.obj$mu), fda.obj$mu))
    }
    
    colnames(Yhat.m) <- c("subj", "time", "value")
    baseplot+geom_line(data= Yhat.m, aes(x=as.numeric(time), y=value, group = subj), color="cornflowerblue")
  })

}


