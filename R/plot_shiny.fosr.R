#' Interactive Plotting for Functional-on-Scalar Regressions
#'
#' Produces an interactive plot illustrating a function-on-scalar
#' regression analysis.
#'
#' @param obj fosr object to be plotted.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param ... additional arguments passed to plotting functions
#'
#' @author Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu},
#' Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
#' @seealso \code{\link{plot_shiny}}
#' @import shiny
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats model.matrix terms
#'
#' @export

#utils::globalVariables(c("value", "subj", "covariate"))


plot_shiny.fosr = function(obj, xlab = "", ylab="", title = "", ...) {

  fosr.obj <- obj

  ### NULLify global values called in ggplot
  value = subj = covariate = UB = LB = residual = depth.rank = coef = grid = x = y = NULL

  ################################
  ## code for processing tabs
  ################################

  p = dim(fosr.obj$beta.hat)[1]
  D = dim(fosr.obj$beta.hat)[2]
  grid = 1:D

  ## Tab 1: covariate choice
  covar.list = names(attributes(terms(fosr.obj$terms))$dataClasses)
  covar.list[1] = "None"
  covarInputValues = 1:length(covar.list)
  names(covarInputValues) = covar.list
  observed.help = "Observed response data, colored according to the covariate selected below."
  observed.call = eval(call("selectInput", inputId = "CovarChoice", label = ("Select Covariate"), choices = covarInputValues, selected = 1))
  
  
  ## Tab 2: fitted values
  pred.list = names(attributes(terms(fosr.obj$terms))$dataClasses)[-1]
  fitted.help = "Fitted response curve for a subject with covariate values specified below."
  fitted.call <- vector("list", length(pred.list))
  for(i in 1:length(pred.list)){
    fitted.call[[i]] =  eval(createInputCall(pred.list[i], get(pred.list[i], fosr.obj$data) ))
  }

  ## Tab 3: coefficient functions
  coef.list = colnames(model.matrix(fosr.obj$terms, fosr.obj$data[1,]))
  coefInputValues = 1:p
  names(coefInputValues) = coef.list
  coef.help = "Coefficient function and confidence bounds for the predictor selected below."
  coef.call = eval(call("selectInput", inputId = "CoefChoice", label = ("Select Predictor"), choices = coefInputValues, selected = 1))
  
  ## Tab 4: plot of residual curves
  residuals.help = "If 'Show Outliers' is selected, the median and outlying curves are shown in blue and red respectively. If 'Rainbowize' 
                    is selected, curves are ordered by band depth with most outlying curves shown in red and 
                    curves closest to the median shown in violet."
  residuals.call = eval(call("radioButtons","residOptions", label="Plot Options", 
                             choices = list("None"=1, "Show Median and Outliers"=2,"Rainbowize by Depth"=3), selected=1))
  
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
                    tabPanelModuleUI("fitted", tabTitle = "Fitted Values", icon("line-chart"), calls = fitted.call,helperText = fitted.help ),
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
        y.obs = fosr.obj$data[,names(attributes(terms(fosr.obj$terms))$dataClasses)[1]]
        colnames(y.obs) = grid
        y.obs.m = melt(y.obs)
        colnames(y.obs.m) = c("subj", "grid", "value")

        CovarChoice = as.numeric(input$CovarChoice)
        selected = covar.list[CovarChoice]
        if(selected == "None") {
          y.obs.m$covariate = NULL
        } else {
          y.obs.m$covariate = rep(fosr.obj$data[,selected], length(grid))
        }
        #y.obs.m

        if(is.null(y.obs.m$covariate)){
          p1 <- ggplot(y.obs.m, aes(x=grid, y=value, group = subj)) + geom_line(alpha = .3, color="black") +
            theme_bw() + xlab("") + ylab("")
        } else {
          p1 <- ggplot(y.obs.m, aes(x=grid, y=value, group = subj, color = covariate)) + geom_line(alpha = .3) +
            theme_bw() + xlab("") + ylab("") + theme(legend.position="bottom", legend.title=element_blank())
        }
      })
      
      plotInputLasagna <- reactive({
        ## this is where we will add code for lasagna plot
        y.obs = fosr.obj$data[,names(attributes(terms(fosr.obj$terms))$dataClasses)[1]]
        df = makeLasagna(data = fosr.obj$data, outcome = y.obs, covariate = NULL)
        plots = bakeLasagna(data = fosr.obj$data, data.long = df$data.long, covariate = NULL)[[1]]
        plots$lasagnaPlot
      })
      
      callModule(tabPanelModule, "observed", plotObject = plotInputObsData, plotName = "observed", plotObject2 = plotInputLasagna)

      
      #callModule(tabPanelModule, "scoreplots", plotObject = stuff, plotName = "scoreplots", plotObject2 = stuff2, is.plotly = TRUE)
      #################################
      ## Code for FittedValues Tab
      #################################

      plotInputFittedVal <- reactive({

        variables = sapply(pred.list, function(u) {input[[u]]})

        input.data = fosr.obj$data[1,]

        reassign = function(var, newdata){
          if(is.numeric(fosr.obj$data[,var])){
            var.value = as.numeric(newdata[var])
            #          } else if(is.factor(fosr.obj$data[,var]) & length(levels(fosr.obj$data[,var])) <=2){
            #            var.value = factor(levels(fosr.obj$data[,var])[newdata[var]+1], levels = levels(fosr.obj$data[,var]))
          } else if(is.factor(fosr.obj$data[,var])){
            var.value = factor(newdata[var], levels = levels(fosr.obj$data[,var]))
          }
          var.value
        }

        input.data[,pred.list] = lapply(pred.list, reassign, variables)

        X.design = t(matrix(model.matrix(fosr.obj$terms, input.data)))
        fit.vals = as.vector(X.design %*% fosr.obj$beta.hat)
        df <- data.frame(grid = grid,
                   fit.vals = fit.vals)

        p2 <- ggplot(df, aes(x = grid, y = fit.vals)) + geom_line(lwd=1) + theme_bw() +
          xlab(xlab) + ylab(ylab) + ylim(c(.9, 1.1) * range(fosr.obj$Yhat))

      })
      
      callModule(tabPanelModule, "fitted", plotObject = plotInputFittedVal, plotName = "fitted")

      #################################
      ## Code for CoefFunc Tab
      #################################

      plotInputCoefFunc <- reactive({
        CoefChoice = as.numeric(input$CoefChoice)
        df <- data.frame(grid = grid,
                   coef = fosr.obj$beta.hat[CoefChoice,],
                   UB =  fosr.obj$beta.UB[CoefChoice,],
                   LB = fosr.obj$beta.LB[CoefChoice,])

        p3 <- ggplot(df, aes(x=grid, y=coef))+geom_line(linetype=1, lwd=1.5, color="black")+
          geom_line(data = df, aes(y=UB), color = "blue") +
          geom_line(data = df, aes(y=LB), color = "blue")+
          theme_bw() + xlab("") + ylab("")

      })

      
      callModule(tabPanelModule, "coef", plotObject = plotInputCoefFunc, plotName = "coef")
      
      #################################
      ## Code for Residual plot
      #################################

      response = fosr.obj$data[,names(attributes(terms(fosr.obj$terms))$dataClasses)[1]]
      resid = response - fosr.obj$Yhat
      colnames(resid) = grid
      outs = outliers(resid, 1.5) # detects outliers
      resid.m = melt(resid)
      colnames(resid.m) = c("subj", "grid", "residual")
      resid.m = resid.m[order(resid.m$subj),]
      resid.m$depths = rep(outs$depth, each = dim(resid)[2])
      resid.m = resid.m[order(resid.m$depths, decreasing = FALSE),]
      resid.m$depth.rank = rep(1:dim(resid)[1], each=dim(resid)[2])


      # residuals for outliers

      resid.outs.m = melt(outs$outcurves)
      colnames(resid.outs.m) = c("subj", "grid", "residual")

      # residuals for median curve
      resid.med.m = melt(outs$medcurve)
      colnames(resid.med.m) = c("subj", "grid", "residual")

       plotInputResid <- reactive({
        residPlot = ggplot(resid.m, aes(x=grid, y=residual, group = subj))+ theme_bw() + geom_line(alpha = .3, color="black")

        if(input$residOptions==2 & dim(outs$outcurves)[1]!= 0){residPlot=residPlot+
                                   geom_line(data=resid.outs.m, aes(x=grid, y=residual, group=subj, color="outliers"))+
                                   geom_line(data=resid.med.m, aes(x=grid, y=residual, group=subj, color = "median"))+
                                   scale_colour_manual("", values = c("outliers"="indianred", "median"="blue"), guide = FALSE)
                                   #theme(legend.position="bottom")

        }
        else if(input$residOptions==2 & dim(outs$outcurves)[1]== 0){residPlot=residPlot+
                                   geom_line(data=resid.med.m, aes(x=grid, y=residual, group=subj, color = "median"))+
                                   scale_colour_manual("", values = c("median"="blue"), guide=FALSE)

        }

        else if (input$residOptions == 3){residPlot = ggplot(resid.m, aes(x=grid, y=residual, group = subj)) +
                                             geom_line(aes(color=factor(depth.rank))) + theme_bw()+ theme(legend.position="none")}
        residPlot  + xlab("") + ylab("")
      })

       callModule(tabPanelModule, "residuals", plotObject = plotInputResid, plotName = "residuals")

      ## add subject number
  
    } ## end server
  )
}

