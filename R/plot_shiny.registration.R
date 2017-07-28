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
#' @importFrom plotly plot_ly ggplotly event_data add_trace layout
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
  Yhat_df <- fpca.obj$Yhat
  Y_df <- fpca.obj$Y

  ## establish inverse link function for plotting
  inv_link = createInvLink(family = fpca.obj$family)

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

  #### fpca

  # muPC plot
  muPC.help = "Solid black line indicates population mean. For the FPC selected below, blue and red lines
               indicate the population mean +/- the FPC times 2 SDs of the associated score distribution."
  muPC.call = as.list(NA)
  muPC.call[[1]] = selectInput(inputId = "PCchoice", label = ("Select FPC"), choices = 1:fpca.obj$npc, selected = 1)

  ## add y-axis scale input if family is not gaussian
  if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian")) {
    muPC.call[[2]] = selectInput("muPC_scale", label = ("Select Y-axis Scale"), choices = c("Natural", "Response"), selected = "Natural")
  }

  # subject fits plot
  subjects.help = "Plot shows observed data and fitted values for the subject selected below."
  subjects.call = eval(call("selectInput", inputId = "subject", label = ("Select Subject"),
                            choices = unique(Yhat_df$id), selected = unique(Yhat_df$id)[1]))

  # scoreplot (need to edit this in both versions)

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
                    tabPanel("fpca", icon = icon("stats", lib = "glyphicon"),
                             tabsetPanel(
                               tabPanelModuleUI("muPC", tabTitle = "Mean +/- FPCs", icon("stats", lib = "glyphicon"),
                                                calls = muPC.call, helperText = muPC.help ),
                               tabPanelModuleUI("subjects",tabTitle = "Subject Fits", icon = icon("user"), calls = subjects.call,
                                                helperText = subjects.help )
                             )
                             ) # end tabPanel
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


      #################################
      ## Define fpca objects
      #################################

      ## define global objects
      mu_df = as_refundObj(matrix(fpca.obj$mu, nrow = 1), index = fpca.obj$argvals)
      efunctions = matrix(fpca.obj$efunctions, ncol = fpca.obj$npc)
      sqrt.evalues = diag(sqrt(fpca.obj$evalues), fpca.obj$npc, fpca.obj$npc)
      scaled_efunctions = efunctions %*% sqrt.evalues

      ## prep objects for plotting on response scale; used in subject plot tabs
      mu_df_inv_link = mu_df
      mu_df_inv_link = mutate(mu_df_inv_link, value = inv_link(value))

      Yhat_df_inv_link = Yhat_df
      Yhat_df_inv_link = mutate(Yhat_df_inv_link, value = inv_link(value))

      ## define plot defaults
      ## set y axes to be max(2 SDs from mu of PC1, fitted values)
      max.y = max(fpca.obj$mu + 2 * abs(scaled_efunctions[, 1]))
      min.y = min(fpca.obj$mu - 2 * abs(scaled_efunctions[, 1]))

      plotDefaults = list(theme = theme_bw(),
                          title = theme(plot.title = element_text(size = 20)),
                          xlab = xlab(xlab),
                          ylab = ylab(ylab),
                          ylim = ylim(c(min(min.y, range(Yhat_df$value)[1]), max(max.y,range(Yhat_df$value)[2]))),
                          x_scale = scale_x_continuous(breaks = seq(0, length(fpca.obj$mu) - 1, length = 6),
                                                       labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1))))

      #################################
      ## Code for plot of fpca mean +/- PCs
      #################################

      plotInputMuPC <- reactive({
        PCchoice = as.numeric(input$PCchoice)
        scaled_efuncs = scaled_efunctions[,PCchoice]

        df_plus = as_refundObj(matrix(fpca.obj$mu + 2 * scaled_efuncs, nrow = 1), index = fpca.obj$argvals)
        df_plus$id = 2

        df_minus = as_refundObj(matrix(fpca.obj$mu - 2 * scaled_efuncs, nrow = 1), index = fpca.obj$argvals)
        df_minus$id = 3

        plot_df = bind_rows(mu_df, df_plus, df_minus) %>%
          mutate(id = as.character(id))

        if (!(is.null(fpca.obj$family) || fpca.obj$family == "gaussian") && input[["muPC_scale"]] == "Response") {
          plot_df = mutate(plot_df, value = inv_link(value))
          plotDefaults[["ylim"]] = ylim(c(range(Y_df$value)[1], range(Y_df$value)[2]))
        }

        p1 <- ggplot(filter(plot_df, id == "1"), aes(x = index, y = value)) + geom_line(lwd = 1) + plotDefaults +
          geom_point(data = filter(plot_df, id == "2"), color = "blue", size = 4, shape = '+') +
          geom_point(data = filter(plot_df, id == "3"), color = "indianred", size = 4, shape = "-") +
          ggtitle(bquote(psi[.(input$PCchoice)]~(t) ~ "," ~.(100*round(fpca.obj$evalues[as.numeric(input$PCchoice)]/sum(fpca.obj$evalues),3)) ~ "% Variance"))
      })

      callModule(tabPanelModule, "muPC", plotObject = plotInputMuPC, plotName = "muPC")

      #################################
      ## Code for fpca subject plots
      #################################

      plotInputSubject <- reactive({
        subjectnum = input$subject

        p4 = ggplot(data = mu_df_inv_link, aes(x = index, y = value)) + geom_line(lwd = 0.5, color = "gray") +
          geom_line(data = filter(Yhat_df_inv_link, id == subjectnum), size = 1, color = "cornflowerblue") +
          geom_point(data = filter(Y_df, id == subjectnum), color = "blue", alpha = 1/3) +
          theme_bw() + xlab(xlab) + ylab(ylab) + ylim(c(range(Y_df$value)[1], range(Y_df$value)[2])) +
          scale_x_continuous(breaks = seq(0, length(fpca.obj$mu) - 1, length = 6), labels = paste0(c(0, 0.2, 0.4, 0.6, 0.8, 1)))

      })

      callModule(tabPanelModule, "subjects", plotObject = plotInputSubject, plotName = "subjects")


    } # end server



  ) # end shiny app

} # end overall funtion
