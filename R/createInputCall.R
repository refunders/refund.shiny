#' Create input calls for plot.shiny.fosr()
#' 
#' Internal method that constructs the input calls for plot.shiny.fosr(). The
#' variable name and values are passed as arguments, and a corresponding slider (for
#' numeric) or drop-down (for factor) input is constructed.
#' 
#' @param name variable name
#' @param variable variable values from dataset
#' 
#' @author Jeff Goldsmith \email{ajg2202@@cumc.columbia.edu}
#' 
createInputCall = function(name, variable){
  
  if(is.numeric(variable)){
    step.width = ((max(variable) - min(variable))/50)
    call("sliderInput", inputId = name, label = name, 
         min = signif(min(variable), 2), max = signif(max(variable), 2), value = mean(variable), 
         animate = animationOptions(interval=300, loop=T))
#  } else if(is.factor(variable) & length(levels(variable)) <= 2){
#    checkboxInput(inputId = name, label = levels(variable)[2], value = FALSE)
  } else if(is.factor(variable) & length(levels(variable)) ){
    selectInput(name, label = name, choices = levels(variable), selected = 1)
  }
  
}

