#' Return inverse link function for plot_shiny.fpca()
#' 
#' Internal method that constructs the inverse link function for a 
#' generalized FPCA fit. This is used in toggling between plots on the
#' natural scale and on the response scale. 
#' 
#' @param family Family of the (generalized) FPCA. Currently supported families
#' are \code{gaussian} and \code{binomial}.
#' 
#' @author Jeff Goldsmith \email{ajg2202@@cumc.columbia.edu}
#' 
createInvLink = function(family = NULL){
  
  if (is.null(family) || family == "gaussian") {
    f = function(x){
      x
    }
  } else if (family == "binomial") {
    f = function(x){
      exp(x) / (1 + exp(x))
    }
  }
  
}
