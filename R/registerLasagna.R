#' Get spaces between timepoints as widths for binary registration lasagna plot.
#'
#' @param z time values for a specific subject
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'

getWidth = function(z){
  c(diff(z), 0)
}



#' Create lasagna plot for unregistered and registered data
#'
#' Get registered and unregistered lasagna plots for binary data. Note: should make this compatible
#' for other data types as well. Requires data to have t.hat and t.star variables.
#'
#' @param data Dataset for lasagna plot.
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
#'
#' @export
#'

registerLasagna = function(data){

  ## NULLify global values called in ggplot
  value = t.star = t.hat = NULL

  ids = unique(data$id)

  width.tstar = as.vector( sapply(ids, function(id) getWidth(data$t.star[data$id %in% id ]) ) )
  width.t = as.vector( sapply(ids, function(id) getWidth(data$t.hat[data$id %in% id ]) ) )

  baseplot = ggplot(data, aes(t.star, id, fill = factor(value))) +
    theme(axis.text=element_text(size=12),
          plot.title = element_text(hjust = 0, size=16, face="bold"),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          axis.text.y=element_blank(),
          panel.background=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major = element_blank(),
          legend.position="none") +
    labs(y = "Subject") +
    scale_fill_manual(values = c("lightblue", "darkblue"))

  plot.tstar = baseplot +
    geom_rect(aes(xmin = t.star, xmax = t.star + width.tstar, ymin = id-0.5, ymax = id+0.5)) +
    labs(x = "observed time")

  plot.t = baseplot +
    geom_rect(aes(xmin = t.hat, xmax = t.hat + width.t, ymin = id-0.5, ymax = id+0.5)) +
    labs(x = "estimated time")

  return(list(plot.tstar, plot.t))
}
