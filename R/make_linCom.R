#' Create lincom plot for FPCA panels
#'
#' Produces a ggplot with mean and sliders to change weighting of each PC; allows you to obtain range of potential
#' fitted values.
#'
#' @param obj fpca object to be plotted.
#' @param pc_weights User-selected weights for FPCs
#' @param response_scale Scale of reponse to be plotted. If TRUE results are plotted on response scale,
#' if FALSE results are plotted on natural scale.
#'
#' @import ggplot2
#' @export
make_linCom = function(obj, pc_weights, response_scale = FALSE){
  ## NULLify global values called in ggplot
  lincom = mu = index = NULL

  inv_link = createInvLink(family <- obj$family)

  efunctions = matrix(obj$efunctions, ncol = obj$npc)
  sqrt.evalues = diag(sqrt(obj$evalues), obj$npc, obj$npc)
  scaled_efunctions = efunctions %*% sqrt.evalues

  min.x = min(obj$Y$index)
  max.x = max(obj$Y$index)
  min.y = min(c(obj$mu - 2 * abs(scaled_efunctions[, 1]), range(obj$Yhat$value)[1]))
  max.y = max(c(obj$mu + 2 * abs(scaled_efunctions[, 1]), range(obj$Yhat$value)[2]))

  df = data.frame(id = 1,
                  lincom = obj$mu + scaled_efunctions %*% pc_weights,
                  mu = obj$mu,
                  index = seq(min.x, max.x, length.out = length(obj$mu)))

  if(response_scale){
    df = mutate(df,
                lincom = inv_link(lincom),
                mu = inv_link(mu))

    max.y = range(obj$Y$value)[2]
    min.y = range(obj$Y$value)[1]
  }

  ggplot(df, aes(index, mu)) + theme_bw() +
    theme(plot.title = element_text(size = 20)) +
    xlim(min.x, max.x) +
    ylim(min.y, max.y) +
    xlab("") + ylab("") +
    geom_line(color = "gray", size = 0.75) +
    geom_line(aes(y = lincom), color = "cornflowerblue", size = 1.25)

}
