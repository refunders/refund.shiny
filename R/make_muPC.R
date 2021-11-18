#' Create muPC plot for FPCA panels
#'
#' Produces a ggplot with mean plus or minus two standard deviations of a selected FPC.
#'
#' @param obj fpca object to be plotted.
#' @param pc_choice FPC to be plotted.
#' @param response_scale Scale of reponse to be plotted. If TRUE results are plotted on response scale,
#' if FALSE results are plotted on natural scale.
#'
#' @import ggplot2
#'
make_muPC = function(obj, pc_choice, response_scale = FALSE){

  ## NULLify global values called in ggplot
  plus = minus = mu = index = NULL

  inv_link = createInvLink(family <- obj$family)

  efunctions = matrix(obj$efunctions, ncol = obj$npc)
  sqrt.evalues = diag(sqrt(obj$evalues), obj$npc, obj$npc)
  scaled_efunctions = efunctions %*% sqrt.evalues
  scaled_efuncs = scaled_efunctions[,pc_choice]

  min.x = min(obj$Y$index)
  max.x = max(obj$Y$index)
  max.y = max(c(obj$mu + 2 * abs(scaled_efunctions[, 1]), range(obj$Yhat$value)[2]))
  min.y = min(c(obj$mu - 2 * abs(scaled_efunctions[, 1]), range(obj$Yhat$value)[1]))

  df = data.frame(id = 1,
                  plus = obj$mu + 2 * scaled_efuncs,
                  minus = obj$mu - 2 * scaled_efuncs,
                  mu = obj$mu,
                  index = seq(min.x, max.x, length.out = length(obj$mu)))

  if(response_scale){
    df = mutate(df, plus = inv_link(plus),
                minus = inv_link(minus),
                mu = inv_link(mu))

    max.y = range(obj$Y$value)[2]
    min.y = range(obj$Y$value)[1]
  }

  ggplot(df, aes(index, mu)) +
    geom_line(lwd = 1) +
    geom_point(aes(y = plus), color = "blue", size = 4, shape = "+") +
    geom_point(aes(y = minus), color = "indianred", size = 4, shape = "-") +
    ggtitle(bquote(psi[.(pc_choice)]~(t) ~ ","
                   ~.(100*round(obj$evalues[pc_choice]/sum(obj$evalues),3)) ~ "% Variance")) +
    theme_bw() +
    theme(plot.title = element_text(size = 20)) +
    xlim(min.x, max.x) +
    ylim(min.y, max.y) +
    xlab("") + ylab("")

}
