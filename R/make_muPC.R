#' Create muPC plot for FPCA panels
#'
#' Produces a ggplot with mean plus or minus two standard deviations of a selected FPC.
#'
#' @param obj fpca object to be plotted.
#' @param pc_choice FPC to be plotted.
#'
#' @import ggplot2
#' @export
make_muPC = function(obj, pc_choice){
  efunctions = matrix(obj$efunctions, ncol = obj$npc)
  sqrt.evalues = diag(sqrt(obj$evalues), obj$npc, obj$npc)
  scaled_efunctions = efunctions %*% sqrt.evalues
  scaled_efuncs = scaled_efunctions[,pc_choice]

  min.x = min(obj$Y$index)
  max.x = max(obj$Y$index)
  max.y = max(obj$mu + 2 * abs(scaled_efunctions[, 1]))
  min.y = min(obj$mu - 2 * abs(scaled_efunctions[, 1]))

  df = data.frame(id = 1,
                  plus = obj$mu + 2 * scaled_efuncs,
                  minus = obj$mu - 2 * scaled_efuncs,
                  mu = obj$mu,
                  index = seq(min.x, max.x, length.out = length(obj$mu)))

  ggplot(df, aes(index, mu)) +
    geom_line(lwd = 1) +
    geom_point(aes(y = plus), color = "blue", size = 2.5, shape = "+") +
    geom_point(aes(y = minus), color = "indianred", size = 2.5, shape = "-") +
    ggtitle(bquote(psi[.(pc_choice)]~(t) ~ ","
                   ~.(100*round(obj$evalues[pc_choice]/sum(obj$evalues),3)) ~ "% Variance")) +
    theme_bw() +
    theme(plot.title = element_text(size = 20)) +
    xlim(min.x, max.x) +
    ylim(min(min.y, range(obj$Yhat$value)[1]), max(max.y,range(obj$Yhat$value)[2])) +
    xlab("") + ylab("")

}
