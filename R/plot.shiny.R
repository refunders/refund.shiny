#' Interactive Plotting for Functional Data
#' 
#' Function for interactive plotting of functional data analysis results. 
#' 
#' This package builds on the \code{refund} package: tools in \code{refund} are used to 
#' conduct analyses and functions in this package create interactive visualizations of the results
#' of those analyses. There are two major categories of analyses that can be viewed:
#' \enumerate{
#' \item{Functional principal components analyses implemented by \code{\link{fpca.sc}}, \code{\link{fpca.face}}, 
#' \code{\link{fpca.ssvd}}, and \code{\link{fpca2s}}. Plots show the mean +/- 2SD times each FPC; scree plots;
#' linear combinations of score values and FPCs; reconstructions for each subject; and score scatterplots.}
#' \item{Function-on-scalar regression analyses implemented by \code{\link{bayes_fosr}}. Plots show the raw data
#' colored by covariate values; fitted values depending on covariates; coefficient functions; and residuals.}
#' }
#' 
#' @param x object to be plotted. Currently, allowed data types are \code{fpca} and \code{fosr}.
#' @param ... additional arguments passed to plotting functions
#' 
#' @author Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu}, 
#' Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#' @seealso \code{\link{plot.shiny.fpca}}, \code{\link{plot.shiny.fosr}}
#' @export plot.shiny
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' library(refund)
#' library(dplyr)
#' 
#' ##### FPCA Example on real data #####
#' 
#' data(cd4)
#' SC = fpca.sc(cd4)
#' plot.shiny(SC)
#' 
#' ##### FPCA Examples on simulated data #####
#' 
#' set.seed(2678695)
#' n = 101
#' m = 101
#' s1 = 20
#' s2 = 10
#' s = 4
#' t = seq(-1, 1, l=m)
#' v1 = t + sin(pi*t)
#' v2 = cos(3*pi*t)
#' V = cbind(v1/sqrt(sum(v1^2)), v2/sqrt(sum(v2^2)))
#' U = matrix(rnorm(n*2), n, 2)
#' D = diag(c(s1^2, s2^2))
#' eps = matrix(rnorm(m*n, sd=s), n, m)
#' Y = U%*%D%*%t(V) + eps
#'
#' SC = fpca.sc(Y)
#' FACE = fpca.face(Y)
#' SSVD = fpca.ssvd(Y, verbose=FALSE)
#' S = fpca2s(Y)
#'  
#' plot.shiny(SC)
#' plot.shiny(FACE)
#' plot.shiny(SSVD)
#' plot.shiny(S)
#' 
#' 
#' ##### FoSR Example #####
#' 
#' data(DTI)
#' DTI = subset(DTI, select = c(cca, case, pasat))
#' DTI = DTI[complete.cases(DTI),]
#' DTI$gender = factor(sample(c("male","female"), dim(DTI)[1], replace = TRUE))
#' DTI$status = factor(sample(c("RRMS", "SPMS", "PPMS"), dim(DTI)[1], replace = TRUE))
#' 
#' fosr.dti1 = fosr_gls(cca ~ pasat, data = DTI)
#' plot.shiny(fosr.dti1)
#' 
#' fosr.dti2 = fosr_gls(cca ~ pasat * gender + status, data = DTI)
#' plot.shiny(fosr.dti2)
#' 
#' 
#' ##### FoSR Example with outliers #####
#' 
#' DTI$cca[1,] = DTI$cca[1,] + .4
#' DTI$cca[2,] = DTI$cca[2,] + .4
#' 
#' fosr.dti3 = fosr_gls(cca ~ pasat * gender + status, data = DTI)
#' plot.shiny(fosr.dti3)
#' 
#' }
#' 
plot.shiny <- function(x, ...){
  UseMethod("plot.shiny")
}