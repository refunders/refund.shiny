#' Pre-process data for lasagna plot
#'
#' Internal method that takes a dataframe of observed data with an outcome matrix and user-selected covariate,
#' sorts outcome by the selected covariate, and assigns heights to each row based on value of the selected covariate.
#' The resulting dataframe is used with \code{bakeLasagna()} to create lasagna plot.
#'
#' @param data Dataset for lasagna plot.
#' @param outcome Matrix of values where each row represents a functional observation.
#' @param covariate User-selected covariate for sorting the rows in the lasagna plot.
#' Defaults to NULL, in which case data is sorted by row number.
#'
#' @author Julia Wrobel \email{julia.wrobel@@cuanschutz.edu}
#'
makeLasagna <- function(data, outcome, covariate = NULL){

  ## NULLify global variables
  hist = subj = NULL

  if(length(covariate) == 0) {
    covariate = "indexList"
    data$indexList = 1:dim(data)[1]}
  ## convert categorical data to numeric
  if(class(data[[covariate]]) == "character") { data[[covariate]] = factor(data[[covariate]]) }
  if(class(data[[covariate]]) == "factor"){
    factor.levels = nlevels(data[[covariate]])
    type.breaks = seq(from = 0.5, to = factor.levels + 0.5, length.out = factor.levels + 1)
    data[[covariate]] = as.numeric(data[[covariate]])
  } else{
    type.breaks = "Sturges" ## this is for the histogram call
  }

  order.covar = data[order(data[[covariate]]),]
  order.outcome = order.covar[[outcome]]
  J = dim(order.outcome)[2] ## J is number of observations per subject
  N = dim(order.outcome)[1] ## N is number of subjects

  rownames(order.outcome) = seq(1:N)
  colnames(order.outcome) = seq(1:J)
  names(dimnames(order.outcome))= c("subj", "grid")

  outcome.df = melt(order.outcome)

  ## histogram values
  covar.hist = hist(order.covar[[covariate]], plot = FALSE, breaks = type.breaks)
  breaks = covar.hist$breaks
  counts = covar.hist$counts
  binwidth = breaks[2] - breaks[1]
  heights = binwidth/counts  # will get an error if counts is zero in any of the bins
  bin.membership = cut(order.covar[[covariate]], breaks = breaks, include.lowest = TRUE)
  bins = levels(bin.membership)

  h = plot.y = rep(NA, length(bin.membership))
  for(b in 1:length(bins)){
    h[which(bin.membership == bins[b])] = heights[b]
    plot.y[which(bin.membership == bins[b])] = seq(breaks[b] + heights[b]/2, breaks[b+1]-heights[b]/2, length.out = counts[b] )
  }

  outcome.df = arrange(outcome.df, subj) %>% mutate(h = rep(h, each = J), plot.y = rep(plot.y, each = J))
  return(list(data.long = outcome.df))
}
