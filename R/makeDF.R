#' make melted dataframes for mfpca
#'
#' Function to create dataframes of observed and fitted values for plotting with mfpca.
#' @param IDvalue id on which to create dataframe.
#' @param data name of data to be manipulated.
#' @param subsetOn name of dataframe with 'id' column on which to subset data.
#' @return a dataframe of melted observations or Yhat values for use in ggplot.
#' @author Julia Wrobel 

makeDF <- function(IDvalue, data, subsetON, ...){
  df = mutate(melt(as.matrix(subset(data, subsetON == IDvalue, ...))), grid=rep(1:ncol(Y.df$Y), each = length(which(Y.df$id == IDvalue))))
  names(df) = c("visit", "Ynames", "value", "time")
  return(df)
}

