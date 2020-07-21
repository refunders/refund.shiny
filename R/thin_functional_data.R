
#' Thin functional data
#'
#' Takes a dense functional dataset in long form and thins it so that there are 100 observations per
#' subject, equally spaced.
#'
#' @param Y functional dataframe
#' @param length_out number of points per subject for dataframe that is returned
#'
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'
#' @importFrom dplyr group_by slice n
#'
#' @export
#'
thin_functional_data = function(Y, length_out = 100){
  Y_grouped = group_by(Y, id)
  Y_thin = slice(Y_grouped, seq(1, n(), length.out = length_out))
  ungroup(Y_thin)
}
