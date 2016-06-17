#' Convert matrices to dataframes for use in functional data analyses
#'
#' @param mat Matrix to be converted; rows contain functional observations on subjects.
#' @param index Time grid on which functional data are observed; defaults to \code{NULL},
#' which assumes an equally-spaced grid on [0,1]
#'
#' @author Jeff Goldsmith \email{jeff.goldsmith@@columbia.edu}
#' 
#' @return An object of classes \code{data.frame} and \code{refund.object}, the latter of
#' which is so far not used. Columns are \code{id} (taken from the rownames of \code{mat},
#' if they exist), \code{index} (with behavior described above), and \code{value} (taken
#' from entries in \code{mat}). 
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' library(ggplot2)
#' library(refund)
#' 
#' cca_df = as_refundObj(DTI$cca)
#' ggplot(cca_df, aes(x = index, y = value, group = id)) + geom_line()
#' }
#' 
as_refundObj.matrix = function(mat, index = NULL) {
	
	if (is.null(index)) {
		index = seq(0, 1, length = dim(mat)[2])
	}
	
	if (!is.null(rownames(mat))) {
		id = rownames(mat)
	} else {
		id = 1:dim(mat)[1]
	}
	
	df = data.frame(
		id = rep(id, each = dim(mat)[2]),
		index = rep(index, dim(mat)[1]),
		value = as.vector(t(mat)),
		stringsAsFactors = FALSE
	)
	
	df = df[complete.cases(df),]
	
	class(df) = c("data.frame", "refund_object")
	
	df
}