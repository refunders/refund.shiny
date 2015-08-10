

##########################################################
# Function for fast MBD calculation
##########################################################

#' function used in method for fast modified band depth (MBD) calculation
#' 
#' @param n 
#' @param p 
#' 
#' @author Ying Sun and Marc G.Genton
#'

combinat=function(n,p){
  if (n<p){combinat=0}
  else {combinat=exp(lfactorial(n)-(lfactorial(p)+lfactorial(n-p)))}
}


#' fMBD  
#' Method for fast modified band depth (fMBD) calculation
#' 
#' @param data name of dataset
#' 
#' @author Ying Sun and Marc G.Genton
#'

fMBD=function(data){
  p=dim(data)[1]
  n=dim(data)[2]
  rmat=apply(data,1,rank)
  down=rmat-1
  up=n-rmat
  (rowSums(up*down)/p+n-1)/combinat(n,2)
}


#' Identifies outliers for plot.shiny.fosr()
#' 
#' Internal method that assigns band depth values to curves based on exact fast MBD computation (Sun & Genton, 2012).
#' Code modified from fbplot in fda package. 
#' A dataframe of residuals is passed as an argument, and depths and outlying curves are returned
#' 
#' @param name variable name
#' @param variable variable values from dataset
#' 
#' @author Julia Wrobel \email{jw3134@@cumc.columbia.edu}
#'

outliers = function(data, factor=1.5){
  # tranpose data so that each column is a curve rather than each row
  data = t(data)
  
  depth = fMBD(data)
  
  tp = dim(data)[1] # number of observations per curve
  n = dim(data)[2]  # number of curves
  x = 1:tp
  
  dp_s = sort(depth, decreasing = TRUE)
  index = order(depth, decreasing = TRUE)
  
  ##### median curve
  med = depth == max(depth)
  medavg = matrix(data[, med], ncol = sum(med), nrow = tp) # gets median curve
  y = apply(medavg, 1, mean)
  ##
  
  ###### get 50% region (analogous to IQR)
  m = ceiling(n * 0.5)
  center = data[, index[1:m]] # 50% region (deepest curves), 'IQR' 
  out = data[, index[(m + 1):n]] # curves outside of 'IQR'
  inf = apply(center, 1, min) 
  sup = apply(center, 1, max)
  ##
  
  ##### get Outliers
  dist = factor * (sup - inf) # defines what it means to be an outlier
  upper = sup + dist
  lower = inf - dist
  outly = (data <= lower) + (data >= upper) # sets matrix, each point in each curve is checked for outlying-ness
  outcol = colSums(outly)
  remove = (outcol > 0)
  colum = 1:n  # lists number of curves 
  outpoint = colum[remove == 1] # gets index of outlying curves
  outcurves = data[, remove] # gets values for outlying curve
  medcurve = data[,med]
  ##
  
 
  # returns index of outliers, subset of data with just outlying curves
  return(list(depth = depth, outpoint = outpoint, medcurve = t(medcurve), outcurves=t(outcurves)))
  
}


