% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_linCom.R
\name{make_linCom}
\alias{make_linCom}
\title{Create lincom plot for FPCA panels}
\usage{
make_linCom(obj, pc_weights, response_scale = FALSE)
}
\arguments{
\item{obj}{fpca object to be plotted.}

\item{pc_weights}{User-selected weights for FPCs}

\item{response_scale}{Scale of reponse to be plotted. If TRUE results are plotted on response scale,
if FALSE results are plotted on natural scale.}
}
\description{
Produces a ggplot with mean and sliders to change weighting of each PC; allows you to obtain range of potential
fitted values.
}
