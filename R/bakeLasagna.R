#' Create side-by-side lasagna plot and density plot 
#' 
#' Internal method used in conjunction with \code{makeLasagna()} to create side-by-side lasagna plot and distribution plot. 
#' The distribution plot gives distribution of sorting covariate.
#' 
#' @param data Dataset for lasagna plot. Same data used in \code{makeLasagna()} function.
#' @param data.long Sorted longform dataset for lasagna plot output by \code{makeLasagna()} function.
#' @param covariate User-selected covariate for sorting the rows in the lasagna plot. 
#' Defaults to NULL, in which case data is sorted by row number.
#' 
#' @author Julia Wrobel \email{ajg2202@@cumc.columbia.edu}
#' @author Nicole Marie Lapointe Jameson 
#' 

bakeLasagna <- function(data, data.long, covariate = NULL){
  
  ## NULLify global variables 
  grid = plot.y = value = h = NULL
  
  if(length(covariate) == 0) { 
    covariate = "indexList"
    data$indexList = factor("id")
  } 
  if(class(data[[covariate]]) == "character") { data[[covariate]] = factor(data[[covariate]]) }
  
  data = data[order(as.numeric(data[[covariate]])),]
  
  ## ggplot
  l <- ggplot(data.long, aes(x= grid, y = plot.y, fill = value)) + 
    geom_tile(aes(height = h)) +  
    scale_fill_continuous(low = "#104E8B", high = "#BFEFFF", guide= "colourbar") +
    #ggtitle("Lasagna Plot")+
    xlab("") + ylab("")+
    
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          plot.title = element_text(hjust = 0, size=16, face="bold"),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          panel.background=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position="none") + 
    scale_x_discrete(breaks=1:nrow(data.long))
  
  ## histogram
  d <-ggplot(data, aes(x = data[[covariate]])) + {
    if(class(data[[covariate]]) == "factor") geom_bar(colour="#FF9912", fill="#FFE4B5", width = 1) 
    else geom_density(colour="#FF9912", fill="#FFE4B5", alpha=.3) } +
    #ggtitle("Covariate") +
    coord_flip() + 
    theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=14),
      plot.title = element_text(hjust = 0, size=16, face="bold"),
      axis.ticks=element_blank(),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(angle = -90),
      panel.background=element_blank(),
      panel.grid.minor=element_blank(),
      legend.position="none")
  
  return(list(lasagnaPlot = l, densityPlot = d))
  
}