#'
#'  SERVER_UTILS.R
#' 
#' Copyright (c) 2016 Charles Eliot

library(shiny)
library(vcd)
library(ggplot2)
library(dplyr)
library(ggthemes)

#' OriginalVonBortGrid()
originalVonBortGrid <- function(vb){
  corps.names <- levels(vb$corps)
  years <- levels(as.factor(vb$year))
  
  df <- NULL
  
  for (year in years){
    x <- vb[vb$year == year,]
    
    # This code assumes that the order of corps names is the same in
    # each per-year subset.
    assertthat::are_equal(corps.names, names(x$deaths))
    
    if (is.null(df)){
      df <- data.frame(x$deaths)
    }
    else {
      df <- cbind(df, x$deaths)
    }
  }
  
  rownames(df) <- corps.names
  colnames(df) <- as.character(years)
  
  return(df)
}


  