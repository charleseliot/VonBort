#'
#'  SERVER.R
#' 
#' Copyright (c) 2016 Charles Eliot

library(shiny)
library(vcd)
library(ggplot2)
library(dplyr)
library(ggthemes)

source("server_utils.R")

#' ===============================
#'  INITIALIZATION
#' ===============================

# Grab the VonBort data set
data("VonBort")

# Put an index on the VonBort data
VonBort$index <- c(1:nrow(VonBort))

# Create secondary views aggregating by year and corps
VonBort.by.year <- VonBort %>% group_by(year) %>% summarize(deaths = sum(deaths))
VonBort.by.corps <- VonBort %>% group_by(corps) %>% summarize(deaths = sum(deaths))

# Extract basic data from the raw data and the per-year aggregation
mean.by.corps.year <- mean(VonBort$deaths)
var.by.corps.year <- sd(VonBort$deaths)^2
mean.by.year <- mean(VonBort.by.year$deaths)
var.by.year <- sd(VonBort.by.year$deaths)^2

# Create a contingency table of observed data
observed.tab <- as.data.frame(table(VonBort$deaths), stringsAsFactors = FALSE)
names(observed.tab) <- c("k", "count")
observed.tab$k <- as.numeric(observed.tab$k)
observed.tab$p <- observed.tab$count / sum(observed.tab$count)

# Construct a grid resembling von Bortkiewicz's 1898 original
original.grid <- originalVonBortGrid(VonBort)

# A couple of things we'll need later
# indexes <- c(0:4)
indexes <- observed.tab$k
pal <- fivethirtyeight_pal()(3)

#' ===============================
#'  MAIN SERVER FUNCTION
#' ===============================

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({compPlt(input$rate, output)})
  output$table <- renderTable(original.grid)
  output$fitPlot <- renderPlot({plot(tryModel(input$model, output))})
})

#' ===============================
#'  PROCESSING FUNCTIONS
#' ===============================

#' The compPlt() function constructs a model Poisson distribution based on 
#' the value of the lambda.in parameter, then draws a rootogram-like plot
#' comparing observed values with the model.

compPlt <- function(lambda.in, output){
  # Construct a Poisson model based on lambda.in
  model.counts <- length(VonBort$deaths) * dpois(indexes, lambda.in)
  model.density <- dpois(indexes, lambda.in)
  
  # Move all the data we're going to use to one data frame, called data.tab.
  # Hang is the position of the bottom of the observed-value bar in a rootogram
  # plot.
  data.tab <- data.frame(k=indexes,
                         observed.count=observed.tab$count,
                         model.count=model.counts,
                         model.density=model.density,
                         hang=model.counts - observed.tab$count)
  
  # Perform a chi-squared test comparing the fit between the observed counts
  # and the counts predicted by the model
  suppressWarnings(
    ct <- chisq.test(data.tab$observed.count, p=data.tab$model.count, rescale.p = TRUE)
  )

  # Write fit statistics back for reactive display
  output$oidRate <- renderPrint({lambda.in})
  output$oidChiSquared <- renderPrint({as.numeric(ct$statistic)})
  output$oidP <- renderPrint({ct$p.value})
  
  # Display a pseudo-rootogram of the observed versus fitted data (pseudo in the
  # sense that it doesn't use a square-root y-axis).
  g <- ggplot(data = data.tab, aes(x = k, ymax = model.count, ymin = hang)) +
    geom_linerange(size=10, color = pal[3], alpha = 0.5) +
    geom_point(aes(y = model.count), shape = 1, size = 5, color = pal[1]) +
    geom_line(aes(y = model.count), size = 2, color = pal[1], linejoin="round") +
    theme_igray() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
    xlab("deaths per corp per year") + ylab("count")

  return(g)
}

tryModel <- function(model, output){
  # Call goodfit() to find the best fit for the given model, then chisq.test() to
  # measure the quality of the fit.
  if (model == "poisson"){
    gf <- goodfit(data.frame(observed.tab$count, observed.tab$k), 
                  type="poisson", 
                  method="MinChisq")
    suppressWarnings(
      ct <- chisq.test(observed.tab$count, p=gf$fitted, rescale.p = TRUE)
    )
  }
  else if (model == "binomial"){
    gf <- goodfit(data.frame(observed.tab$count, observed.tab$k), 
                  type="binomial", 
                  method="MinChisq",
                  par = list(size=4))

    suppressWarnings(
      ct <- chisq.test(observed.tab$count, p=gf$fitted, rescale.p = TRUE)
    )
  }
  else if (model == "nbinomial"){
    gf <- goodfit(data.frame(observed.tab$count, observed.tab$k), 
                  type="nbinomial", 
                  method="MinChisq",
                  par = list(size=4))
    
    suppressWarnings(
      ct <- chisq.test(observed.tab$count, p=gf$fitted, rescale.p = TRUE)
    )
  }

  # Write stats about the model back to the reactive display
  output$oidModelName <- renderPrint({gf$type})
  output$oidFittingMethod <- renderPrint({gf$method}) 
  output$oidGoodFitX2 <- renderPrint({as.numeric(ct$statistic)})
  output$oidGoodFitP <- renderPrint({ct$p.value})

  # Return the goodfit object  
  return(gf)
}
