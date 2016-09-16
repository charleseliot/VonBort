#'
#'  UI.R
#' 
#' Copyright (c) 2016 Charles Eliot

library(shiny)

shinyUI(fluidPage(
  title = "von Bortkiewicz Analysis",
  
  titlePanel("Prussian Cavalry Killed By Horse Kicks (1875-1894)"),

  tabsetPanel(
    tabPanel("Original Data",
             br(),
             div("This famous data set was published by Ladislaus von Bortkiewicz in 1898 in ",
                 strong("Das Gesetz der kleinen Zahlen"), "(The Law of Small Numbers)."),
             br(),
             div("von Bortkiewicz collected statistics on the numbers of deaths from horse kicks in 14 Prussian Cavalry corps over a 20 year period. Even though the individual counts are small and apparently haphazard, von Bortkiewicz was able to glean trends, variations, and contributing factors."),
             br(),
             div("The von Bortkiewicz data set is included in the R library", strong("vcd"), "as", strong("VonBort"), "."),
             br(),
             div("Now click on the", strong("Model Fitting"), "tab to play around with the data a bit."),
             br(),
             tableOutput("table")),
    tabPanel("Model Fitting",
             br(),
             div("The von Bortkiewicz data set is well fitted by a Poisson distribution. Move the slider around to find the best fit for the Poisson rate parameter."),
             br(),
             # Sidebar with slider input for a Poisson model
             sidebarLayout(
               sidebarPanel(
                 sliderInput("rate", "Rate:", min = 0.6, max = 0.8, step = 0.01, value = 0.7),

                 h3("Fitting information"),
                 h4("Rate:"),
                 verbatimTextOutput("oidRate"),
                 h4("Chi-squared (lower = better fit):"),
                 verbatimTextOutput("oidChiSquared"),
                 h4("p value (higher = better fit):"),
                 verbatimTextOutput("oidP")
               ),
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             ),
             br(),
             div("We can use R's goodfit() function to try different model distributions. Pick a model from the drop-down list, and see how well the model fits the observed data. The Poisson distribution is far better than the Binomial Distribution, but the Negative Binomial distribution even better."),
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("model", "Model:",
                             c("Poisson" = "poisson",
                               "Binomial" = "binomial",
                               "Negative Binomial" = "nbinomial")),
                 plotOutput("fitPlot")
               ),
               mainPanel(
                 h4("Model:"),
                 verbatimTextOutput("oidModelName"),
                 h4("Fitting method:"),
                 verbatimTextOutput("oidFittingMethod"),
                 h4("Chi-squared:"),
                 verbatimTextOutput("oidGoodFitX2"),
                 h4("p value:"),
                 verbatimTextOutput("oidGoodFitP")
               )
             )
    )
  )
))
