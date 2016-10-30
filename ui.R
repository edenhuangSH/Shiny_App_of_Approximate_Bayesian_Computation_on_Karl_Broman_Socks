suppressWarnings(library(shiny))
suppressWarnings(library(ggplot2))
suppressWarnings(library(parallel))

# Define UI for application that plots the density 
ui = fluidPage(
  
  # Application title
  titlePanel("Appoximate Bayesian Computing with Socks"),
  
  # Sidebar with a slider input for sock parameters
  sidebarLayout(
    sidebarPanel(
      sliderInput('prior_mean',
                  'Prior Mean Number of Socks',
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput('prior_sd',
                  'Prior Std Deviation of Mean',
                  min = 1,
                  max = 30,
                  value = 15),
      sliderInput('frac_pair',
                  'Prior Fraction of Paired Socks',
                  min = 0,
                  max = 0.95,
                  value = 0.8),
      sliderInput('sig_pair',
                  'Prior Std Deviation of Fraction',
                  min = 0.05,
                  max = 0.2,
                  value = 0.1),
      sliderInput('n_pick',
                  'Number of Socks Picked',
                  min = 1,
                  max = 15,
                  value = 11)
    ),
    
    # main panel with tabbed interface
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Description',
                           htmlOutput('descript'),
                           actionButton('show_button', 'Show Socks'),
                           conditionalPanel('input.show_button > 0',
                                            HTML('<br>'),
                                            verbatimTextOutput('answer'))),
                  tabPanel('Prior Plots',
                           fluidRow(
                             splitLayout(cellWidths = c('48%', '48%'),
                                         plotOutput('prior_nbinom_plot'),
                                         plotOutput('prior_beta_plot'))
                           )),
                  tabPanel('Posterior Plot',
                           plotOutput('post_plot'),
                           HTML('<br><b>Summary Statistics<b>'),
                           verbatimTextOutput('summary_stat'),
                           hr(),
                           h4("Add the Line"),
                           actionButton("action", label = "Posterior Median"))
                  
      )
    )
  )
)