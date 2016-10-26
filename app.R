suppressWarnings(library(shiny))
suppressWarnings(library(ggplot2))

# Define UI for application that draws a histogram
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
                        'Prior Std Deviation of Socks',
                        min = 1,
                        max = 30,
                        value = 15),
            sliderInput('frac_pair',
                        'Prior Fraction of Paired Socks',
                        min = 0,
                        max = 1,
                        value = 0.8),
            sliderInput('n_pick',
                        'Number of Socks Picked',
                        min = 1,
                        max = 15,
                        value = 11)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server = function(input, output) {

    sock_sim = reactive({
        n_samp = 100000
        replicate(n_samp, {
            # generating a sample of the parameters from the priors
            prior_mean = input$prior_mean
            prior_sd   = input$prior_sd
            n_pick     = input$n_pick
            frac_pair  = input$frac_pair
            prior_size = -prior_mean^2 / (prior_mean - prior_sd^2)

            n_socks    = rnbinom(1, mu = prior_mean, size = prior_size)
            frac_pair  = rbeta(1, shape1 = 15, shape2 = 2)
            n_pairs    = round(floor(n_socks / 2) * frac_pair)
            n_odd      = n_socks - n_pairs * 2

            # simulating picking out n_picked socks
            socks      = rep(seq_len(n_pairs + n_odd),
                             rep(c(2, 1), c(n_pairs, n_odd)))
            picked_socks = sample(socks, size =  min(n_pick, n_socks))
            sock_counts  = table(picked_socks)

            # Returning the parameters and counts of the number of matched
            # and unique socks among those that were picked out.
            c(unique  = sum(sock_counts == 1),
              pairs   = sum(sock_counts == 2),
              n_socks = n_socks,
              n_pairs = n_pairs,
              n_odd   = n_odd,
              frac_pair = frac_pair)
        })
    })

    post_samp = reactive({
        post = sock_sim()
        n_pick = input$n_pick
        post = post[ , post["unique",] == n_pick & post["n_pairs", ] == 0]
    })

    output$distPlot = renderPlot({
        post = post_samp()
    })
}

# Run the application
shinyApp(ui = ui, server = server)

