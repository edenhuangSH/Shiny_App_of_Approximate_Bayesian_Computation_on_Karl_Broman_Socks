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

# Define server logic required to simulate socks and draw a density plot
server = function(input, output) {

    sock_sim = reactive({
        set.seed(1)
        n_samp = 2000
        replicate(n_samp, {

            # input parameters
            prior_mean = input$prior_mean
            prior_sd   = input$prior_sd
            n_pick     = input$n_pick
            mu_pair    = input$frac_pair
            sig_pair   = (0.1)^2

            # calculating parameter values
            prior_size = -prior_mean^2 / (prior_mean - prior_sd^2)
            a = ((1 - mu_pair) / (sig_pair^2) - 1/mu_pair) * (mu_pair)
            b = a * (1/mu_pair - 1)

            # generating a sample of the parameters from the priors
            n_socks    = rnbinom(1, mu = prior_mean, size = prior_size)
            frac_pair  = rbeta(1, a, b)
            n_pairs    = round(floor(n_socks / 2) * frac_pair)
            n_odd      = n_socks - n_pairs * 2

            # simulating picking out n_picked socks
            socks        = rep(seq_len(n_pairs + n_odd),
                               rep(c(2, 1), c(n_pairs, n_odd)))
            picked_socks = sample(socks, size =  min(n_pick, n_socks))
            sock_counts  = table(picked_socks)

            # returning a vector of results
            c(unique  = sum(sock_counts == 1),
              pairs   = sum(sock_counts == 2),
              n_socks = n_socks,
              n_pairs = n_pairs,
              n_odd   = n_odd,
              frac_pair = frac_pair)
        })
    })

    # accept samples that match the data
    post_samp = reactive({
        post = sock_sim()
        n_pick = input$n_pick
        post = post[ , post["unique",] == n_pick]
    })

    # plot density of the number of socks
    output$distPlot = renderPlot({
        post  = post_samp()
        socks = data.frame(n = post['n_socks',])
        ggplot(socks, aes(x = n)) +
            geom_density() +
            theme(rect = element_rect(fill='#F0F0F0', colour='#F0F0F0'),
                  text = element_text(face='bold'),
                  panel.grid.major = element_line(color='gray80', size=0.6),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill='#F0F0F0'),
                  axis.text =  element_text(size=13),
                  axis.title = element_text(size=13),
                  plot.title = element_text(size=20),
                  axis.ticks = element_line(color='gray80'),
                  legend.position="none" ) +
            labs(y = 'Density',
                 x = 'Number of Socks',
                 title = 'Posterior Distribution of Socks')

    })
}

# Run the application
shinyApp(ui = ui, server = server)

