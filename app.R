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

        mainPanel(
            tabsetPanel(type = 'tabs',
                tabPanel('Description',
                         htmlOutput('descript'),
                         actionButton('show_button', 'Show Socks'),
                         conditionalPanel('input.show_button > 0',
                                          HTML('<br>'),
                                          verbatimTextOutput('answer'))),
                tabPanel('Prior Plots',
                         plotOutput('prior_nbinom_plot'),
                         plotOutput('prior_beta_plot')),
                tabPanel('Posterior Plot',
                         plotOutput('post_plot'))

            )
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
            a = ((1 - mu_pair) / (sig_pair) - 1/mu_pair) * (mu_pair)
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

    # plot prior beta distribution
    output$prior_beta_plot = renderPlot({
        mu_pair    = input$frac_pair
        sig_pair   = (0.1)^2
        a = ((1 - mu_pair) / (sig_pair) - 1/mu_pair) * (mu_pair)
        b = a * (1/mu_pair - 1)
        range = seq(0,1,0.01)

        beta = dbeta(seq(0,1,0.01), a, b)
        ggplot(data.frame(x = range, y = beta), aes(x = x, y = y)) +
            geom_line() +
            theme_bw() +
            labs(x = 'Fraction of paired socks',
                 y = 'Density',
                 title = 'Beta Prior')
    })

    # plot prior nbinom distribution
    output$prior_nbinom_plot = renderPlot({
        prior_mean = input$prior_mean
        prior_sd   = input$prior_sd
        prior_size = -prior_mean^2 / (prior_mean - prior_sd^2)
        range = seq(1,80,1)

        binom = dnbinom(range, mu = prior_mean, size = prior_size)
        ggplot(data.frame(x = range, y = binom), aes(x = x, y = y)) +
            geom_line() +
            theme_bw() +
            labs(x = 'Total number of Socks',
                 y = 'Density',
                 title = 'Negative Binomial Prior')

    })

    # plot posterior density of the number of socks
    output$post_plot = renderPlot({
        post  = post_samp()
        socks = data.frame(n = post['n_socks',])
        ggplot(socks, aes(x = n)) +
            geom_density() +
            theme_bw() +
            labs(y = 'Density',
                 x = 'Number of Socks',
                 title = 'Posterior Distribution of Socks')

    })

    output$descript = renderUI({
        HTML(
            '<br><b>Approximate Bayesian Computing</b><br>

            A computational method of estimating the posterior of a
            distribution when the likelihood cannot easily be derived. From
            a generative model, samples are simulated, but only the samples
            which conform to the observed data are accepted. The posterior
            parameters are estimated from the accepted sample parameters.
            More information about ABC can be found in this
            <a href=https://en.wikipedia.org/wiki/Approximate_Bayesian_computation>
            Wikipedia</a> article. <br><br>

            <b>Karl Broman\'s Socks</b><br>

            The statistician <a href=https://twitter.com/kwbroman/status/523221976001679360>Karl Broman</a> tweeted that of the first 11 socks picked from his dryer are
            unique. Can we estimate the number of total socks in Broman\'s
            dryer? A solution adapted from <a href=http://www.sumsar.net/blog/2014/10/tiny-data-and-the-socks-of-karl-broman>Rasmus Baath</a>.
            uses ABC. Play around with the prior parameters and see how the
            distribution of socks change. <br><br>

            <b>Prior Parameters</b><br>
            The prior estimate of the number socks is modeled with a
            Negative Binomial distribtion. Prior mean and
            Prior sample size control the shape of the negative binomial
            distribution. Of course socks often go missing. The fraction of
            socks that are paired is modeled with a Beta function. Fraction
            of socks controls the shape of the Beta. Finally you can pick
            how many unique socks you see from the dryer. <br><br>

            <b>Answer</b><br>
            Once you are satisified, click the button to show how many socks
            are actually in Karl Broman\'s laundry. <br><br>'
            )
    })

    output$answer = renderText(
        '23 pairs and 3 unpaired for a total of 45 socks.'
    )
}

# Run the application
shinyApp(ui = ui, server = server)

