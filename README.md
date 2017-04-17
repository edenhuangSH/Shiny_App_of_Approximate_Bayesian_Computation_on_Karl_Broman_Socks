

source: http://www2.stat.duke.edu/~cr173/Sta523_Fa16/hw/hw5.html


## Background

Karl Broman is a Professor of Biostatistics at UW-Madison who is a prominent R user and a prolific tweeter. The response of the statistics / R twittersphere was to treat this as a probability modeling problem - based on that sample how many total socks are there and what proportion are paired? 


## Task 1 - Sock Shiny App

Our first goal for this project to create a web app that will allow a user to dynamically interact with the analysis by specifying different prior(s) and or parameterizations. We will create this interactive tool using Rstudio’s Shiny package. The app allows the user to select from a range of appropriate priors. Our Shiny App includes the dynamic isualizations of the priors and posteriors based on the user’s selections as well as relevant posterior summary statistics.

## Task 2 - Improving the Implementation

The analysis we are implementing is based on a computational method called approximate Bayesian computation (ABC) that trades complexity for computational efficiency. We also add local multithreading to achieve the best possible performance for the ABC sampler. 

