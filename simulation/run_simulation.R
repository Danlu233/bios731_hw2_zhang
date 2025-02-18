# This file produces implementing logistic regression in R four different ways
####################################################################

library(tidyverse)


###############################################################
## define or source functions used in code below
###############################################################

source(here::here("source", "01_newton.R"))
source(here::here("source", "02_MM.R"))
source(here::here("source", "03_GLM.R"))
source(here::here("source", "04_optim.R"))

###############################################################
## set simulation design elements
###############################################################


nsim = 1


n = 200
beta_true = c(1, 0.3)


###############################################################
## start simulation code
###############################################################

# set a random seed
set.seed(1234)

for(i in 1:nsim){
  
  
  ####################
  # simulate data
  simdata = get_simdata(n = params$n,
                        beta_treat = params$beta_true,
                        sigma2 = params$sigma2_true)
  
  ####################
  # apply method(s)
  fit = fit_model(simdata)
  
  ####################
  # calculate estimates
  estimates = get_estimates(model_fit = fit,
                            true_beta = params$beta_true)
  
  ####################
  # store results, including estimates, speed, parameter scenarios
  estimates = estimates %>%
    mutate(true_beta = params$beta_true,
           n = params$n,
           sigma2_true = params$sigma2_true)
  
  results[[i]] = estimates
  
}

####################
# save results
# note that I am saving results outside of the for loop. For slow simulations,
# you may want to save each iteration separately
filename = paste0("scenario_", scenario, ".RDA")
save(results,
     file = here::here("results", filename))