simulate_data <- function(n, true_beta) {
  
  beta0 = true_beta[1]
  
  x = rnorm(n, mean = 0, sd = 1)
  
  logit_p = beta0 + true_beta[2] * x
  
  p = 1 / (1 + exp(-logit_p))
  
  y = rbinom(n = n, size = 1, prob = p)
  
  tibble(
    x = x,
    y = y
  )
  
}