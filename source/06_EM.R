em_exp <- function(y, delta, lambda_init = 1, tol = 1e-6, max_iter = 100) {
  lambda <- lambda_init
  iter <- 0
  diff <- Inf
  
  while (diff > tol && iter < max_iter) {
    lambda_new <- length(y) / sum(delta * y + (1 - delta) * (y + 1 / lambda))
    diff <- abs(lambda_new - lambda)
    lambda <- lambda_new
    iter <- iter + 1
  }
  
  return(lambda)
}

# Example using veteran dataset
library(survival)
data(veteran)
y <- veteran$time
delta <- as.numeric(veteran$status == 1)

lambda_hat <- em_exp(y, delta)
print(lambda_hat)
