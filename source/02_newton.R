library(tictoc)

newton_function = function(x, y, beta0 = c(0,0), tol = 1e-6, max_iter = 100) {
  
  tic()
  beta_cur = beta0
  beta_history = gradient_vec = matrix(NA, nrow = max_iter, 
                                       ncol = length(beta0))
  
  x = cbind(1,x)
  for (iter in 1:max_iter) {
    
    # store results
    beta_history[iter,] = beta_cur
    
    pi = exp(x%*%beta_cur) / (1 + exp(x%*%beta_cur))
    # Compute the gradient and hessian
    gradient = as.numeric( crossprod(x, y - pi))
    
    w = diag(pi * (1-pi))
    hessian <- -1 * crossprod(x, w) %*% x
    
    gradient_vec[iter,] = gradient
    
    #se
    se = - solve(hessian)
    
    # Check stopping criterion
    if(sqrt(sum(gradient^2)) < tol){
      message("Converged in", iter, "iterations.\n")
      break
    }
    
    # Update the solution
    beta_cur = beta_cur - solve(hessian) %*% gradient
  }
  
  return(list(solution = beta_cur, 
              se = se,
              beta_history = beta_history,
              gradient = gradient_vec,
              converged = (iter < max_iter),
              niter = iter, 
              time = toc(quiet = T)$callback_msg)) 
  
}