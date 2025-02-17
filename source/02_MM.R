mm_function - function(x, y, beta0 = c(0,0), tol = 1e-6, max_iter = 100) {
  
  iter = 1
  tol_criteria = Inf
  
  # define vectors to store elements of interest
  p = length(beta0)
  objective = rep(NA, length = max_iter)
  theta_vec = matrix(NA, nrow = max_iter, ncol = p)
  
  ## Add initial value
  theta0 = c(1, 1)
  theta = theta0
  
  while(iter < max_iter  & tol_criteria > tol){
    
    pi = exp(x %*% theta)/ (1 + exp(x %*% theta))
    
    ###############################################################
    ## Majorization
    ###############################################################
    
    objective[iter] = - 1 / p * sum(pi) + sum(y * x %*% theta)
    
    ###############################################################
    ## Minimization
    ###############################################################

    theta[1] = - sum(pi * x[,1]) + sum(y * x[,1])
    theta[2] = - sum(pi * x[,2]) + sum(y * x[,2])
    theta_vec[,iter] = theta
    ###############################################################
    
    if(iter > 1){
      tol_criteria = abs(objective[iter] - objective[iter-1])
    }
    iter = iter + 1
    message(paste0("iteration: ", iter, "; ll: ", round(tol_criteria, 4)))
    
  
  }
  
  # compute SE
  w_diag = (pi * (1 - pi)) * exp(-p * x %*% theta) * exp(p * x %*% theta)
  W = diag(as.vector(W_diag), nrow = length(y))
  hessian = -t(x) %*% W %*% x
  
  se = sqrt(diag(solve(-hessian)))
  
  return(list(solution = theta, 
              se = se,
              theta_history = theta_vec,
              objective = objective,
              converged = (iter < max_iter),
              niter = iter)) 
  
}  