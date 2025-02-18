library(tictoc)

glm_function = function(simdata) {
  
  tic()
  
  fit = glm(y~x, family = binomial(link = "logit"), data = simdata)
  
  return(list(solution = fit$coefficients, 
              se = sqrt(diag(vcov(fit))),
              converged = fit$converged,
              niter = fit$iter, 
              time = toc(quiet = T)$callback_msg)) 
  
}