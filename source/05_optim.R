optim_function = function(simdata) {
  
  fit = optim(y ~ x, method = "BFGS", data = simdata)
  
  return(fit)
}