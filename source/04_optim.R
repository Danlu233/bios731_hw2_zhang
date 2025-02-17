optim_function = function(x, y) {
  
  fit = optim(y ~ x, method = "BFGS")
  
  return(fit)
}