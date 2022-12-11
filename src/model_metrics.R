

calculate_lk_weights = function(model_list, best_fit, R, A, B, C, D="", X="", Y="", is_lm=TRUE) {
  likelihoods = c()
  deltas = c()
  eqs = c()
  aics = c()
  for (model in model_list){
    
    # Store AIC values
    aics = c(aics, AIC(model))
    
    # Calculate relative likelihood of each model
    delta = abs(AIC(best_fit) - AIC(model))
    deltas = c(deltas, delta)
    likelihood = exp( -0.5 * delta)
    likelihoods = c(likelihoods, likelihood)
    
    # Store equation
    if (is_lm) {
      equation = paste(model$call)[2]
    }
    else {
      equation = c(paste(formula(model))[2], paste(formula(model))[1], paste(formula(model))[3]) %>%
                      paste(collapse=' ')
    }
    eqs = c(eqs, equation)
    
  }
  
  if (is_lm) {
    eqs = rename_regformula(eqs, R, A, B, C, D)
  }
  else {
    eqs = rename_regformulaME(eqs, R, A, B, C, X, Y)
  }
  
  weights = c()
  for (l in likelihoods) {
    # Calculate the Akaike weight for each model by dividing likelihood by the sum of likelihoods across all models.
    weight = l/sum(likelihoods)
    weights = c(weights, weight)
  }
  
  summary_table = cbind(eqs, aics, deltas, likelihoods, weights)
  colnames(summary_table) = c("Equation", "AIC","dAIC", "Likelihood","Weight")
 
  summary_tbl = as.data.frame(summary_table) 
  summary_tbl$AIC = as.numeric(summary_tbl$AIC)
  summary_tbl$dAIC = as.numeric(summary_tbl$dAIC)
  summary_tbl$Likelihood = as.numeric(summary_tbl$Likelihood)
  summary_tbl$Weight = as.numeric(summary_tbl$Weight)
  
  is.num <- sapply(summary_tbl, is.numeric)
  summary_tbl[is.num] <- lapply(summary_tbl[is.num], round, 5)
  
  return(summary_tbl)
}
