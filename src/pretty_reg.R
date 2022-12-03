## Renaming Regression Formulas Function

rename_regformula = function (eqs, response_var, predA, predB, predC, predD) {
  # eqs:              list of linear regression formals as characters
  # response_var:     response variable as a character
  # predA:            predictor variable A as a character
  # predB:            predictor variable B as a character
  # predC:            predictor variable C as a character
  # predD:            predictor variable D as a character
  
  for (i in 1:length(eqs)) {
    if (grepl("\\s[C]", eqs[i])) {
      C = paste0(" ", predC)
      eqs[i] =  gsub("\\s[C]", C, eqs[i])
    }
    if (grepl("\\s[A]", eqs[i])) {
      A = paste0(" ", predA)
      eqs[i] =  gsub("\\s[A]", A, eqs[i])
    }
    if (grepl("\\s[B]", eqs[i])) {
      B = paste0(" ", predB)
      eqs[i] =  gsub("\\s[B]", B, eqs[i])
    }
    if (grepl("\\s[D]", eqs[i])) {
      D = paste0(" ", predD)
      eqs[i] =  gsub("\\s[D]", D, eqs[i])
    }
    if (grepl("[R]\\s", eqs[i])) {
      R = paste0(response_var, " ")
      eqs[i] = gsub("[R]\\s", R, eqs[i])
    }
  }
  return(eqs)
}

rename_regformulaME = function (eqs, response_var, predA, predB, predC, randX, randY) {
  # eqs:              list of linear regression formals as characters
  # response_var:     response variable as a character
  # predA:            predictor variable A as a character
  # predB:            predictor variable B as a character
  # predC:            predictor variable C as a character
  # randX:            random variable X as a character
  # randY:            random variable Y as a character
  for (i in 1:length(eqs)) {
    if (grepl(" A", eqs[i], fixed = TRUE)) {
      eqs[i] =  gsub("A", predA, eqs[i])
    }
    if (grepl(" B", eqs[i], fixed = TRUE)) {
      eqs[i] =  gsub("B", predB, eqs[i])
    }
    if (grepl(" C", eqs[i], fixed = TRUE)) {
      eqs[i] =  gsub("C", predC, eqs[i])
    }
    if (grepl("X", eqs[i], fixed = TRUE)) {
      eqs[i] =  gsub("X", randX, eqs[i])
    }
    if (grepl("Y", eqs[i], fixed = TRUE)) {
      eqs[i] =  gsub("Y", randY, eqs[i])
    }
    if (grepl( "R", eqs[i], fixed = TRUE)) {
      eqs[i] = gsub("R", response_var, eqs[i])
    }
  }
  return(eqs)
}