# A function that formats the output of regressions into a quickly legible output 

###############################################################################################
# FUNCTION: tidy_regression | Formats the output of regressions into a quickly legible output
#                             in color or in black-and-white 
#
# INPUT: the fitted model by the functions lm, glm, lmer, or glmer and a Boolean, is_color,
#        which is color if TRUE and black-and-whtie if FALSE
#
# OUTPUT: spits out the effects, their coefficients, and their test statistic (t- or p-values).
# 
# COLOR CITATION: https://gist.github.com/vratiu/9780109
###############################################################################################

#source("src/colorize_output.R")

tidy_regression <- function(fit, is_color) {
  
  n_col <- length(as.data.frame(summary(fit)$coefficients))
  test_stat <- summary(fit)$coefficients[,n_col] # test_statistic: p-value (multivariate) or t-value (mixed effect)
  coeffs <- summary(fit)$coefficients[,1] 
  AIC <- AIC(fit) # for multivariate modeling
  aic <- summary(fit)$AICtab[1] # for mixed effect modeling
  name <- as.character(summary(fit)$call)
  cat(name, end="\n")
  cat("AIC: ", AIC, aic, end="\n")
  
  table <- as.data.frame(cbind(coeffs, test_stat))
  table <- cbind(fixed_effect = rownames(table), table)
  rownames(table) <- 1:nrow(table)
  as.character(table$fixed_effect[1])
  
  # Make all elements of a character vector the same length (for tidy printing purposes)
  table$fixed_effect <- as.character(table$fixed_effect)
  table$fixed_effect <- gsub("\\s", " ", format(table$fixed_effect , width=max(nchar(table$fixed_effect))))
  
  for (i in 1:nrow(table)) {
    
    coeff <- round(table$coeffs[i], 7)
    
    if (coeff < 0) {
      cat(table$fixed_effect[i], "\t")
      if (is_color) {
        cat("coeff: ", paste0("\033[0;", color=31, "m",coeff,"\033[0m","\t"))
        #cat("coeff: ", colorize(coeff, "red"), "\t") # testing out
      } else {cat("coeff: ", paste0(coeff, "\t")) }
      if (n_col < 4) {
        cat("tval: ", table$test_stat[i])
        cat('\n')
      }
      else {
        if (table$test_stat[i] < 0.05){
          cat("Pr(>|t|): ", table$test_stat[i], "*")}
        else if (table$test_stat[i] < 0.10 && table$test_stat[i] > 0.05){
          cat("Pr(>|t|): ", table$test_stat[i], ".")} 
        else {cat("Pr(>|t|): ", table$test_stat[i])}
        cat('\n')
      }
    }
    
    if (coeff > 0) {
      cat(table$fixed_effect[i], "\t")
      if (is_color) {
        cat("coeff: ", paste0("\033[0;", color=32, "m",coeff,"\033[0m","\t"))
      } else {cat("coeff: ", paste0(coeff, "\t")) }
      if (n_col < 4) {
        cat("tval: ", table$test_stat[i])
        cat('\n')
      }
      else {
        if (table$test_stat[i] < 0.05){
          cat("Pr(>|t|): ", table$test_stat[i], "*")} 
        else if (table$test_stat[i] < 0.10 && table$test_stat[i] > 0.05){
          cat("Pr(>|t|): ", table$test_stat[i], ".")} 
        else {cat("Pr(>|t|): ", table$test_stat[i])}
        cat('\n')
      }
    }
  }
  
}
