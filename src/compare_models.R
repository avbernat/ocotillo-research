model_comparisonsAIC <- function(model_selection_filepath){
  
  source("src/AICprobabilities.R")
  source(model_selection_filepath)
  
  using_glmer <- FALSE
  
  if (grepl("glmer", model_selection_filepath)) {
    using_glmer <- TRUE
  }
  if (grepl("lmer", model_selection_filepath)) {
    using_glmer <- TRUE
  }
  
  AICs <- sort(summary$AIC)
  models_init <- sort(P, decreasing=TRUE, index.return=TRUE)
  top <- length(models_init$x[which(models_init$x>0.05)])
  
  AICs <- AICs[1:top]
  models <- lapply(models_init$ix[1:top],1,FUN=as.integer)
  probs <- models_init$x[1:top]
  print(rbind(AICs, models, probs))
  
  cat("\n")
  for (m_num in unlist(models)) {

    if (m_num == max(nrow(summary))) {
      m <- paste0("m", 0)
    }
    else {m <- paste0("m", m_num)}
    
    regression <- get(m)
    cat(m, end="\t")
    
    if (using_glmer) {
      print(formula(regression))
    }
    else{print(regression$call)}
  }
}
