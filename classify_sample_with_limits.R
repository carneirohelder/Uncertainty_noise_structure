classify_sample_with_limits <- function(X_sample, model, limit) {
  
  global_lower <- -limit
  global_upper <- limit
  
  X_sample <- as.matrix(X_sample)
  
  # Extract model coefficients
  coef <- as.matrix(model$finalModel$coefficients[, 1, model$bestTune$ncomp])
  
  # Predict y_mean for each sample
  y_means <- X_sample %*% coef
  
  # Classify each sample
  classifications <- ifelse(
    y_means >= global_lower & y_means <= global_upper, "Uncertain",
    ifelse(y_means < global_lower, "Class 2", "Class 1")
  )
  
  return(data.frame(
    y_mean = as.vector(y_means),
    classification = classifications
  ))
}
