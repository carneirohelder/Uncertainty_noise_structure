predict_noise_confidence_interval <- function(model, replicate_index, n_sim = 500, return_simulations = FALSE) {
  required_pkgs <- c("MASS", "Rfast")
  missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Please install the following packages: ", paste(missing, collapse = ", "))
  }
  
  if (!inherits(model$finalModel, "mvr")) {
    stop("This function currently supports only linear models like PLS (mvr).")
  }
  
  # Extract training data (excluding .outcome)
  X_train <- as.matrix(model$trainingData[, -ncol(model$trainingData)])
  
  # Unique sample identifiers
  sample_ids <- unique(replicate_index)
  
  # Collect residuals per sample
  all_residuals <- lapply(sample_ids, function(id) {
    replicates <- X_train[replicate_index == id, , drop = FALSE]
    x_bar <- colMeans(replicates)
    sweep(replicates, 2, x_bar)  # Subtract mean
  })
  
  # Combine all residuals
  E_all <- do.call(rbind, all_residuals)
  E_all <- scale(E_all, center = TRUE, scale = FALSE)
  
  Sigma_global <- Rfast::cova(E_all, center = FALSE)
  
  # Simulate noise-only data
  zero_mean <- rep(0, ncol(X_train))
  sim_X <- MASS::mvrnorm(n = n_sim, mu = zero_mean, Sigma = Sigma_global)
  
  # Predict
  coef <- as.matrix(model$finalModel$coefficients[, 1, model$bestTune$ncomp])
  y_pred <- as.vector(sim_X %*% coef)
  
  # Confidence interval
  y_mean <- 0
  y_sd <- sd(y_pred)
  lower <- y_mean - 2 * y_sd
  upper <- y_mean + 2 * y_sd
  
  if (return_simulations) {
    return(list(lower = lower, upper = upper, sim_X = sim_X))
  } else {
    return(c(lower = lower, upper = upper))
  }
}
