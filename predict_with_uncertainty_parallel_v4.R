predict_with_uncertainty <- function(X,
                                    model, 
                                    n_sim = 500, 
                                    ncores = parallel::detectCores() - 1,
                                    return_simulations = FALSE,
                                    simulations = NULL) {
  
  required_pkgs <- c("MASS", "Rfast", "future.apply", "future")
  missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Please install the following packages: ", paste(missing, collapse = ", "))
  }
  
  X <- as.matrix(X)
  X[,1] <- as.numeric(X[,1])  # Ensure sample IDs are numeric
  sample_ids <- unique(X[, 1])
  
  future::plan(future::multisession, workers = ncores)
  
  results <- future.apply::future_lapply(sample_ids, function(id) {
    replicates <- X[X[, 1] == id, -1, drop = FALSE]
    x_bar <- colMeans(replicates)
    
    sim_X <- if (!is.null(simulations)) {
      if (!id %in% names(simulations)) {
        stop("Simulation data not found for sample ID: ", id)
      }
      simulations[[as.character(id)]]  # Ensure you handle character keys if needed
    } else {
      E <- sweep(replicates, 2, x_bar)
      Sigma <- Rfast::cova(E, center = FALSE)
      eig <- eigen(Sigma, symmetric = TRUE)
      Q <- eig$vectors
      sqrt_Lambda <- sqrt(pmax(eig$values, 0))
      
      Z_all <- array(rnorm(n_sim * ncol(replicates) * nrow(replicates)), 
                     dim = c(ncol(replicates), nrow(replicates), n_sim))
      
      sim_X <- sapply(1:n_sim, function(i) {
        Z <- Z_all[, , i]
        E_sim <- t(Q %*% (sqrt_Lambda * Z))
        colMeans(E_sim)
      })
      sim_X <- t(matrix(rep(x_bar, n_sim), ncol = n_sim) + sim_X)
    }
    
    if (inherits(model$finalModel, "ksvm")) {
      y_pred <- predict(model$finalModel, sim_X, type = "decision")
      y_pred <- -y_pred
    } else if (inherits(model$finalModel, "mvr")) {
      coef <- as.matrix(model$finalModel$coefficients[, 1, model$bestTune$ncomp])
      y_pred <- as.vector(as.matrix(sim_X) %*% coef)
    } else {
      stop("Unsupported model type: ", class(model$finalModel)[1])
    }
    y_mean <- mean(y_pred)
    y_sd <- sd(y_pred)
    area_below_0 <- pnorm(0, mean = y_mean, sd = y_sd)
    area_above_0 <- 1 - area_below_0
    lower <- y_mean - 2 * y_sd
    upper <- y_mean + 2 * y_sd
    
    classification <- if (0 >= lower && 0 <= upper) {
      "Uncertain"
    } else if (y_mean < 0) {
      "Class 2"
    } else {
      "Class 1"
    }
    
    result <- list(
      sample = id,
      y_mean = y_mean,
      y_sd = y_sd,
      area_above_0 = area_above_0,
      area_below_0 = area_below_0,
      classification = classification
    )
    
    if (return_simulations) {
      result$sim_X <- sim_X
    }
    
    return(result)
  }, future.seed = TRUE)
  
  future::plan(future::sequential)
  
  if (return_simulations) {
    return(results)
  } else {
    results_df <- do.call(rbind.data.frame, results)
    return(results_df)
  }
}
