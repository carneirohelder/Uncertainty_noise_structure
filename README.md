# Predict with Uncertainty Using Replicate Noise

This repository provides a set of R functions to quantify uncertainty in predictions from machine learning models trained on replicate-based data. The method leverages **replicate-level noise** to simulate multivariate uncertainty and derive confidence intervals or probabilistic classifications for each sample.

---

## Motivation

In spectroscopy and other analytical techniques, multiple replicate measurements are collected per sample. Instead of simply averaging these replicates, this approach:

- Estimates **within-sample noise**,
- Simulates multiple synthetic versions of each sample,
- Predicts outcomes for these simulated samples,
- Uses the distribution of predictions to assess **uncertainty** and **classification confidence**.

---

## Included Functions

### `predict_with_uncertainty_parallel()`

Performs Monte Carlo simulations of replicate-level noise per sample and classifies samples using a model. Supports parallel processing.

#### Parameters:

- `X`: A matrix where the **first column** is a sample ID, and the rest are features (replicates per sample).
- `model`: A `caret` model (currently supports `pls` and `ksvm`).
- `n_sim`: Number of Monte Carlo simulations per sample (default = 30).
- `ncores`: Number of cores to use for parallelization.
- `return_simulations`: If `TRUE`, return simulated data for each sample.
- `simulations`: (Optional) Precomputed list of simulated replicate data per sample.

#### Returns:

- A `data.frame` or `list` (if `return_simulations = TRUE`) with:
  - Sample ID
  - Mean and standard deviation of predictions
  - Confidence interval bounds
  - Area under the curve (for binary classification)
  - Final classification (`"Class 1"`, `"Class 2"`, or `"Uncertain"`)

---

### `simulate_replicate_noise()`

Generates simulated replicate data for each sample using estimated within-sample noise structure.

#### Parameters:

- `X`: A matrix where the first column is sample ID and remaining columns are features.
- `n_sim`: Number of synthetic replicates to simulate per sample.

#### Returns:

- A **named list** where each element is a matrix of simulated replicates (`sim_X`) for a sample.

This output can be passed into `predict_with_uncertainty_parallel(..., simulations = ...)` to reuse precomputed noise.

---

## Example Usage

```r
# Simulate noise structure for each sample
sim_noise <- simulate_replicate_noise(X, n_sim = 100)

# Run predictions using a trained PLS or SVM model
results <- predict_with_uncertainty_parallel(
  X = X,
  model = trained_model,
  n_sim = 100,
  ncores = 4,
  simulations = sim_noise,
  return_simulations = FALSE
)