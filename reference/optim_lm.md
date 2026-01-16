# Optimize simulated data to match target correlations and fixed-effects regression estimates

Uses the nds3 algorithmic framework to simulate data such that the
resulting correlations and regression coefficients match specified
targets under a given regression model and input parameters.

## Usage

``` r
optim_lm(
  sim_data,
  target_cor,
  target_reg,
  reg_equation,
  target_se = NULL,
  weight = c(1, 1),
  max_iter = 1e+05,
  init_temp = 1,
  cooling_rate = NULL,
  tolerance = 1e-06,
  progress_bar = TRUE,
  max_starts = 1,
  hill_climbs = NULL,
  min_decimals = 1,
  progress_mode = "console"
)
```

## Arguments

- sim_data:

  Data frame. Predictor variables and outcome to be optimized; at least
  two columns.

- target_cor:

  Numeric vector. Target upper-triangular (excluding diagonal)
  correlation values for predictor and outcome variables.

- target_reg:

  Numeric vector. Target regression coefficients including intercept,
  matching terms in \`reg_equation\`.

- reg_equation:

  Character or formula. Regression model (e.g., "Y ~ X1 + X2 + X1:X2").

- target_se:

  Numeric vector, optional. Target standard errors for regression
  coefficients (same length as \`target_reg\`).

- weight:

  Numeric vector of length 2. Weights for correlation vs. regression
  error in the objective function. Default \`c(1, 1)\`.

- max_iter:

  Integer. Maximum iterations for simulated annealing per start. Default
  \`1e5\`.

- init_temp:

  Numeric. Initial temperature for annealing. Default \`1\`.

- cooling_rate:

  Numeric or NULL. Cooling rate per iteration (0–1); if NULL, computed
  as \`(max_iter - 10) / max_iter\`.

- tolerance:

  Numeric. Error tolerance for convergence; stops early if best error \<
  \`tolerance\`. Default \`1e-6\`.

- progress_bar:

  Logical. Show text progress bar during optimization. Default \`TRUE\`.

- max_starts:

  Integer. Number of annealing restarts. Default \`1\`.

- hill_climbs:

  Integer or NULL. Number of hill‐climbing iterations for optional local
  refinement; if NULL, skips refinement. Default \`NULL\`.

- min_decimals:

  Integer. Minimum number of decimal places for target values (including
  trailing zeros). Default \`1\`.

- progress_mode:

  Character. Either "console" or "shiny" (or "off" internally set) for
  progress handler. Default \`console\`.

## Value

A \`nds3.object\` list containing:

- best_error:

  Numeric. Minimum objective error achieved.

- data:

  Data frame of optimized predictor and outcome values.

- inputs:

  List of all input parameters for reproducibility.

- track_error:

  Numeric vector of best error at each iteration of annealing.

- track_error_ratio:

  Numeric vector of error ratios (cor vs. reg) per iteration.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Optimize given sim_data from the Descriptives module matching means and SDs.
res <- optim_lm(
  sim_data = sim_data,
  target_cor = c(.23),
  target_reg = c(2.1, 1.2, -0.8),
  reg_equation = "Y ~ X1 + X2",
  max_iter = 10000,
  hill_climbs = 50
)
} # }
```
