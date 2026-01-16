# Estimate objective function weights via pilot simulations

Runs single or multiple optimizations (i.e. optim_lm) to calibrate the
balance between correlation and regression components in the objective
function, yielding suggested weights.

## Usage

``` r
weights_est(
  sim_runs,
  sim_data,
  target_cor,
  target_reg,
  target_se = NULL,
  reg_equation,
  max_iter = 1e+05,
  init_temp = 1,
  cooling_rate = NULL,
  tolerance = 1e-06,
  progress_bar = TRUE,
  weight = c(1, 1),
  pool_range = 10,
  max_starts = 1,
  parallel_start = 1,
  move_prob = list(start = c(residual = 0, k_cycle = 0, local = 0.25, tau = 0.75), end =
    c(residual = 0.2, k_cycle = 0.1, local = 0.7, tau = 0)),
  min_decimals = 0,
  progress_mode = "console"
)
```

## Arguments

- sim_runs:

  Integer; number of simulation runs.

- sim_data:

  Data frame. Predictor variables and outcome to be optimized; at least
  two columns.

- target_cor:

  Numeric vector. Target upper-triangular (excluding diagonal)
  correlation values for predictor and outcome variables.

- target_reg:

  Numeric vector. Target regression coefficients including intercept,
  matching terms in \`reg_equation\`.

- target_se:

  Numeric vector, optional. Target standard errors for regression
  coefficients (same length as \`target_reg\`).

- reg_equation:

  Character or formula. Regression model (e.g., "Y ~ X1 + X2 + X1:X2").

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

- weight:

  Numeric vector of length 2. Weights for correlation vs. regression
  error in the objective function. Default \`c(1, 1)\`.

- pool_range:

  Integer; the range of best error values to pool for estimating the
  weights. Default is \`10\`.

- max_starts:

  Integer. Number of annealing restarts. Default \`1\`.

- parallel_start:

  Number of independent runs (parallel or sequential) to simulate the
  weights.

- move_prob:

  List. Start/end move probabilities for operations: residual swap,
  k-cycle, local swap, tau reordering.

- min_decimals:

  Integer. Minimum number of decimal places for target values (including
  trailing zeros). Default \`1\`.

- progress_mode:

  Character. Either "console" or "shiny" for progress handler. Default
  \`console\`.

## Value

A list with components:

- weights:

  Numeric vector of estimated weights for (correlation/regression).

- data:

  The optimized data set from the final run.

- track_error:

  Numeric vector of best error at each iteration of annealing.

- error_ratio:

  Numeric vector of pilot-run error ratios used in weight estimation.

## Examples

``` r
 if (FALSE) { # \dontrun{
# estimate weights of objective function
parallel_lm(
sim_runs = 1,
  parallel_start = 7,
  return_best_solution = FALSE,
  sim_data = sim_data,
  target_cor = c(.23),
  target_reg = c(2.1, 1.2, -0.8),
  reg_equation = "Y ~ X1 + X2",
  max_iter = 10000,
  hill_climbs = 50
)
} # }
```
