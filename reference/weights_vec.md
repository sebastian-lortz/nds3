# Estimate weighting factors for mean and SD errors

Compute weighting factors to match a target mean and SD across
variables.

## Usage

``` r
weights_vec(
  N,
  target_mean,
  target_sd,
  range,
  obj_weight = c(1, 1),
  integer,
  int.probs = NULL,
  est_iter = 5000,
  eps = 0.001,
  max_weight = 10000,
  metric = "mean"
)
```

## Arguments

- N:

  Integer. Number of values in each vector.

- target_mean:

  Named numeric vector. Desired means for each variable (names identify
  columns).

- target_sd:

  Named numeric vector. Desired standard deviations for each variable.

- range:

  Numeric vector of length 2 or numeric matrix. Allowed value range for
  all variables (vector), or per-variable bounds as a two-row matrix
  matching \`target_mean\`.

- obj_weight:

  List of numeric vectors length 2, one per variable. Weights for mean
  vs. SD error. Default \`c(1,1)\`.

- integer:

  Logical vector: TRUE for integer-valued vectors.

- int.probs:

  List of numeric vectors, one per variable. Sampling probabilities for
  integer moves; NULL for uniform.

- est_iter:

  Number of Monte Carlo iterations to estimate weights.

- eps:

  Numeric. Small constant to avoid division by zero in objective.
  Default \`1e-12\`.

- max_weight:

  Maximum allowed weight magnitude.

- metric:

  Character: "mean" or "median" for summarizing estimated weights.

## Value

A list of length equal to \`target_mean\`, where each element is a
numeric vector of length 2 containing the estimated weights for mean vs.
SD error for that variable.

## Details

Computes quasi-optimal weights balancing mean and standard deviation
errors for multiple variables by running Monte Carlo optimization to
estimate relative baseline contribution of each term in the objective
function.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Estimate weights
weights_vec(
  N = 100,
  target_mean = c(5,10),
  target_sd   = c(2,3),
  integer     = c(TRUE,TRUE),
  range       = matrix(c(0,15, 0,20), nrow = 2),
)
} # }
```
