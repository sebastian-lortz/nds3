# Perform hill-climbing optimization

Executes a hill-climbing algorithm to iteratively improve a candidate
data set by minimizing a supplied error function. Supports both the LM
modules.

## Usage

``` r
hill_climb(
  current_candidate,
  error_function,
  N,
  hill_climbs = 100,
  num_preds = NULL,
  progress_bar = TRUE,
  neighborhood_size = 4,
  outcome = NULL,
  progressor = NULL,
  pb_interval = NULL
)
```

## Arguments

- current_candidate:

  A data frame representing the initial candidate solution to be
  optimized.

- error_function:

  An objective function that takes a candidate and returns a list
  containing element \`\$total_error\`.

- N:

  Integer. Sample size; the number of subjects in \`current_candidate\`.

- hill_climbs:

  Integer. Maximum number of iterations for hill climbing. Default is
  1e2.

- num_preds:

  Integer. Number of predictors (columns).

- progress_bar:

  Logical. Whether to display a text progress bar. Default is TRUE.

- neighborhood_size:

  Integer. Number of candidate moves evaluated per iteration. Default is
  4.

- outcome:

  Optional vector. Outcome variable for standard model moves (unused if
  error_function handles it).

- progressor:

  Optional function. Callback for external progress updates (internal
  use).

- pb_interval:

  Optional numeric. Interval (in iterations) between progressor calls.

## Value

A list with components:

- best_candidate:

  The optimized candidate structure achieving lowest error.

- best_error:

  Numeric. The minimum value of the objective function found during
  optimization.

## Examples

``` r
 if (FALSE) { # \dontrun{
hill_climb(
 current_candidate = data.frame(),
 outcome = NULL,
 N = 100,
 error_function = function(candidate) {},
 hill_climbs = 100,
 num_preds = NULL,
 progress_bar = TRUE,
 progressor = NULL,
 pb_interval= NULL)
 } # }
```
