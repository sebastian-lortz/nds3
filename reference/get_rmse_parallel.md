# Compute RMSE metrics across nds3.object runs

Calculates root-mean-square error (RMSE) metrics for both between-run
variability and deviations from specified target values across one or
more \`nds3.object\` results.

## Usage

``` r
get_rmse_parallel(object_list)
```

## Arguments

- object_list:

  A \`nds3.object\` or list thereof. Objects produced by analysis
  functions such as \`parallel_aov()\`, \`parallel_lm()\`.

## Value

A list with components:

- between_rmse:

  A \`data.frame\` summarizing RMSE metrics (Mean_RMSE, SD_RMSE,
  Min_RMSE, Max_RMSE) for between-run differences (not taking into
  account targets).

- target_rmse:

  A \`data.frame\` summarizing RMSE metrics for deviations from the
  original target inputs (not taking into account the mean across runs).

- data_rmse:

  A \`list\` with raw RMSE values:

  between

  :   Named numeric vector of between-run RMSEs (\`rmse_cor\`,
      \`rmse_reg\`, etc.).

  target

  :   Named numeric vector of target-run RMSEs.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Multiple-run RMSE comparison
result <- parallel_lm(args = ..., ...)
get_rmse_parallel(result)
} # }
```
