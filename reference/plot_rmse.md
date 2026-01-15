# Plot RMSE Comparison for nds3.object runs

Visualizes the root-mean-square error (RMSE) distributions for
between-run variability versus deviations from target values across
multiple \`nds3.object\` runs (e.g. parallel_aov()).

## Usage

``` r
plot_rmse(object_list)
```

## Arguments

- object_list:

  A \`nds3.object\` or list thereof, typically output from \`optim\_\*\`
  functions.

## Value

A \`ggplot2\` object.

## Examples

``` r
 if (FALSE) { # \dontrun{
results <- parallel_aov(args = ..., ...)
plot_rmse(results)
} # }
```
