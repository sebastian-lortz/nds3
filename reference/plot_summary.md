# Plot Summary of Simulated vs. Target Statistics for a nds3.object

Visualizes discrepancies between simulated and target summary statistics
(e.g., means, SDs, F-values, correlations, regression coefficients) for
a single \`nds3.object\`.

## Usage

``` r
plot_summary(nds3_obj, standardised = TRUE, eps = 1e-12)
```

## Arguments

- nds3_obj:

  A \`nds3.object\` produced by \`optim\_\*\` functions, containing both
  simulated data/results and \`inputs\$target\_\*\` values.

- standardised:

  Logical; if \`TRUE\`, differences are divided by target values (except
  when targets are near zero); default \`TRUE\`.

- eps:

  Numeric; threshold below which a target is treated as zero for
  standardization; default \`1e-12\`.

## Value

A \`ggplot2\` object.

## Examples

``` r
 if (FALSE) { # \dontrun{
res <- optim_aov(...)
plot_summary(res, standardised = FALSE)
} # }
```
