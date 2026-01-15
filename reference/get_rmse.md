# Compute RMSE for a single nds3.object result

Calculates root-mean-square error (RMSE) metrics for a single
\`nds3.object\` output, comparing model estimates of the simulated data
against original target inputs.

## Usage

``` r
get_rmse(result)
```

## Arguments

- result:

  A \`nds3.object\` produced by analysis functions (e.g., \`optim_vec\`,
  \`optim_aov()\`, \`optim_lm()\`, \`optim()\`).

## Value

A named \`list\` of RMSE values. Possible elements:

- rmse_cor:

  Numeric. RMSE of correlation estimates for LM-based objects.

- rmse_reg:

  Numeric. RMSE of regression coefficient estimates for LM-based
  objects.

- rmse_se:

  Numeric. RMSE of standard error estimates for LM-based objects.

- rmse_F:

  Numeric. RMSE of F-values for ANOVA-based objects.

- rmse_mean:

  Numeric. RMSE of means for vector-based objects.

- rmse_sd:

  Numeric. RMSE of standard deviations for vector-based objects.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Regression-based result
result <- optim_lm(args = ..., ...)
get_rmse(result)
} # }
```
