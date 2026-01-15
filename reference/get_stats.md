# Extract statistics from a single nds3.object

Computes and returns key analytical outputs from one \`nds3.object\`,
including model fits, parameter estimates, and summary statistics
appropriate for regression, ANOVA, or vector-based analyses.

## Usage

``` r
get_stats(result)
```

## Arguments

- result:

  A \`nds3.object\` produced by analysis functions (e.g., \`optim_vec\`,
  \`optim_aov()\`, \`optim_lm()\`, \`optim()\`).

## Value

A named \`list\` containing elements dependent on the input type:

- model:

  Model object or ANOVA table used for estimating parameters.

- reg:

  Numeric vector of regression coefficients (fixed effects) for
  regression-based objects.

- se:

  Numeric vector of standard errors corresponding to \`reg\`.

- cor:

  Numeric vector of bivariate correlations for regression-based objects.

- mean:

  Numeric vector of means: for regression-based, variable means (columns
  in wide format); for vector-based, variable means; for ANOVA,
  (sub)group means.

- sd:

  Numeric vector of standard deviations for regression- and vector-based
  objects.

- F_value:

  Numeric vector of F-statistics for ANOVA-based objects.

## Examples

``` r
 if (FALSE) { # \dontrun{
get_stats(nds3.object)
} # }
```
