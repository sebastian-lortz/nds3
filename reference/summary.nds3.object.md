# Summarize a nds3.object

Provides a comprehensive summary of a \`nds3.object\`, including RMSE,
model components, and statistics depending on the analysis type (LM,
ANOVA, or vec).

## Usage

``` r
# S3 method for class 'nds3.object'
summary(object, ...)
```

## Arguments

- object:

  A \`nds3.object\` produced by \`optim\_\*\` functions.

- ...:

  Additional arguments (unused).

## Value

An object of class \`summary.nds3.object\` containing:

- rmse:

  Named list of RMSE values.

- inputs:

  Original input list.

- data:

  Optimized data frame.

- best_error:

  Numeric. Minimum objective error achieved.

- track_error:

  Numeric vector of best error at each iteration.

- track_error_ratio:

  Numeric vector of error ratios (cor vs. reg) per iteration (if
  available).

- model:

  Fitted regression model or ANOVA table (if applicable).

- coefficients:

  regression estimates (LM only).

- std_errors:

  Coefficient standard errors (LM only).

- correlations:

  Correlation values (LM only).

- F_value:

  ANOVA F statistics (ANOVA only).

- means:

  Group or variable means.

- sds:

  Group or variable standard deviations (vector only).

## Examples

``` r
 if (FALSE) { # \dontrun{
res_lm <- optim_lm(args = ..., ...)
summary(res_lm)
} # }
```
