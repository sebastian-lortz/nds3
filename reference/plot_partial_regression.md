# Partial Regression Plots for a Linear Model

Given a fitted linear model, this function computes partial‐regression
plots for each term in the model. It regress out the other predictors
and plots the residuals against each term’s residuals, annotating with
the slope (beta) and residual standard deviation.

## Usage

``` r
plot_partial_regression(model)
```

## Arguments

- model:

  A fitted `lm` model object.

## Value

A named list of `ggplot2` objects, one per predictor term in the model.
