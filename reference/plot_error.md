# Plot Error Ratio Evolution for a nds3.object

Creates a plot of error versus iteration, to assess the trajectory of
the objective function value of a \`nds3.object\`.

## Usage

``` r
plot_error(nds3_obj, run = 1, show_best = TRUE, first_iter = 1)
```

## Arguments

- nds3_obj:

  A \`nds3.object\` S3 object produced by \`optim\_\*\` functions,
  containing a \`track_error\` element (numeric vector or list of
  vectors).

- run:

  Integer. Index of the run to plot when \`track_error\` is a list;
  default \`1\`.

- show_best:

  Logical. If \`TRUE\`, adds a point marking the minimum error; default
  \`TRUE\`.

- first_iter:

  Integer. Number of initial iterations to skip before plotting
  (zero-based); default \`1\` (plots from iteration 2 onward).

## Value

A \`ggplot2\` object.

## Examples

``` r
 if (FALSE) { # \dontrun{
result <- optim_lm(args = ..., ...)
plot_error(result, first_iter = 500)
} # }
```
