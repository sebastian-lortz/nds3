# Plot Error Ratio Evolution for a nds3.object

Displays how the ratio of correlation to regression error evolves across
iterations for a \`nds3.object\`, enabling assessment of balance between
objectives.

## Usage

``` r
plot_error_ratio(
  nds3_obj,
  run = 1,
  show_mean = TRUE,
  show_median = TRUE,
  show_final = TRUE
)
```

## Arguments

- nds3_obj:

  A \`nds3.object\` S3 object produced by \`optim\_\*\` functions,
  containing a \`track_error\` element (numeric vector or list of
  vectors).

- run:

  Integer. Index of the run to plot when \`track_error\` is a list;
  default \`1\`.

- show_mean:

  Logical. If \`TRUE\`, draws a horizontal line at the mean error ratio;
  default \`TRUE\`.

- show_median:

  Logical. If \`TRUE\`, draws a horizontal line at the median error
  ratio; default \`TRUE\`.

- show_final:

  Logical. If \`TRUE\`, draws a horizontal line at the final iteration's
  error ratio; default \`TRUE\`.

## Value

A \`ggplot2\` object.

## Examples

``` r
 if (FALSE) { # \dontrun{
result <- optim_lm(args = ..., ...)
plot_error(result, run = 1)
} # }
```
