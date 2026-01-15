# Plot cooling schedule of a nds3.object

Visualizes the squared annealing temperature across iterations for a
given \`nds3.object\`, allowing inspection of the cooling schedule used
in optimization.

## Usage

``` r
plot_cooling(nds3_obj)
```

## Arguments

- nds3_obj:

  A \`nds3.object\` returned by one of the \`optim\_\*\` functions,
  containing \`inputs\$max_iter\`, \`inputs\$init_temp\`, and
  \`inputs\$cooling_rate\`.

## Value

A \`ggplot2\` object.

## Examples

``` r
 if (FALSE) { # \dontrun{
result <- optim_aov(args = ..., ...)
plot_cooling(result)
} # }
```
