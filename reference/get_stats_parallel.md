# Aggregate statistics across nds3.object runs

Computes summary metrics (mean, median, standard deviation, minimum, and
maximum) for each numeric output component across multiple
\`nds3.object\` results.

## Usage

``` r
get_stats_parallel(object_list)
```

## Arguments

- object_list:

  A \`list\` of \`nds3.object\` instances. Outputs from functions (e.g.,
  \`optim_vec\`, \`optim_aov()\`, \`optim_lm()\`, \`optim()\`).

## Value

A \`list\` of \`data.frame\` objects. Each data.frame corresponds to one
numeric component (e.g., \`reg\`, \`se\`, \`cor\`, \`mean\`, \`sd\`),
and contains columns:

- mean:

  Numeric. Mean of the component across runs.

- med:

  Numeric. Median of the component across runs.

- sd:

  Numeric. Standard deviation across runs.

- min:

  Numeric. Minimum value observed across runs.

- max:

  Numeric. Maximum value observed across runs.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Single-threaded example
result <- parallel_lm(args = ..., ...)
get_stats_parallel(result)
} # }
```
