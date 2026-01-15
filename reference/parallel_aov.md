# Optimize multiple data sets to match ANOVA F-values

Uses the nds3 algorithmic framework to simulate multiple data sets that
produce target ANOVA F-statistics under a specified factorial design
given input parameters.

## Usage

``` r
parallel_aov(
  parallel_start = 3,
  return_best_solution = FALSE,
  N,
  levels,
  target_group_means,
  target_f_list,
  integer,
  range,
  formula,
  factor_type,
  subgroup_sizes = NULL,
  df_effects = NULL,
  tolerance = 1e-08,
  typeSS = 3,
  max_iter = 1000,
  init_temp = 1,
  cooling_rate = NULL,
  max_step = 0.2,
  max_starts = 1,
  checkGrim = FALSE,
  min_decimals = 1,
  progress_mode = "console"
)
```

## Arguments

- parallel_start:

  Number of independent runs (parallel or sequential) to simulate data
  sets.

- return_best_solution:

  Logical; return only the best run if TRUE.

- N:

  Integer. Total number of subjects (sum of \`subgroup_sizes\`).

- levels:

  Integer vector. Number of factor levels per factor in the design.

- target_group_means:

  Numeric vector. Desired means for each group in the design.

- target_f_list:

  List with components:

  F

  :   Numeric vector of target F-statistics.

  effect

  :   Character vector of effect names matching \`F\`.

  contrast

  :   Optional character formula for contrasts.

  contrast_method

  :   Optional character specifying contrast method.

- integer:

  Logical. If TRUE, candidate values are treated as integers, if FALSE
  treated as continuous values.

- range:

  Numeric vector of length 2. Lower and upper bounds for candidate
  means.

- formula:

  Formula or character. Model formula used to compute F-values (e.g.,
  \`y ~ A + B + A\*B\`).

- factor_type:

  Character vector. Type of each factor (\`"between"\` or \`"within"\`)
  matching length of \`levels\`.

- subgroup_sizes:

  Numeric vector. Optional sizes of each between-subjects group for
  unbalanced designs; length must equal product of \`levels\` for
  between factors.

- df_effects:

  Numeric vector. Degrees of freedom of the model effects. Default is
  \`NULL\`.

- tolerance:

  Numeric. Error tolerance for convergence; stops early if best error \<
  \`tolerance\`. Default \`1e-6\`.

- typeSS:

  Integer. Type of sums-of-squares for ANOVA (2 or 3). Default is 3.

- max_iter:

  Integer. Maximum iterations per restart. Default is 1e3.

- init_temp:

  Numeric. Initial temperature for annealing. Default is 1.

- cooling_rate:

  Numeric. Cooling rate per iteration (between 0 and 1); if NULL,
  calculated automatically as \`(init_temp-10)/init_temp\`.

- max_step:

  Numeric. Maximum move size as proportion of \`range\`. Default is 0.2.

- max_starts:

  Integer. Number of annealing restarts. Default is 1.

- checkGrim:

  Logical. If TRUE and \`integer = TRUE\`, perform GRIM checks on
  \`target_group_means\`. Default is FALSE.

- min_decimals:

  Integer. Minimum number of decimal places for target values (including
  trailing zeros). Default \`1\`.

- progress_mode:

  Character. Either "console" or "shiny" for progress handler. Default
  \`console\`.

## Value

A list of multiple \`nds3.object\`s, each containing:

- best_error:

  Numeric. Minimum error (RMSE) achieved.

- data:

  Data frame of optimized outcome values (and grouping variables).

- inputs:

  List of all input arguments.

- track_error:

  Numeric vector of best error at each iteration.

- grim:

  List of the GRIM results.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Balanced 2x2 design
parallel_aov(
  parallel_start = 7,
  return_best_solution = FALSE,
  N = 40,
  levels = c(2, 2),
  target_group_means = c(1, 2, 3, 4),
  target_f_list = list(effect = c("A", "B"),
                       F = c(5.6, 8.3), ),
  formula = y ~ A + B + A*B,
  factor_type = c("between", "between"),
  range = c(0, 5),
  integer = FALSE,
  max_iter = 1000,
  max_starts = 3
)
} # }
```
