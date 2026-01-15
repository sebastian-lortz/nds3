# Check plausibility of a reported mean with the GRIM test (Brown & Heathers 2007)

Performs the GRIM (Granularity-Related Inconsistency of Means) test to
assess whether a reported mean is numerically possible given the sample
size.

## Usage

``` r
check_grim(n, target_mean, decimals, tol.r = .Machine$double.eps^0.5)
```

## Arguments

- n:

  Integer. Sample size; a positive whole number.

- target_mean:

  Numeric. Reported mean to be tested for plausibility.

- decimals:

  Integer. Number of decimal places in the reported mean.

- tol.r:

  Numeric. Tolerance for rounding errors; a non-negative value. Default
  is the square root of machine double precision epsilon.

## Value

A list with components:

- test:

  Logical. TRUE if the reported mean is plausible.

- grim_mean:

  Numeric. The adjusted mean that is numerically plausible (rounded to
  \`decimals\`).

## Examples

``` r
if (FALSE) { # \dontrun{
check_grim(10, 3.7, 1)
} # }
```
