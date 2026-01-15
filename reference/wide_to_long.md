# Reshape Data from Wide to Long Format

Converts a data.frame or matrix in wide format (with an `ID` column and
repeated‐measure columns named `<variable>_<time>`, e.g. `V1_1, V1_2`)
into long format. The resulting data has columns `ID`, any
between‐subject factors, `time`, and one column per measure.

## Usage

``` r
wide_to_long(data)
```

## Arguments

- data:

  A `data.frame` or `matrix` in wide format. If a matrix is provided, it
  will be coerced to a data.frame. Must have at least two columns
  matching the regex `"<var>_<time>"`.

## Value

A `data.frame` in long format with columns `ID`, any between‐subject
factors (if present), `time` (integer), and one column per measure.
