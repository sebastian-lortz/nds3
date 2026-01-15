# Reshape Data from Long to Wide Format

Converts a data.frame in long format (with an `ID` column, optional
between-subject grouping columns, a `time` column, and one or more
measurement columns) into wide format. Column names in the result follow
the pattern `<measure>_<time>`.

## Usage

``` r
long_to_wide(data)
```

## Arguments

- data:

  A `data.frame` in long format. The first column is taken as the
  subject identifier (`ID`); the next columns before `time` are treated
  as between‐subject factors; the `time` column must be named `"time"`;
  remaining columns are the measurements.

## Value

A `data.frame` in wide format, with one row per subject (and
between‐subject factor combination) and one column per measure–time
combination.
