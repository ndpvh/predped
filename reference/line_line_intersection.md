# Compute the line-line intersection between several segments. Is a vectorized function with minimal loss of time when the number of segments to test increases.

Compute the line-line intersection between several segments. Is a
vectorized function with minimal loss of time when the number of
segments to test increases.

## Usage

``` r
line_line_intersection(segments_1, segments_2, return_all = FALSE, cpp = TRUE)
```

## Arguments

- segments_1:

  Matrix with four columns denoting the x- and y-coordinates that make
  up the line segment. Should be in order x_1, y_1, x_2, y_2.

- segments_2:

  Matrix of line segments that \`segments_1\` should be tested with.
  Should have the same structure as \`segments_1\`

- return_all:

  Logical denoting whether it should return the intersection of all
  segments to each other. If true, will include indicators of which
  segments were compared. Defaults to \`FALSE\`.

- cpp:

  Logical denoting whether to use the R (`cpp = FALSE`) or Rcpp version
  (`cpp = TRUE`) of this function. Defaults to `TRUE`

## Value

Returns a logical denoting whether any of the segments in
