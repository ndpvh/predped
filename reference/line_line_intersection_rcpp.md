# Rcpp version of line_line_intersection. Computes the between several segments. Is a vectorized function with minimal loss of time when the number of segments to test increases.

Rcpp version of line_line_intersection. Computes the between several
segments. Is a vectorized function with minimal loss of time when the
number of segments to test increases.

## Usage

``` r
line_line_intersection_rcpp(segments_1, segments_2)
```

## Arguments

- segments_1:

  Matrix with four columns denoting the x- and y-coordinates that make
  up the line segment. Should be in order x_1, y_1, x_2, y_2.

- segments_2:

  Matrix of line segments that \`segments_1\` should be tested with.
  Should have the same structure as \`segments_1\`

## Value

Returns a logical denoting whether any of the segments in
