# Compute the min-log-likelihood

Rcpp alternative to
[`mll`](https://github.com/ndpvh/predped/reference/mll.md). Be wary:
This version does not automatically add the motion variables to the data
if not present in the data, nor does it do any of the other
preprocessing. It is therefore better not used as an alternative to the
R version, but rather as an extension of it (as done automatically in
predped).

## Usage

``` r
mll_rcpp(data, parameters, ids, idx, cells, sizes, summed)
```

## Arguments

- data:

  List containing data.frames to use in the estimation procedure.

- parameters:

  List containing the parameters to be used. Should be specified in the
  same order as specified in `"parameter_names"`.

- ids:

  CharacterVector containing the names of the participants in the data
  set.

- idx:

  IntegerVector containing the index of the parameters to use to
  evaluate a given row in the data. Note that this index uses C++
  convention. Order should conform to the order in the list of the data.

- cells:

  IntegerVector denoting the cell to which a participant has moved at a
  given iteration. Order should conform to the order in the list of the
  data.

- sizes:

  IntegerVector containing the number of data points per person. Ignored
  if `summed` is `TRUE`.

- summed:

  Boolean denoting whether to sum the min-log-likelihood to one value
  per person. If `TRUE`, you get the resulting summed min-log-likelihood
  for each individual with a correction to avoid `-Inf`s. If `FALSE`,
  the function will instead return a list of vectors containing the raw
  likelihoods (not min-log-likelihoods!), allowing users to specify
  their own corrections (if needed).

## Value

Either named vector containing the summed min-log-likelihood
(`summed = TRUE`) or named list with vectors of raw likelihoods
(`summed = FALSE`) per person in the dataset.
