# Compute the min-log-likelihood

Use data to compute the min-log-likelihood of choosing a given observed
cell given a set of parameters. Should allow the user to estimate the
parameters of the model.

## Usage

``` r
mll(
  data,
  parameters,
  parameter_names = colnames(params_from_csv[["params_archetypes"]])[-c(1, 2)],
  transform = TRUE,
  bounds = params_from_csv[["params_bounds"]],
  cpp = TRUE,
  summed = FALSE,
  ...
)
```

## Arguments

- data:

  Data.frame containing at least "id", "time", "x", "y", "goal_x",
  "goal_y", and "goal_id". If it does not have the utility variables
  yet, these will add them to the data.frame.

- parameters:

  Numeric vector or matrix containing the parameters to be used. Should
  be specified in the same order as specified in `"parameter_names"`. If
  a matrix, each row should contain parameters to be estimated for each
  instance of "id" separately.

- parameter_names:

  Character vector containing the parameters that you want to estimate.
  Defaults to all parameters defined in
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md).
  Whenever not all parameters are used, the excluded parameters are
  assumed to have a value of 0.

- transform:

  Logical denoting whether to transform the provided parameters from the
  real axis to the bounded scales imposed on the parameters within
  `predped`. Defaults to `TRUE`.

- bounds:

  Matrix containing the lower and upper bounds of the parameters in its
  first and second column respectively. Additionally, rownames should
  denote for which parameter a certain pair represents the bounds. Only
  used when `transform = TRUE`. Defaults to the default bounds of
  `predped`.

- cpp:

  Logical denoting whether to use the
  [`mll_rcpp`](https://github.com/ndpvh/predped/reference/mll_rcpp.md)
  function to compute the min-log-likelihood. Defaults to `TRUE`.

- summed:

  Logical denoting whether to sum the min-log-likelihood to one value
  per person. If `TRUE`, you get the resulting summed min-log-likelihood
  for each individual with a correction to avoid `-Inf`s. If `FALSE`,
  the function will instead return a list of vectors containing the raw
  likelihoods (not min-log-likelihoods!), allowing users to specify
  their own corrections (if needed). Defaults to `FALSE`.

- ...:

  Additional arguments passed on to
  [`add_motion_variables`](https://github.com/ndpvh/predped/reference/add_motion_variables.md).
  In a typical estimation situation, these motion variables should
  already be in `data`.

  @return Either named vector containing the summed min-log-likelihood
  (`summed = TRUE`) or named list with vectors of raw likelihoods
  (`summed = FALSE`) per person in the dataset.
