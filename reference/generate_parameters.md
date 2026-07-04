# Generate parameters

Use the mean parameter values and their standard deviations to generate
parameters to be used in simulation.

## Usage

``` r
generate_parameters(
  n = 1,
  filename = NULL,
  sep = ",",
  archetype = "BaselineEuropean",
  mean = NULL,
  Sigma = NULL,
  bounds = NULL,
  individual_differences = TRUE,
  transform_covariance = TRUE
)
```

## Arguments

- n:

  Integer denoting the number of parameters to generate. Defaults to
  `1`.

- filename:

  Character denoting the path to a file containing parameters. Defaults
  to `NULL`, triggering reading in the csv-files that come with predped.

- sep:

  Character denoting the separator in case `x` is a delimited file.
  Defaults to `","`.

- archetype:

  String denoting the archetype to be used for the covariance matrix.
  Ignored if `Sigma` is provided. Defaults to `"BaselineEuropean"`.

- mean:

  Dataframe containing the means for each of the parameters for a given
  agent. Defaults to `NULL`, triggering reading in the data instead.

- Sigma:

  Either a covariance matrix that defines the individual differences on
  each of the parameters (when `transform_covariance = FALSE`), or a
  matrix containing standard deviations for each of the parameters on
  its diagonal and correlations between the parameters on its
  off-diagonal (when `transform_covariance = TRUE`; see
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)).
  Default covariance matrices exist for each of the archetypes in
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)
  and thus changes with the value of `archetype`. Defaults to `NULL`,
  triggering reading in the data (but only if
  `individual_differences = TRUE`).

- bounds:

  Named numeric matrix containing the bounds for each of the parameters.
  Usually provided in the parameter-list under name `"params_bounds"`.
  Defaults to `NULL`, triggering reading in the data (but only if
  `individual_differences = TRUE`).

- individual_differences:

  Logical denoting whether to use the standard deviations in the
  parameter list to create some variation in the parameters. Defaults to
  `TRUE`.

- transform_covariance:

  Logical denoting whether to transform `Sigma` to a proper covariance
  matrix or not. Defaults to `TRUE`.

## Value

Data.frame containing the generated parameter values.

## Details

When `individual_differences = FALSE`, generating `n` parameters for a
given `archetype` comes down to repeating the same parameters under
`params_archetypes` `n` times.

When `individual_differences = TRUE`, we use the following steps to
generate the data. First, we transform the mean values under the name
`"params_archetypes"` in the parameter list or provided through the
`mean` argument to a value between 0 and 1 by using the bounds under
`"params_bounds"` in the parameter list or through those provided
through the `bounds` argument. Then, we transform this bounded value to
an unbounded value drawn from a normal distribution through the `qnorm`
function. This transformation is done through the
[`to_unbounded`](https://github.com/ndpvh/predped/reference/to_unbounded.md)
function.

Once we have unbounded values for the parameters, we transform the
provided matrix under `"params_sigma"` in the parameter list or provided
through the `Sigma` argument. Note that this transformation is only done
if `transform_covariance = TRUE`. This transformation is done through
the
[`to_covariance`](https://github.com/ndpvh/predped/reference/to_covariance.md).

Having both the unbounded means and the covariance matrix for the
parameters now allows us to draw random values for these parameters
through a multivariate normal distribution. For this, we rely on the
`MASS` package. Once the parameters have been sampled, we transform them
back to their original bounds through the
[`to_bounded`](https://github.com/ndpvh/predped/reference/to_bounded.md)
function.

## See also

[`load_parameters`](https://github.com/ndpvh/predped/reference/load_parameters.md),
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md),
[`plot_distribution`](https://github.com/ndpvh/predped/reference/plot_distribution.md),
[`to_bounded`](https://github.com/ndpvh/predped/reference/to_bounded.md),
[`to_covariance`](https://github.com/ndpvh/predped/reference/to_covariance.md),
[`to_unbounded`](https://github.com/ndpvh/predped/reference/to_unbounded.md)

## Examples

``` r
# Generate multiple instances of the BaselineEuropean
parameters <- generate_parameters(5)
head(parameters)
#>      radius slowing_time preferred_speed randomness stop_utility   reroute
#> 1 0.2441243    1.1032897        1.264805 0.09766157    10423.610 10.727450
#> 2 0.2517194    0.9757366        1.290145 0.04524117     9712.305  8.196912
#> 3 0.2484244    1.0046285        1.290394 0.08275572     9993.118  8.582732
#> 4 0.2558896    0.9759652        1.263390 0.11591617     9603.723 10.625717
#> 5 0.2517657    0.9958707        1.207432 0.07279814     9918.527 10.469219
#>   b_turning a_turning b_current_direction a_current_direction
#> 1 0.2047671         2           1.1849026                   2
#> 2 0.2048117         2           1.3082050                   2
#> 3 0.2203179         2           0.8907058                   2
#> 4 0.1948534         2           1.3232414                   2
#> 5 0.2158429         2           1.0950839                   2
#>   blr_current_direction b_goal_direction a_goal_direction b_blocked a_blocked
#> 1                    10         8.423245                2  4.008381         2
#> 2                    10        11.976694                2  5.287782         2
#> 3                    10         9.848939                2  4.226951         2
#> 4                    10         7.553959                2  4.344917         2
#> 5                    10        11.981158                2  3.334899         2
#>   b_interpersonal a_interpersonal d_interpersonal b_preferred_speed
#> 1        1.564919               2               0          2.228301
#> 2        2.614970               2               0          1.333030
#> 3        2.977035               2               0          1.758503
#> 4        2.553351               2               0          2.513710
#> 5        1.926506               2               0          1.740262
#>   a_preferred_speed b_leader a_leader d_leader b_buddy a_buddy a_group_centroid
#> 1                 2        0        0        0       0       0                0
#> 2                 2        0        0        0       0       0                0
#> 3                 2        0        0        0       0       0                0
#> 4                 2        0        0        0       0       0                0
#> 5                 2        0        0        0       0       0                0
#>   b_group_centroid b_visual_field central non_central acceleration
#> 1                0              0       0           0            0
#> 2                0              0       0           0            0
#> 3                0              0       0           0            0
#> 4                0              0       0           0            0
#> 5                0              0       0           0            0
#>   constant_speed deceleration a_lgvf b_lgvf e_lgvf
#> 1              0            0      0      0      0
#> 2              0            0      0      0      0
#> 3              0            0      0      0      0
#> 4              0            0      0      0      0
#> 5              0            0      0      0      0

# Note that you can turn individual differences off
parameters <- generate_parameters(5, individual_differences = FALSE)
head(parameters)
#>   radius slowing_time preferred_speed randomness stop_utility reroute b_turning
#> 1   0.25            1            1.25        0.1        10000      10       0.2
#>   a_turning b_current_direction a_current_direction blr_current_direction
#> 1         2                   1                   2                    10
#>   b_goal_direction a_goal_direction b_blocked a_blocked b_interpersonal
#> 1               10                2         4         2               2
#>   a_interpersonal d_interpersonal b_preferred_speed a_preferred_speed b_leader
#> 1               2               0                 2                 2        0
#>   a_leader d_leader b_buddy a_buddy a_group_centroid b_group_centroid
#> 1        0        0       0       0                0                0
#>   b_visual_field central non_central acceleration constant_speed deceleration
#> 1              0       0           0            0              0            0
#>   a_lgvf b_lgvf e_lgvf
#> 1      0      0      0
```
