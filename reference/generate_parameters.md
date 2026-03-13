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
#> 1 0.2476285    1.1082649        1.281888 0.10444394    10031.062  9.491371
#> 2 0.2386299    1.0243900        1.229275 0.11267521    10340.805 10.480950
#> 3 0.2370365    0.9893258        1.189598 0.07260163    10280.482  9.960191
#> 4 0.2585052    0.9661647        1.355738 0.11972814     9744.742 10.478425
#> 5 0.2485657    0.9122877        1.195922 0.09370872    10104.520 10.878481
#>   b_turning a_turning b_current_direction a_current_direction
#> 1 0.1950871         2           0.9496390                   2
#> 2 0.2421611         2           0.9580037                   2
#> 3 0.1652041         2           1.1813542                   2
#> 4 0.1982872         2           1.1537426                   2
#> 5 0.2026871         2           0.9678876                   2
#>   blr_current_direction b_goal_direction a_goal_direction b_blocked a_blocked
#> 1                    10        11.705623                2  4.141124         2
#> 2                    10         8.420216                2  5.051235         2
#> 3                    10        10.210274                2  5.389299         2
#> 4                    10        10.629605                2  3.546915         2
#> 5                    10         9.793571                2  4.251361         2
#>   b_interpersonal a_interpersonal d_interpersonal b_preferred_speed
#> 1        1.994146               2               0         1.9845856
#> 2        1.665658               2               0         0.8855753
#> 3        1.817663               2               0         1.6584604
#> 4        2.335323               2               0         2.0701238
#> 5        1.632801               2               0         2.1780386
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
#>   constant_speed deceleration
#> 1              0            0
#> 2              0            0
#> 3              0            0
#> 4              0            0
#> 5              0            0

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
```
