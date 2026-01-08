# Constructor for the [`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md)

Constructor for the
[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md)

## Usage

``` r
# S4 method for class 'predped'
initialize(
  .Object,
  setting,
  id = character(0),
  filename = NULL,
  sep = ",",
  archetypes = NULL,
  weights = NULL
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- setting:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- id:

  Character that serves as an identifier for the agent. Defaults to an
  empty character, triggering the random generation of an id.

- filename:

  Character denoting the path to personalized parameters. Defaults to
  `NULL`, triggering reading in the csv-files.

- sep:

  Character denoting the separator of the delimited file in `filename`.
  If `filename` is not provided or if `filename` is not a delimited
  file, this argument is ignored. Defaults to `","`.

- archetypes:

  Character or character vector denoting the archetype(s) you want to
  include in the simulation. Defaults to `NULL`, triggering the
  inclusion of all currently defined archetypes.

- weights:

  Numeric vector containing the probability with which an agent of each
  archetype can be selected to wander around in the environment. The
  weights should be in the same length as the `archetypes` argument.
  Defaults to an equal probability for each of the archetypes.

## Value

Object of the
[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md)

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md),
[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md),
[`load_parameters`](https://github.com/ndpvh/predped/reference/load_parameters.md),
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)

## Examples

``` r
# Initialize predped model
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list())

my_model <- predped(setting = my_background, 
                    archetypes = c("BaselineEuropean", 
                                   "DrunkAussie"), 
                    weights = c(0.9, 0.1))

# Access the two slots that were specified
my_model@id
#> [1] "model hzgsd"
head(my_model@parameters)
#> $params_archetypes
#>               name          color radius slowing_time preferred_speed
#> 1 BaselineEuropean cornflowerblue   0.25            1            1.25
#> 3      DrunkAussie      goldenrod   0.25            1            1.00
#>   randomness stop_utility reroute b_turning a_turning b_current_direction
#> 1        0.1        10000      10       0.2         2                   1
#> 3        5.0         1000      20       0.2         2                   1
#>   a_current_direction blr_current_direction b_goal_direction a_goal_direction
#> 1                   2                  10.0               10                2
#> 3                   2                   0.1               10                2
#>   b_blocked a_blocked b_interpersonal a_interpersonal d_interpersonal
#> 1         4         2               2               2               0
#> 3         4         2               1               2               0
#>   b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> 1                 2                 2        0        0        0       0
#> 3                 1                 2        1        2        0       0
#>   a_buddy a_group_centroid b_group_centroid b_visual_field central non_central
#> 1       0                0                0              0       0           0
#> 3       0                0                0              0       0           0
#>   acceleration constant_speed deceleration
#> 1            0              0            0
#> 3            0              0            0
#> 
#> $params_sigma
#> $params_sigma$BaselineEuropean
#>                       radius slowing_time preferred_speed randomness
#> radius                  0.15          0.0            0.00        0.0
#> slowing_time            0.00          0.1            0.00        0.0
#> preferred_speed         0.00          0.0            0.05        0.0
#> randomness              0.00          0.0            0.00        0.1
#> stop_utility            0.00          0.0            0.00        0.0
#> reroute                 0.00          0.0            0.00        0.0
#> b_turning               0.00          0.0            0.00        0.0
#> a_turning               0.00          0.0            0.00        0.0
#> b_current_direction     0.00          0.0            0.00        0.0
#> a_current_direction     0.00          0.0            0.00        0.0
#> blr_current_direction   0.00          0.0            0.00        0.0
#> b_goal_direction        0.00          0.0            0.00        0.0
#> a_goal_direction        0.00          0.0            0.00        0.0
#> b_blocked               0.00          0.0            0.00        0.0
#> a_blocked               0.00          0.0            0.00        0.0
#> b_interpersonal         0.00          0.0            0.00        0.0
#> a_interpersonal         0.00          0.0            0.00        0.0
#> d_interpersonal         0.00          0.0            0.00        0.0
#> b_preferred_speed       0.00          0.0            0.00        0.0
#> a_preferred_speed       0.00          0.0            0.00        0.0
#> b_leader                0.00          0.0            0.00        0.0
#> a_leader                0.00          0.0            0.00        0.0
#> d_leader                0.00          0.0            0.00        0.0
#> b_buddy                 0.00          0.0            0.00        0.0
#> a_buddy                 0.00          0.0            0.00        0.0
#> a_group_centroid        0.00          0.0            0.00        0.0
#> b_group_centroid        0.00          0.0            0.00        0.0
#> b_visual_field          0.00          0.0            0.00        0.0
#> central                 0.00          0.0            0.00        0.0
#> non_central             0.00          0.0            0.00        0.0
#> acceleration            0.00          0.0            0.00        0.0
#> constant_speed          0.00          0.0            0.00        0.0
#> deceleration            0.00          0.0            0.00        0.0
#>                       stop_utility reroute b_turning a_turning
#> radius                        0.00     0.0       0.0         0
#> slowing_time                  0.00     0.0       0.0         0
#> preferred_speed               0.00     0.0       0.0         0
#> randomness                    0.00     0.0       0.0         0
#> stop_utility                  0.01     0.0       0.0         0
#> reroute                       0.00     0.1       0.0         0
#> b_turning                     0.00     0.0       0.1         0
#> a_turning                     0.00     0.0       0.0         0
#> b_current_direction           0.00     0.0       0.0         0
#> a_current_direction           0.00     0.0       0.0         0
#> blr_current_direction         0.00     0.0       0.0         0
#> b_goal_direction              0.00     0.0       0.0         0
#> a_goal_direction              0.00     0.0       0.0         0
#> b_blocked                     0.00     0.0       0.0         0
#> a_blocked                     0.00     0.0       0.0         0
#> b_interpersonal               0.00     0.0       0.0         0
#> a_interpersonal               0.00     0.0       0.0         0
#> d_interpersonal               0.00     0.0       0.0         0
#> b_preferred_speed             0.00     0.0       0.0         0
#> a_preferred_speed             0.00     0.0       0.0         0
#> b_leader                      0.00     0.0       0.0         0
#> a_leader                      0.00     0.0       0.0         0
#> d_leader                      0.00     0.0       0.0         0
#> b_buddy                       0.00     0.0       0.0         0
#> a_buddy                       0.00     0.0       0.0         0
#> a_group_centroid              0.00     0.0       0.0         0
#> b_group_centroid              0.00     0.0       0.0         0
#> b_visual_field                0.00     0.0       0.0         0
#> central                       0.00     0.0       0.0         0
#> non_central                   0.00     0.0       0.0         0
#> acceleration                  0.00     0.0       0.0         0
#> constant_speed                0.00     0.0       0.0         0
#> deceleration                  0.00     0.0       0.0         0
#>                       b_current_direction a_current_direction
#> radius                                0.0                   0
#> slowing_time                          0.0                   0
#> preferred_speed                       0.0                   0
#> randomness                            0.0                   0
#> stop_utility                          0.0                   0
#> reroute                               0.0                   0
#> b_turning                             0.0                   0
#> a_turning                             0.0                   0
#> b_current_direction                   0.1                   0
#> a_current_direction                   0.0                   0
#> blr_current_direction                 0.0                   0
#> b_goal_direction                      0.0                   0
#> a_goal_direction                      0.0                   0
#> b_blocked                             0.0                   0
#> a_blocked                             0.0                   0
#> b_interpersonal                       0.0                   0
#> a_interpersonal                       0.0                   0
#> d_interpersonal                       0.0                   0
#> b_preferred_speed                     0.0                   0
#> a_preferred_speed                     0.0                   0
#> b_leader                              0.0                   0
#> a_leader                              0.0                   0
#> d_leader                              0.0                   0
#> b_buddy                               0.0                   0
#> a_buddy                               0.0                   0
#> a_group_centroid                      0.0                   0
#> b_group_centroid                      0.0                   0
#> b_visual_field                        0.0                   0
#> central                               0.0                   0
#> non_central                           0.0                   0
#> acceleration                          0.0                   0
#> constant_speed                        0.0                   0
#> deceleration                          0.0                   0
#>                       blr_current_direction b_goal_direction a_goal_direction
#> radius                                    0             0.00                0
#> slowing_time                              0             0.00                0
#> preferred_speed                           0             0.00                0
#> randomness                                0             0.00                0
#> stop_utility                              0             0.00                0
#> reroute                                   0             0.00                0
#> b_turning                                 0             0.00                0
#> a_turning                                 0             0.00                0
#> b_current_direction                       0             0.00                0
#> a_current_direction                       0             0.00                0
#> blr_current_direction                     0             0.00                0
#> b_goal_direction                          0             0.15                0
#> a_goal_direction                          0             0.00                0
#> b_blocked                                 0             0.00                0
#> a_blocked                                 0             0.00                0
#> b_interpersonal                           0             0.00                0
#> a_interpersonal                           0             0.00                0
#> d_interpersonal                           0             0.00                0
#> b_preferred_speed                         0             0.00                0
#> a_preferred_speed                         0             0.00                0
#> b_leader                                  0             0.00                0
#> a_leader                                  0             0.00                0
#> d_leader                                  0             0.00                0
#> b_buddy                                   0             0.00                0
#> a_buddy                                   0             0.00                0
#> a_group_centroid                          0             0.00                0
#> b_group_centroid                          0             0.00                0
#> b_visual_field                            0             0.00                0
#> central                                   0             0.00                0
#> non_central                               0             0.00                0
#> acceleration                              0             0.00                0
#> constant_speed                            0             0.00                0
#> deceleration                              0             0.00                0
#>                       b_blocked a_blocked b_interpersonal a_interpersonal
#> radius                     0.00         0            0.00               0
#> slowing_time               0.00         0            0.00               0
#> preferred_speed            0.00         0            0.00               0
#> randomness                 0.00         0            0.00               0
#> stop_utility               0.00         0            0.00               0
#> reroute                    0.00         0            0.00               0
#> b_turning                  0.00         0            0.00               0
#> a_turning                  0.00         0            0.00               0
#> b_current_direction        0.00         0            0.00               0
#> a_current_direction        0.00         0            0.00               0
#> blr_current_direction      0.00         0            0.00               0
#> b_goal_direction           0.00         0            0.00               0
#> a_goal_direction           0.00         0            0.00               0
#> b_blocked                  0.15         0            0.00               0
#> a_blocked                  0.00         0            0.00               0
#> b_interpersonal            0.00         0            0.15               0
#> a_interpersonal            0.00         0            0.00               0
#> d_interpersonal            0.00         0            0.00               0
#> b_preferred_speed          0.00         0            0.00               0
#> a_preferred_speed          0.00         0            0.00               0
#> b_leader                   0.00         0            0.00               0
#> a_leader                   0.00         0            0.00               0
#> d_leader                   0.00         0            0.00               0
#> b_buddy                    0.00         0            0.00               0
#> a_buddy                    0.00         0            0.00               0
#> a_group_centroid           0.00         0            0.00               0
#> b_group_centroid           0.00         0            0.00               0
#> b_visual_field             0.00         0            0.00               0
#> central                    0.00         0            0.00               0
#> non_central                0.00         0            0.00               0
#> acceleration               0.00         0            0.00               0
#> constant_speed             0.00         0            0.00               0
#> deceleration               0.00         0            0.00               0
#>                       d_interpersonal b_preferred_speed a_preferred_speed
#> radius                              0              0.00                 0
#> slowing_time                        0              0.00                 0
#> preferred_speed                     0              0.00                 0
#> randomness                          0              0.00                 0
#> stop_utility                        0              0.00                 0
#> reroute                             0              0.00                 0
#> b_turning                           0              0.00                 0
#> a_turning                           0              0.00                 0
#> b_current_direction                 0              0.00                 0
#> a_current_direction                 0              0.00                 0
#> blr_current_direction               0              0.00                 0
#> b_goal_direction                    0              0.00                 0
#> a_goal_direction                    0              0.00                 0
#> b_blocked                           0              0.00                 0
#> a_blocked                           0              0.00                 0
#> b_interpersonal                     0              0.00                 0
#> a_interpersonal                     0              0.00                 0
#> d_interpersonal                     0              0.00                 0
#> b_preferred_speed                   0              0.15                 0
#> a_preferred_speed                   0              0.00                 0
#> b_leader                            0              0.00                 0
#> a_leader                            0              0.00                 0
#> d_leader                            0              0.00                 0
#> b_buddy                             0              0.00                 0
#> a_buddy                             0              0.00                 0
#> a_group_centroid                    0              0.00                 0
#> b_group_centroid                    0              0.00                 0
#> b_visual_field                      0              0.00                 0
#> central                             0              0.00                 0
#> non_central                         0              0.00                 0
#> acceleration                        0              0.00                 0
#> constant_speed                      0              0.00                 0
#> deceleration                        0              0.00                 0
#>                       b_leader a_leader d_leader b_buddy a_buddy
#> radius                       0        0        0       0       0
#> slowing_time                 0        0        0       0       0
#> preferred_speed              0        0        0       0       0
#> randomness                   0        0        0       0       0
#> stop_utility                 0        0        0       0       0
#> reroute                      0        0        0       0       0
#> b_turning                    0        0        0       0       0
#> a_turning                    0        0        0       0       0
#> b_current_direction          0        0        0       0       0
#> a_current_direction          0        0        0       0       0
#> blr_current_direction        0        0        0       0       0
#> b_goal_direction             0        0        0       0       0
#> a_goal_direction             0        0        0       0       0
#> b_blocked                    0        0        0       0       0
#> a_blocked                    0        0        0       0       0
#> b_interpersonal              0        0        0       0       0
#> a_interpersonal              0        0        0       0       0
#> d_interpersonal              0        0        0       0       0
#> b_preferred_speed            0        0        0       0       0
#> a_preferred_speed            0        0        0       0       0
#> b_leader                     0        0        0       0       0
#> a_leader                     0        0        0       0       0
#> d_leader                     0        0        0       0       0
#> b_buddy                      0        0        0       0       0
#> a_buddy                      0        0        0       0       0
#> a_group_centroid             0        0        0       0       0
#> b_group_centroid             0        0        0       0       0
#> b_visual_field               0        0        0       0       0
#> central                      0        0        0       0       0
#> non_central                  0        0        0       0       0
#> acceleration                 0        0        0       0       0
#> constant_speed               0        0        0       0       0
#> deceleration                 0        0        0       0       0
#>                       a_group_centroid b_group_centroid b_visual_field central
#> radius                               0                0              0       0
#> slowing_time                         0                0              0       0
#> preferred_speed                      0                0              0       0
#> randomness                           0                0              0       0
#> stop_utility                         0                0              0       0
#> reroute                              0                0              0       0
#> b_turning                            0                0              0       0
#> a_turning                            0                0              0       0
#> b_current_direction                  0                0              0       0
#> a_current_direction                  0                0              0       0
#> blr_current_direction                0                0              0       0
#> b_goal_direction                     0                0              0       0
#> a_goal_direction                     0                0              0       0
#> b_blocked                            0                0              0       0
#> a_blocked                            0                0              0       0
#> b_interpersonal                      0                0              0       0
#> a_interpersonal                      0                0              0       0
#> d_interpersonal                      0                0              0       0
#> b_preferred_speed                    0                0              0       0
#> a_preferred_speed                    0                0              0       0
#> b_leader                             0                0              0       0
#> a_leader                             0                0              0       0
#> d_leader                             0                0              0       0
#> b_buddy                              0                0              0       0
#> a_buddy                              0                0              0       0
#> a_group_centroid                     0                0              0       0
#> b_group_centroid                     0                0              0       0
#> b_visual_field                       0                0              0       0
#> central                              0                0              0       0
#> non_central                          0                0              0       0
#> acceleration                         0                0              0       0
#> constant_speed                       0                0              0       0
#> deceleration                         0                0              0       0
#>                       non_central acceleration constant_speed deceleration
#> radius                          0            0              0            0
#> slowing_time                    0            0              0            0
#> preferred_speed                 0            0              0            0
#> randomness                      0            0              0            0
#> stop_utility                    0            0              0            0
#> reroute                         0            0              0            0
#> b_turning                       0            0              0            0
#> a_turning                       0            0              0            0
#> b_current_direction             0            0              0            0
#> a_current_direction             0            0              0            0
#> blr_current_direction           0            0              0            0
#> b_goal_direction                0            0              0            0
#> a_goal_direction                0            0              0            0
#> b_blocked                       0            0              0            0
#> a_blocked                       0            0              0            0
#> b_interpersonal                 0            0              0            0
#> a_interpersonal                 0            0              0            0
#> d_interpersonal                 0            0              0            0
#> b_preferred_speed               0            0              0            0
#> a_preferred_speed               0            0              0            0
#> b_leader                        0            0              0            0
#> a_leader                        0            0              0            0
#> d_leader                        0            0              0            0
#> b_buddy                         0            0              0            0
#> a_buddy                         0            0              0            0
#> a_group_centroid                0            0              0            0
#> b_group_centroid                0            0              0            0
#> b_visual_field                  0            0              0            0
#> central                         0            0              0            0
#> non_central                     0            0              0            0
#> acceleration                    0            0              0            0
#> constant_speed                  0            0              0            0
#> deceleration                    0            0              0            0
#> 
#> $params_sigma$DrunkAussie
#>                       radius slowing_time preferred_speed randomness
#> radius                  0.15          0.0            0.00        0.0
#> slowing_time            0.00          0.1            0.00        0.0
#> preferred_speed         0.00          0.0            0.05        0.0
#> randomness              0.00          0.0            0.00        0.1
#> stop_utility            0.00          0.0            0.00        0.0
#> reroute                 0.00          0.0            0.00        0.0
#> b_turning               0.00          0.0            0.00        0.0
#> a_turning               0.00          0.0            0.00        0.0
#> b_current_direction     0.00          0.0            0.00        0.0
#> a_current_direction     0.00          0.0            0.00        0.0
#> blr_current_direction   0.00          0.0            0.00        0.0
#> b_goal_direction        0.00          0.0            0.00        0.0
#> a_goal_direction        0.00          0.0            0.00        0.0
#> b_blocked               0.00          0.0            0.00        0.0
#> a_blocked               0.00          0.0            0.00        0.0
#> b_interpersonal         0.00          0.0            0.00        0.0
#> a_interpersonal         0.00          0.0            0.00        0.0
#> d_interpersonal         0.00          0.0            0.00        0.0
#> b_preferred_speed       0.00          0.0            0.00        0.0
#> a_preferred_speed       0.00          0.0            0.00        0.0
#> b_leader                0.00          0.0            0.00        0.0
#> a_leader                0.00          0.0            0.00        0.0
#> d_leader                0.00          0.0            0.00        0.0
#> b_buddy                 0.00          0.0            0.00        0.0
#> a_buddy                 0.00          0.0            0.00        0.0
#> a_group_centroid        0.00          0.0            0.00        0.0
#> b_group_centroid        0.00          0.0            0.00        0.0
#> b_visual_field          0.00          0.0            0.00        0.0
#> central                 0.00          0.0            0.00        0.0
#> non_central             0.00          0.0            0.00        0.0
#> acceleration            0.00          0.0            0.00        0.0
#> constant_speed          0.00          0.0            0.00        0.0
#> deceleration            0.00          0.0            0.00        0.0
#>                       stop_utility reroute b_turning a_turning
#> radius                        0.00     0.0       0.0         0
#> slowing_time                  0.00     0.0       0.0         0
#> preferred_speed               0.00     0.0       0.0         0
#> randomness                    0.00     0.0       0.0         0
#> stop_utility                  0.01     0.0       0.0         0
#> reroute                       0.00     0.1       0.0         0
#> b_turning                     0.00     0.0       0.1         0
#> a_turning                     0.00     0.0       0.0         0
#> b_current_direction           0.00     0.0       0.0         0
#> a_current_direction           0.00     0.0       0.0         0
#> blr_current_direction         0.00     0.0       0.0         0
#> b_goal_direction              0.00     0.0       0.0         0
#> a_goal_direction              0.00     0.0       0.0         0
#> b_blocked                     0.00     0.0       0.0         0
#> a_blocked                     0.00     0.0       0.0         0
#> b_interpersonal               0.00     0.0       0.0         0
#> a_interpersonal               0.00     0.0       0.0         0
#> d_interpersonal               0.00     0.0       0.0         0
#> b_preferred_speed             0.00     0.0       0.0         0
#> a_preferred_speed             0.00     0.0       0.0         0
#> b_leader                      0.00     0.0       0.0         0
#> a_leader                      0.00     0.0       0.0         0
#> d_leader                      0.00     0.0       0.0         0
#> b_buddy                       0.00     0.0       0.0         0
#> a_buddy                       0.00     0.0       0.0         0
#> a_group_centroid              0.00     0.0       0.0         0
#> b_group_centroid              0.00     0.0       0.0         0
#> b_visual_field                0.00     0.0       0.0         0
#> central                       0.00     0.0       0.0         0
#> non_central                   0.00     0.0       0.0         0
#> acceleration                  0.00     0.0       0.0         0
#> constant_speed                0.00     0.0       0.0         0
#> deceleration                  0.00     0.0       0.0         0
#>                       b_current_direction a_current_direction
#> radius                                0.0                   0
#> slowing_time                          0.0                   0
#> preferred_speed                       0.0                   0
#> randomness                            0.0                   0
#> stop_utility                          0.0                   0
#> reroute                               0.0                   0
#> b_turning                             0.0                   0
#> a_turning                             0.0                   0
#> b_current_direction                   0.1                   0
#> a_current_direction                   0.0                   0
#> blr_current_direction                 0.0                   0
#> b_goal_direction                      0.0                   0
#> a_goal_direction                      0.0                   0
#> b_blocked                             0.0                   0
#> a_blocked                             0.0                   0
#> b_interpersonal                       0.0                   0
#> a_interpersonal                       0.0                   0
#> d_interpersonal                       0.0                   0
#> b_preferred_speed                     0.0                   0
#> a_preferred_speed                     0.0                   0
#> b_leader                              0.0                   0
#> a_leader                              0.0                   0
#> d_leader                              0.0                   0
#> b_buddy                               0.0                   0
#> a_buddy                               0.0                   0
#> a_group_centroid                      0.0                   0
#> b_group_centroid                      0.0                   0
#> b_visual_field                        0.0                   0
#> central                               0.0                   0
#> non_central                           0.0                   0
#> acceleration                          0.0                   0
#> constant_speed                        0.0                   0
#> deceleration                          0.0                   0
#>                       blr_current_direction b_goal_direction a_goal_direction
#> radius                                    0             0.00                0
#> slowing_time                              0             0.00                0
#> preferred_speed                           0             0.00                0
#> randomness                                0             0.00                0
#> stop_utility                              0             0.00                0
#> reroute                                   0             0.00                0
#> b_turning                                 0             0.00                0
#> a_turning                                 0             0.00                0
#> b_current_direction                       0             0.00                0
#> a_current_direction                       0             0.00                0
#> blr_current_direction                     0             0.00                0
#> b_goal_direction                          0             0.15                0
#> a_goal_direction                          0             0.00                0
#> b_blocked                                 0             0.00                0
#> a_blocked                                 0             0.00                0
#> b_interpersonal                           0             0.00                0
#> a_interpersonal                           0             0.00                0
#> d_interpersonal                           0             0.00                0
#> b_preferred_speed                         0             0.00                0
#> a_preferred_speed                         0             0.00                0
#> b_leader                                  0             0.00                0
#> a_leader                                  0             0.00                0
#> d_leader                                  0             0.00                0
#> b_buddy                                   0             0.00                0
#> a_buddy                                   0             0.00                0
#> a_group_centroid                          0             0.00                0
#> b_group_centroid                          0             0.00                0
#> b_visual_field                            0             0.00                0
#> central                                   0             0.00                0
#> non_central                               0             0.00                0
#> acceleration                              0             0.00                0
#> constant_speed                            0             0.00                0
#> deceleration                              0             0.00                0
#>                       b_blocked a_blocked b_interpersonal a_interpersonal
#> radius                     0.00         0            0.00               0
#> slowing_time               0.00         0            0.00               0
#> preferred_speed            0.00         0            0.00               0
#> randomness                 0.00         0            0.00               0
#> stop_utility               0.00         0            0.00               0
#> reroute                    0.00         0            0.00               0
#> b_turning                  0.00         0            0.00               0
#> a_turning                  0.00         0            0.00               0
#> b_current_direction        0.00         0            0.00               0
#> a_current_direction        0.00         0            0.00               0
#> blr_current_direction      0.00         0            0.00               0
#> b_goal_direction           0.00         0            0.00               0
#> a_goal_direction           0.00         0            0.00               0
#> b_blocked                  0.15         0            0.00               0
#> a_blocked                  0.00         0            0.00               0
#> b_interpersonal            0.00         0            0.15               0
#> a_interpersonal            0.00         0            0.00               0
#> d_interpersonal            0.00         0            0.00               0
#> b_preferred_speed          0.00         0            0.00               0
#> a_preferred_speed          0.00         0            0.00               0
#> b_leader                   0.00         0            0.00               0
#> a_leader                   0.00         0            0.00               0
#> d_leader                   0.00         0            0.00               0
#> b_buddy                    0.00         0            0.00               0
#> a_buddy                    0.00         0            0.00               0
#> a_group_centroid           0.00         0            0.00               0
#> b_group_centroid           0.00         0            0.00               0
#> b_visual_field             0.00         0            0.00               0
#> central                    0.00         0            0.00               0
#> non_central                0.00         0            0.00               0
#> acceleration               0.00         0            0.00               0
#> constant_speed             0.00         0            0.00               0
#> deceleration               0.00         0            0.00               0
#>                       d_interpersonal b_preferred_speed a_preferred_speed
#> radius                              0              0.00                 0
#> slowing_time                        0              0.00                 0
#> preferred_speed                     0              0.00                 0
#> randomness                          0              0.00                 0
#> stop_utility                        0              0.00                 0
#> reroute                             0              0.00                 0
#> b_turning                           0              0.00                 0
#> a_turning                           0              0.00                 0
#> b_current_direction                 0              0.00                 0
#> a_current_direction                 0              0.00                 0
#> blr_current_direction               0              0.00                 0
#> b_goal_direction                    0              0.00                 0
#> a_goal_direction                    0              0.00                 0
#> b_blocked                           0              0.00                 0
#> a_blocked                           0              0.00                 0
#> b_interpersonal                     0              0.00                 0
#> a_interpersonal                     0              0.00                 0
#> d_interpersonal                     0              0.00                 0
#> b_preferred_speed                   0              0.15                 0
#> a_preferred_speed                   0              0.00                 0
#> b_leader                            0              0.00                 0
#> a_leader                            0              0.00                 0
#> d_leader                            0              0.00                 0
#> b_buddy                             0              0.00                 0
#> a_buddy                             0              0.00                 0
#> a_group_centroid                    0              0.00                 0
#> b_group_centroid                    0              0.00                 0
#> b_visual_field                      0              0.00                 0
#> central                             0              0.00                 0
#> non_central                         0              0.00                 0
#> acceleration                        0              0.00                 0
#> constant_speed                      0              0.00                 0
#> deceleration                        0              0.00                 0
#>                       b_leader a_leader d_leader b_buddy a_buddy
#> radius                       0        0        0       0       0
#> slowing_time                 0        0        0       0       0
#> preferred_speed              0        0        0       0       0
#> randomness                   0        0        0       0       0
#> stop_utility                 0        0        0       0       0
#> reroute                      0        0        0       0       0
#> b_turning                    0        0        0       0       0
#> a_turning                    0        0        0       0       0
#> b_current_direction          0        0        0       0       0
#> a_current_direction          0        0        0       0       0
#> blr_current_direction        0        0        0       0       0
#> b_goal_direction             0        0        0       0       0
#> a_goal_direction             0        0        0       0       0
#> b_blocked                    0        0        0       0       0
#> a_blocked                    0        0        0       0       0
#> b_interpersonal              0        0        0       0       0
#> a_interpersonal              0        0        0       0       0
#> d_interpersonal              0        0        0       0       0
#> b_preferred_speed            0        0        0       0       0
#> a_preferred_speed            0        0        0       0       0
#> b_leader                     0        0        0       0       0
#> a_leader                     0        0        0       0       0
#> d_leader                     0        0        0       0       0
#> b_buddy                      0        0        0       0       0
#> a_buddy                      0        0        0       0       0
#> a_group_centroid             0        0        0       0       0
#> b_group_centroid             0        0        0       0       0
#> b_visual_field               0        0        0       0       0
#> central                      0        0        0       0       0
#> non_central                  0        0        0       0       0
#> acceleration                 0        0        0       0       0
#> constant_speed               0        0        0       0       0
#> deceleration                 0        0        0       0       0
#>                       a_group_centroid b_group_centroid b_visual_field central
#> radius                               0                0              0       0
#> slowing_time                         0                0              0       0
#> preferred_speed                      0                0              0       0
#> randomness                           0                0              0       0
#> stop_utility                         0                0              0       0
#> reroute                              0                0              0       0
#> b_turning                            0                0              0       0
#> a_turning                            0                0              0       0
#> b_current_direction                  0                0              0       0
#> a_current_direction                  0                0              0       0
#> blr_current_direction                0                0              0       0
#> b_goal_direction                     0                0              0       0
#> a_goal_direction                     0                0              0       0
#> b_blocked                            0                0              0       0
#> a_blocked                            0                0              0       0
#> b_interpersonal                      0                0              0       0
#> a_interpersonal                      0                0              0       0
#> d_interpersonal                      0                0              0       0
#> b_preferred_speed                    0                0              0       0
#> a_preferred_speed                    0                0              0       0
#> b_leader                             0                0              0       0
#> a_leader                             0                0              0       0
#> d_leader                             0                0              0       0
#> b_buddy                              0                0              0       0
#> a_buddy                              0                0              0       0
#> a_group_centroid                     0                0              0       0
#> b_group_centroid                     0                0              0       0
#> b_visual_field                       0                0              0       0
#> central                              0                0              0       0
#> non_central                          0                0              0       0
#> acceleration                         0                0              0       0
#> constant_speed                       0                0              0       0
#> deceleration                         0                0              0       0
#>                       non_central acceleration constant_speed deceleration
#> radius                          0            0              0            0
#> slowing_time                    0            0              0            0
#> preferred_speed                 0            0              0            0
#> randomness                      0            0              0            0
#> stop_utility                    0            0              0            0
#> reroute                         0            0              0            0
#> b_turning                       0            0              0            0
#> a_turning                       0            0              0            0
#> b_current_direction             0            0              0            0
#> a_current_direction             0            0              0            0
#> blr_current_direction           0            0              0            0
#> b_goal_direction                0            0              0            0
#> a_goal_direction                0            0              0            0
#> b_blocked                       0            0              0            0
#> a_blocked                       0            0              0            0
#> b_interpersonal                 0            0              0            0
#> a_interpersonal                 0            0              0            0
#> d_interpersonal                 0            0              0            0
#> b_preferred_speed               0            0              0            0
#> a_preferred_speed               0            0              0            0
#> b_leader                        0            0              0            0
#> a_leader                        0            0              0            0
#> d_leader                        0            0              0            0
#> b_buddy                         0            0              0            0
#> a_buddy                         0            0              0            0
#> a_group_centroid                0            0              0            0
#> b_group_centroid                0            0              0            0
#> b_visual_field                  0            0              0            0
#> central                         0            0              0            0
#> non_central                     0            0              0            0
#> acceleration                    0            0              0            0
#> constant_speed                  0            0              0            0
#> deceleration                    0            0              0            0
#> 
#> 
#> $params_bounds
#>                        [,1]    [,2]
#> radius                2e-01 3.0e-01
#> slowing_time          5e-01 2.5e+00
#> preferred_speed       1e-02 2.5e+00
#> randomness            1e-04 5.0e+00
#> stop_utility          1e+01 1.0e+06
#> reroute               2e+00 3.0e+01
#> b_turning             0e+00 1.0e+00
#> a_turning             0e+00 3.0e+00
#> b_current_direction   0e+00 2.0e+01
#> a_current_direction   0e+00 3.0e+00
#> blr_current_direction 5e-02 2.0e+01
#> b_goal_direction      0e+00 2.0e+01
#> a_goal_direction      0e+00 3.0e+00
#> b_blocked             5e-02 2.0e+01
#> a_blocked             0e+00 3.0e+00
#> b_interpersonal       0e+00 2.0e+01
#> a_interpersonal       0e+00 3.0e+00
#> d_interpersonal       0e+00 1.0e+00
#> b_preferred_speed     0e+00 2.0e+01
#> a_preferred_speed     0e+00 3.0e+00
#> b_leader              0e+00 3.2e+02
#> a_leader              0e+00 3.0e+00
#> d_leader              0e+00 2.2e+02
#> b_buddy               0e+00 2.2e+02
#> a_buddy               0e+00 3.0e+00
#> a_group_centroid      0e+00 4.0e+00
#> b_group_centroid      0e+00 2.0e+01
#> b_visual_field        0e+00 1.1e+02
#> central               0e+00 1.0e+00
#> non_central           0e+00 1.0e+00
#> acceleration          0e+00 1.0e+00
#> constant_speed        0e+00 1.0e+00
#> deceleration          0e+00 1.0e+00
#> 
my_model@weights
#> [1] 0.9 0.1
```
