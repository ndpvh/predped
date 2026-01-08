# Load parameters

Read in the parameters from either a database or from the
predped-provided csv-files. Only reads in data from database (a) when a
database is provided, (b) when that database exists and (c) when that
database contains the required table. If either of these conditions is
not satisfied, we read in the csv-files that are present in predped.

## Usage

``` r
load_parameters(x = NULL, sep = ",")
```

## Arguments

- x:

  Character denoting the path to a file containing parameters. Defaults
  to `NULL`, triggering reading in the csv-files that come with predped.

- sep:

  Character denoting the separator in case `x` is a delimited file.
  Defaults to `","`.

## Value

List of parameter values contained in `params_archetypes` (means),
`params_sigma` (standard deviations), and `params_bounds` (the bounds of
the parameters).

## Details

Note that reading in from a database is currently only supported for the
mean values or `params_archetypes`, not for the standard deviations in
`params_sigma` and bounds in `params_bounds`.

## See also

[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)

## Examples

``` r
# Read in the datasets from the predped-provided csv-files
parameters <- load_parameters()
head(parameters)
#> $params_archetypes
#>                         name          color radius slowing_time preferred_speed
#> 1           BaselineEuropean cornflowerblue   0.25          1.0           1.250
#> 2            BigRushingDutch         salmon   0.29          1.0           2.000
#> 3                DrunkAussie      goldenrod   0.25          1.0           1.000
#> 4        CautiousOldEuropean         grey50   0.25          1.0           0.500
#> 5                     Rushed darkolivegreen   0.25          1.0           2.000
#> 6                 Distracted        #FF66CC   0.25          0.5           1.037
#> 7          BaselineEuropean1 cornflowerblue   0.25          1.0           1.250
#> 8           BigRushingDutch1         salmon   0.29          1.0           2.000
#> 9               DrunkAussie1      goldenrod   0.25          1.0           1.000
#> 10      CautiousOldEuropean1         grey50   0.25          1.0           0.500
#> 11                   Rushed1 darkolivegreen   0.25          1.0           2.000
#> 12               Distracted1        #FF66CC   0.25          0.5           1.037
#> 13                      Lost        #35BBAF   0.25          1.0           1.250
#> 14           SociallyAnxious        #42FA4A   0.25          1.0           1.250
#> 15    SocialBaselineEuropean cornflowerblue   0.25          1.0           1.250
#> 16     SocialBigRushingDutch         salmon   0.29          1.0           2.000
#> 17         SocialDrunkAussie      goldenrod   0.25          1.0           1.000
#> 18 SocialCautiousOldEuropean         grey50   0.25          1.0           0.500
#>    randomness stop_utility reroute b_turning a_turning b_current_direction
#> 1       0.100        10000      10       0.2         2                   1
#> 2       0.001       100000      20       0.2         2                   1
#> 3       5.000         1000      20       0.2         2                   1
#> 4       0.001          100       5       0.2         2                   1
#> 5       0.010       100000      20       0.2         2                   1
#> 6       3.000         1000      15       0.2         2                   1
#> 7       0.100        10000      10       0.2         2                   1
#> 8       0.001       100000      20       0.2         2                   1
#> 9       5.000         1000      20       0.2         2                   1
#> 10      0.001          100       5       0.2         2                   1
#> 11      0.010       100000      20       0.2         2                   1
#> 12      3.000         1000      15       0.2         2                   1
#> 13      5.000         2500       5       0.2         2                   1
#> 14      0.010       100000       4       0.2         2                   1
#> 15      0.100        10000      10       0.2         2                   1
#> 16      0.001       100000      20       0.2         2                   1
#> 17      5.000         1000      20       0.2         2                   1
#> 18      0.001          100       5       0.2         2                   1
#>    a_current_direction blr_current_direction b_goal_direction a_goal_direction
#> 1                    2                  10.0               10                2
#> 2                    2                  10.0               20                2
#> 3                    2                   0.1               10                2
#> 4                    2                  10.0               10                2
#> 5                    2                   1.0               20                2
#> 6                    2                   3.0               12                2
#> 7                    2                  10.0               10                2
#> 8                    2                  10.0               20                2
#> 9                    2                   0.1               10                2
#> 10                   2                  10.0               10                2
#> 11                   2                   1.0               20                2
#> 12                   2                   3.0               12                2
#> 13                   2                   1.0               10                2
#> 14                   2                   1.0               12                2
#> 15                   2                  10.0               10                2
#> 16                   2                  10.0               20                2
#> 17                   2                   0.1               10                2
#> 18                   2                  10.0               10                2
#>    b_blocked a_blocked b_interpersonal a_interpersonal d_interpersonal
#> 1          4         2             2.0               2               0
#> 2          2         2             1.0               2               0
#> 3          4         2             1.0               2               0
#> 4          4         2             4.0               2               0
#> 5          1         2             0.5               2               0
#> 6          1         2             0.0               2               0
#> 7          4         2             7.0               2               0
#> 8          2         2             6.0               2               0
#> 9          4         2             6.0               2               0
#> 10         4         2             9.0               2               0
#> 11         1         2             5.5               2               0
#> 12         1         2             5.0               2               0
#> 13         2         2             1.0               2               0
#> 14        12         2             3.0               2               0
#> 15         4         2             2.0               2               0
#> 16         2         2             1.0               2               0
#> 17         4         2             1.0               2               0
#> 18         4         2             4.0               2               0
#>    b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> 1                  2                 2      0.0        0        0       0
#> 2                  4                 2      1.0        2        0       0
#> 3                  1                 2      1.0        2        0       0
#> 4                  2                 2      2.0        2        0       0
#> 5                  8                 2      1.0        2        0       0
#> 6                  4                 2      0.5        2        0       5
#> 7                  2                 2      2.0        2        0       0
#> 8                  4                 2      1.0        2        0       0
#> 9                  1                 2      1.0        2        0       0
#> 10                 2                 2      2.0        2        0       0
#> 11                 8                 2      1.0        2        0       0
#> 12                 4                 2      0.5        2        0       5
#> 13                 2                 2     30.0        2        0       5
#> 14                 1                 2      0.5        2        0       0
#> 15                 2                 2     50.0        2       50      50
#> 16                 4                 2      1.0        2       50      50
#> 17                 1                 2      1.0        2       50      50
#> 18                 2                 2      2.0        2       50      50
#>    a_buddy a_group_centroid b_group_centroid b_visual_field central non_central
#> 1        0                0                0              0       0           0
#> 2        0                0                0              0       0           0
#> 3        0                0                0              0       0           0
#> 4        0                0                0              0       0           0
#> 5        0                0                0              0       0           0
#> 6        0                0                0              0       0           0
#> 7        0                0                0              0       0           0
#> 8        0                0                0              0       0           0
#> 9        0                0                0              0       0           0
#> 10       0                0                0              0       0           0
#> 11       0                0                0              0       0           0
#> 12       0                0                0              0       0           0
#> 13       0                0                0              0       0           0
#> 14       0                0                0              0       0           0
#> 15       2                2                4             60       0           0
#> 16       2                2                4             60       0           0
#> 17       2                2                4             60       0           0
#> 18       2                2                4             60       0           0
#>    acceleration constant_speed deceleration
#> 1             0              0            0
#> 2             0              0            0
#> 3             0              0            0
#> 4             0              0            0
#> 5             0              0            0
#> 6             0              0            0
#> 7             0              0            0
#> 8             0              0            0
#> 9             0              0            0
#> 10            0              0            0
#> 11            0              0            0
#> 12            0              0            0
#> 13            0              0            0
#> 14            0              0            0
#> 15            0              0            0
#> 16            0              0            0
#> 17            0              0            0
#> 18            0              0            0
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
#> $params_sigma$BigRushingDutch
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
#> $params_sigma$CautiousOldEuropean
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
#> $params_sigma$Rushed
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
#> $params_sigma$Distracted
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
#> $params_sigma$BaselineEuropean1
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
#> $params_sigma$BigRushingDutch1
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
#> $params_sigma$DrunkAussie1
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
#> $params_sigma$CautiousOldEuropean1
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
#> $params_sigma$Rushed1
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
#> $params_sigma$Distracted1
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
#> $params_sigma$Lost
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
#> $params_sigma$SociallyAnxious
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
#> $params_sigma$SocialBaselineEuropean
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
#> $params_sigma$SocialBigRushingDutch
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
#> $params_sigma$SocialDrunkAussie
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
#> $params_sigma$SocialCautiousOldEuropean
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
```
