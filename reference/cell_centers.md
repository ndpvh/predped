# Getter/Setter for the `cell_centers`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
cell_centers(object)

cell_centers(object) <- value

# S4 method for class 'agent'
cell_centers(object)

# S4 method for class 'agent'
cell_centers(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `cell_centers`
  slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  cell_centers = matrix(1, nrow = 33, ncol = 2))

# Access the cell centers for the agent
cell_centers(my_agent)
#>       [,1] [,2]
#>  [1,]    1    1
#>  [2,]    1    1
#>  [3,]    1    1
#>  [4,]    1    1
#>  [5,]    1    1
#>  [6,]    1    1
#>  [7,]    1    1
#>  [8,]    1    1
#>  [9,]    1    1
#> [10,]    1    1
#> [11,]    1    1
#> [12,]    1    1
#> [13,]    1    1
#> [14,]    1    1
#> [15,]    1    1
#> [16,]    1    1
#> [17,]    1    1
#> [18,]    1    1
#> [19,]    1    1
#> [20,]    1    1
#> [21,]    1    1
#> [22,]    1    1
#> [23,]    1    1
#> [24,]    1    1
#> [25,]    1    1
#> [26,]    1    1
#> [27,]    1    1
#> [28,]    1    1
#> [29,]    1    1
#> [30,]    1    1
#> [31,]    1    1
#> [32,]    1    1
#> [33,]    1    1

# Change the cell centers for the agent
cell_centers(my_agent) <- compute_centers(my_agent)
cell_centers(my_agent)
#>             [,1]         [,2]
#>  [1,] 0.02097582  0.066526806
#>  [2,] 0.04648698  0.055401029
#>  [3,] 0.06226374  0.039666378
#>  [4,] 0.07005192  0.025496813
#>  [5,] 0.07374837  0.013003828
#>  [6,] 0.07500000  0.000000000
#>  [7,] 0.07374837 -0.013003828
#>  [8,] 0.07005192 -0.025496813
#>  [9,] 0.06226374 -0.039666378
#> [10,] 0.04648698 -0.055401029
#> [11,] 0.02097582 -0.066526806
#> [12,] 0.01398388  0.044351204
#> [13,] 0.03099132  0.036934019
#> [14,] 0.04150916  0.026444252
#> [15,] 0.04670128  0.016997875
#> [16,] 0.04916558  0.008669218
#> [17,] 0.05000000  0.000000000
#> [18,] 0.04916558 -0.008669218
#> [19,] 0.04670128 -0.016997875
#> [20,] 0.04150916 -0.026444252
#> [21,] 0.03099132 -0.036934019
#> [22,] 0.01398388 -0.044351204
#> [23,] 0.00699194  0.022175602
#> [24,] 0.01549566  0.018467010
#> [25,] 0.02075458  0.013222126
#> [26,] 0.02335064  0.008498938
#> [27,] 0.02458279  0.004334609
#> [28,] 0.02500000  0.000000000
#> [29,] 0.02458279 -0.004334609
#> [30,] 0.02335064 -0.008498938
#> [31,] 0.02075458 -0.013222126
#> [32,] 0.01549566 -0.018467010
#> [33,] 0.00699194 -0.022175602
```
