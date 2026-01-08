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
#>              [,1]         [,2]
#>  [1,] 0.022552935  0.071528771
#>  [2,] 0.048209071  0.057453333
#>  [3,] 0.063254358  0.040297471
#>  [4,] 0.070476947  0.025651511
#>  [5,] 0.073860581  0.013023613
#>  [6,] 0.075000000  0.000000000
#>  [7,] 0.073566397 -0.012971741
#>  [8,] 0.058822571 -0.021409665
#>  [9,] 0.052794342 -0.033633705
#> [10,] 0.040237008 -0.047952599
#> [11,] 0.018823483 -0.059700462
#> [12,] 0.015035290  0.047685848
#> [13,] 0.032139380  0.038302222
#> [14,] 0.042169572  0.026864980
#> [15,] 0.046984631  0.017101007
#> [16,] 0.049240388  0.008682409
#> [17,] 0.050000000  0.000000000
#> [18,] 0.049044265 -0.008647827
#> [19,] 0.039215047 -0.014273110
#> [20,] 0.035196228 -0.022422470
#> [21,] 0.026824672 -0.031968400
#> [22,] 0.012548989 -0.039800308
#> [23,] 0.007517645  0.023842924
#> [24,] 0.016069690  0.019151111
#> [25,] 0.021084786  0.013432490
#> [26,] 0.023492316  0.008550504
#> [27,] 0.024620194  0.004341204
#> [28,] 0.025000000  0.000000000
#> [29,] 0.024522132 -0.004323914
#> [30,] 0.019607524 -0.007136555
#> [31,] 0.017598114 -0.011211235
#> [32,] 0.013412336 -0.015984200
#> [33,] 0.006274494 -0.019900154
```
