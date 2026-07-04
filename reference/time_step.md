# Getter/Setter for the `time_step`-slot

Works for
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md).

## Usage

``` r
time_step(object)

time_step(object) <- value

# S4 method for class 'trace'
time_step(object)

# S4 method for class 'trace'
time_step(object) <- value
```

## Arguments

- object:

  An instance of the
  [`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md).

- value:

  Value with which to replace the original value of the `states` slot.

## See also

[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)

## Examples

``` r
# Initialize trace
setting <- background(shape = rectangle(center = c(0, 0), size = c(5, 5)),
                      objects = list(circle(center = c(0, 0), radius = 1)))
my_trace <- trace(time_step = 0.5, 
                  setting = setting)

# Access the time_step slot for the trace
time_step(my_trace)
#> [1] 0.5

# Change the time_step slot for the agent
time_step(my_trace) <- 1
time_step(my_trace)
#> [1] 1
```
