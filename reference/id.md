# Getter/Setter for the `id`-slot

Works for all objects that have an `id`-slot, such as all extensions of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
and the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
id(object)

id(object) <- value

# S4 method for class 'object'
id(object)

# S4 method for class 'object'
id(object) <- value

# S4 method for class 'goal'
id(object)

# S4 method for class 'goal'
id(object) <- value

# S4 method for class 'agent'
id(object)

# S4 method for class 'agent'
id(object) <- value

# S4 method for class 'predped'
id(object)

# S4 method for class 'predped'
id(object) <- value
```

## Arguments

- object:

  An instance of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
  or
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- value:

  Value with which to replace the original value of the `id` slot.

## Details

Note that while the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
and
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
are not explicitly mentioned, this getter/setter works for these classes
as well.#'

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  id = "my agent")

# Access the id slot for the agent
id(my_agent)
#>   my agent 
#> "my agent" 

# Change the id slot for the agent
id(my_agent) <- "renamed agent"
id(my_agent)
#>   renamed agent 
#> "renamed agent" 
```
