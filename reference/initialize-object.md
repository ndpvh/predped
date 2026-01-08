# Constructor for the [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)

Constructor for the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)

## Usage

``` r
# S4 method for class 'object'
initialize(
  .Object,
  id = character(0),
  moveable = FALSE,
  interactable = TRUE,
  ...
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- id:

  Character denoting the identifier of the object Defaults to an empty
  string, triggering the creation of a random identifier.

- moveable:

  Logical denoting whether the position of the object can be changed.
  Defaults to `FALSE`.

- interactable:

  Logical denoting whether the object can be interacted with. Defaults
  to `TRUE`.

- ...:

  Arguments passed on up in the hierarchy

## Value

Object of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
