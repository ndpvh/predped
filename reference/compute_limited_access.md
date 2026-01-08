# Transform `segment`s to `polygon`s

This function is used to transform an object of the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
to an object of the
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md).
Done as a precomputation in the constructor for the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
(filling the `precomputed_limited_access` slot based on the
`limited_access` slot.

## Usage

``` r
compute_limited_access(segment)
```

## Arguments

- segment:

  Object of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Value

Object of the
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md).

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
