# Changelog

## predped v0.4.3

Minor changes:

- Adjusted the output of
  [`plot_distribution()`](https://github.com/ndpvh/predped/reference/plot_distribution.md)
  so that it either pastes all plots together into one (default when
  `ggpubr` is available) or returns all plots in a list;
- Added a warning if users want plots to be pasted together but `ggpubr`
  is not available;
- Added an internal function
  [`generate_parameters()`](https://github.com/ndpvh/predped/reference/generate_parameters.md)
  that underlies the behavior of
  [`plot_distribution()`](https://github.com/ndpvh/predped/reference/plot_distribution.md)
  and
  [`generate_parameters()`](https://github.com/ndpvh/predped/reference/generate_parameters.md);
- Suppressed warnings when pasting together the plots of
  [`plot_distribution()`](https://github.com/ndpvh/predped/reference/plot_distribution.md),
  as they are harmless and hard to get rid of even when changing plot
  limits (`"removing values because they fall outside of limits"` of
  `geom_histogram()`);
- Adding tests for
  [`plot_distribution()`](https://github.com/ndpvh/predped/reference/plot_distribution.md),
  [`generate_parameters()`](https://github.com/ndpvh/predped/reference/generate_parameters.md),
  and
  [`get_parameters()`](https://github.com/ndpvh/predped/reference/get_parameters.md);
- Added tolerance to providing a message for the `weights` argument in
  [`predped()`](https://github.com/ndpvh/predped/reference/predped-class.md):
  This message was often displayed even with the default parameter set,
  limiting its usefulness for users.

## predped v0.4.1

Minor bugs were solved in this version:

- Changed how waiting is handled, allowing agents to temporarily move
  out of the way whenever multiple agents are blocking each other’s
  access to their goals, preventing them from completing the goals;
- Make agents the `group_representative` by default, allowing users to
  specify their own agents without having to worry about this attribute.

## predped v0.4.0

- Initial release
