# predped v0.4.3

Minor changes:

- Adjusted the output of `plot_distribution()` so that it either pastes all plots together into one (default when `ggpubr` is available) or returns all plots in a list;
- Added a warning if users want plots to be pasted together but `ggpubr` is not available;
- Added an internal function `generate_parameters()` that underlies the behavior of `plot_distribution()` and `generate_parameters()`;
- Suppressed warnings when pasting together the plots of `plot_distribution()`, as they are harmless and hard to get rid of even when changing plot limits (`"removing values because they fall outside of limits"` of `geom_histogram()`);
- Adding tests for `plot_distribution()`, `generate_parameters()`, and `get_parameters()`;
- Added tolerance to providing a message for the `weights` argument in `predped()`: This message was often displayed even with the default parameter set, limiting its usefulness for users.


# predped v0.4.1

Minor bugs were solved in this version:

- Changed how waiting is handled, allowing agents to temporarily move out of the way whenever multiple agents are blocking each other's access to their goals, preventing them from completing the goals;
- Make agents the `group_representative` by default, allowing users to specify their own agents without having to worry about this attribute.



# predped v0.4.0

- Initial release