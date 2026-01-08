# Make combinations of different nodes

This function connects all the provided nodes to each other, effectively
creating all the potential paths that the agent can use to move to their
goal.

## Usage

``` r
combine_nodes(nodes_1, nodes_2 = NULL)
```

## Arguments

- nodes_1:

  Dataframe of nodes with columns `"node_ID"`, `"X"`, and `"Y"`.

- nodes_2:

  Dataframe of the different nodes than `nodes_1`, but with the same
  structure. When `NULL`, triggers the function to make all possible
  combinations between the nodes inside of `nodes_1`. If not `NULL`, all
  possible combinations between the nodes in `nodes_1` with those of
  `nodes_2` will be made. Defaults to `NULL`.

## Value

Dataframe containing the combinations of the nodes, where all
information is contained within columns `"from"`, `"from_x"`,
`"from_y"`, `"to"`, `"to_x"`, and `"to_y"`.

## Details

How this function works depends on whether only one dataframe or two
dataframes are provided to the function. If one dataframe is provided,
all possible combinations between the nodes inside of this dataframe are
created. If two dataframes are provided, all possible combinations of
the nodes across dataframes are created.

## See also

[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md)

## Examples

``` r
# Let's create a dataframe containing some nodes
nodes <- data.frame(node_ID = c("node 1", "node 2", "node 3"), 
                    X = 1:3, 
                    Y = 1:3)

# Combine all nodes in this dataframe to each other
edges <- combine_nodes(nodes)
head(edges)
#>       from from_x from_y     to to_x to_y
#> 1.1 node 1      1      1 node 2    2    2
#> 1.2 node 1      1      1 node 3    3    3
#> 2   node 2      2      2 node 1    1    1
#> 2.2 node 2      2      2 node 3    3    3
#> 3   node 3      3      3 node 1    1    1
#> 3.1 node 3      3      3 node 2    2    2

# Create a second dataframe and combine all its nodes to the ones in the 
# first dataframe
nodes_2 <- data.frame(node_ID = c("node 4", "node 5", "node 6"), 
                      X = 4:6, 
                      Y = 4:6)

edges <- combine_nodes(nodes, nodes_2)
head(edges) 
#>       from from_x from_y     to to_x to_y
#> 1   node 1      1      1 node 4    4    4
#> 1.1 node 1      1      1 node 5    5    5
#> 1.2 node 1      1      1 node 6    6    6
#> 2   node 2      2      2 node 4    4    4
#> 2.1 node 2      2      2 node 5    5    5
#> 2.2 node 2      2      2 node 6    6    6
```
