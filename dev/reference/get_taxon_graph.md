# Get a graph of the taxa in a dataset

This function loads the taxon index for either of the NCBI or GBIF taxa
present in a dataset (see
[`get_taxa`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxa.md))
and creates a taxonomic graph for the dataset using the igraph package.

## Usage

``` r
get_taxon_graph(record, which = c("gbif", "ncbi"))
```

## Arguments

- record:

  A single dataset record id

- which:

  Return the GBIF or NCBI taxon graph

## Value

An [`graph`](https://r.igraph.org/reference/make_graph.html) object.

## Details

The graph vertices are the different taxa used in the dataset along with
nodes for all parent taxa of those taxa. The graph vertex names are
arbitrary strings (tx1, ...) but the vertex attributes contain the
worksheet name, validated taxon name, taxon rank and taxon status (see
`names(vertex_attr(obj))`).

## Examples

``` r
   set_example_safedata_dir()
   beetle_graph <- get_taxon_graph(1400562)
   plot(beetle_graph, vertex.label.cex = 0.6, vertex.size = 15,
        vertex.size2 = 3, vertex.shape = "rectangle")

   # show worksheet names for tips
   wsn <- igraph::vertex_attr(beetle_graph, "worksheet_name")
   txn <- igraph::vertex_attr(beetle_graph, "taxon_name")
   labels <- ifelse(is.na(wsn), txn, wsn)
   is_leaf <- igraph::vertex_attr(beetle_graph, "leaf")
   vert_col <- ifelse(is_leaf, "cornflowerblue", "grey")
   plot(beetle_graph, vertex.label.cex = 0.6, vertex.size = 15,
        vertex.size2 = 3, vertex.shape = "rectangle",
        vertex.label = labels, vertex.color= vert_col)

   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
