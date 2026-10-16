# Getting a phylogeny for a dataset

The function `igraph_to_phylo` takes a taxon graph (see
[`get_taxon_graph`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_graph.md))
and attempts to convert that to a a
[`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html) object from ape.
This will fail if the graph is not simple (no loops or multiple edges)
or is not connected (has isolated taxa). Neither of these conditions
should happen in datasets but they do.

## Usage

``` r
igraph_to_phylo(g, labels = "taxon_name")

get_phylogeny(record, labels = "taxon_name")
```

## Arguments

- g:

  A taxon graph returned by
  [`get_taxon_graph`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_graph.md)

- labels:

  The name of a vertex attribute from
  [`get_taxon_graph`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_graph.md)
  to use as node labels.

- record:

  A single dataset record id

## Value

An [`phylo`](https://rdrr.io/pkg/ape/man/read.tree.html) object.

## Details

The phylogeny is assigned with equal branch lengths and so displays
showing the taxonomic hierarchy of taxa. Note that the phylogeny will
contain singleton nodes if an internal taxon has a single descendant -
see the example below showing internal node labels. The
[`ape`](https://rdrr.io/pkg/ape/man/ape-package.html) functions
[`has.singles`](https://rdrr.io/pkg/ape/man/collapse.singles.html) and
[`collapse.singles`](https://rdrr.io/pkg/ape/man/collapse.singles.html)
can be used to detect and remove these if required.

The function `get_phylogeny` is simply a wrapper that calls
[`get_taxon_graph`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_graph.md)
and then `igraph_to_phylo`.

## Functions

- `get_phylogeny()`: Get a phylogeny for a dataset

## See also

[`get_taxon_graph`](https://imperialcollegelondon.github.io/safedata/dev/reference/get_taxon_graph.md)

## Examples

``` r
   set_example_safedata_dir()
   beetle_graph <- get_taxon_graph(1400562)
   beetle_phylo <- igraph_to_phylo(beetle_graph)
   ape::plot.phylo(beetle_phylo, show.node.label = TRUE)

   # Or wrapped into a single function
   beetle_phylo <- get_phylogeny(1400562)
   ape::plot.phylo(beetle_phylo, show.node.label = TRUE)
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
