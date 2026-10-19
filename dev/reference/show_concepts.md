# Show safedata metadata

These functions provide access to the metadata associated with safedata
datasets. The functions provide three levels of information:

## Usage

``` r
show_concepts(obj)

show_record(obj)

show_worksheet(obj, worksheet = NULL, extended_fields = FALSE)
```

## Arguments

- obj:

  A reference to safedata records or a loaded worksheet (see above)

- worksheet:

  The name of a worksheet to show. Obviously, if `obj` is a loaded
  worksheet, that will be the worksheet described and this can be left
  as NULL.

- extended_fields:

  Logical - show a compact description of worksheet fields or a longer
  output including full metadata descriptors.

## Value

Invisibly, a safedata metadata object or a list of such objects. These
are not really intended for end user consumption.

## Details

- `show_concepts`:

  displays the record versions grouped under dataset concepts,

- `show_record`:

  displays summary information about a specific record, and

- `show_worksheet`:

  displays metadata about data worksheet fields within a record.

All three functions accept a first argument `obj`, which can be one of
three things:

1.  A character or numeric vector of safedata record or concept ids,
    which will be validated using
    [`validate_record_ids`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md),
    or

2.  An already validated
    [`safe_record_set`](https://imperialcollegelondon.github.io/safedata/dev/reference/validate_record_ids.md)
    object, or

3.  A `safedata` data frame loaded using
    [`load_safe_data`](https://imperialcollegelondon.github.io/safedata/dev/reference/load_safe_data.md).

If `show_concepts` is passed a record id, then the function looks up the
relevant concept. The version table indicates which versions are
available ("\*" for the most recent available version and "o" for older
available versions), and which are unavailable due to embargo or
restriction ("x"). A "!" is used to show that a private local copy of an
embargoed or restricted dataset has been inserted using
[`insert_dataset`](https://imperialcollegelondon.github.io/safedata/dev/reference/insert_dataset.md).

## Functions

- `show_concepts()`: Show the records associated with a dataset concept.

- `show_record()`: Show details of a specific dataset

- `show_worksheet()`: Show details of a data worksheet

## Examples

``` r
   set_example_safedata_dir()
   recs <- validate_record_ids(c(1400562, 3266827, 3266821))
   show_concepts(recs)
#> 
#> Concept ID: 1400561
#> Title: Myrmecophilous Pselaphine beetles in tropical forests
#> Versions: 1 available, 0 embargoed or restricted
#> 
#>  record_id  published embargo available
#>    1400562 2018-08-20      --         *
#> 
#> -------------
#> 
#> Concept ID: 3266821
#> Title: Microclimate proxy measurements from a logging gradient in Malaysian Borneo (BALI project)
#> Versions: 1 available, 0 embargoed or restricted
#> 
#>  record_id  published embargo available
#>    3266822 2019-07-03      --         *
#> 
#> -------------
#> 
#> Concept ID: 3266826
#> Title: Quantifying the spatial heterogeneity of forest conversion costs and how it relates to biodiversity, conservation and land use history
#> Versions: 1 available, 0 embargoed or restricted
#> 
#>  record_id  published embargo available
#>    3266827 2019-07-03      --         *
#> 
   show_record(recs[1,])
#> 
#> Record summary
#> Title: Myrmecophilous Pselaphine beetles in tropical forests
#> Authors: Psomas
#> Publication date: 2018-08-20
#> Record ID: 1400562
#> Concept ID: 1400561
#> Status: open
#> Taxa: 
#>  - 67 GBIF taxa reported
#> Locations: 20 locations reported
#> 
#> Data worksheets:
#>             name ncol nrow description
#> EnvironVariables    5   20 Environmental variables
#>         Ant-Psel    4   20 Ant-Pselaphine data
#>   MorphAbundance   44   20 Morphospeices abundance
#> MorphFunctTraits   18   42 Morphospecies_Functional Traits
#> 
   # Show worksheet metadata from a record or from a loaded worksheet
   show_worksheet(1400562, "EnvironVariables")
#> Record ID: 1400562
#> Worksheet name: EnvironVariables
#> Number of data rows: 20
#> Number of data fields: 4
#> Description:
#> Environmental variables
#> Status: open
#> 
#> Fields:
#>   field_name field_type description         
#> 1 Site       Location   Site of measurements
#> 2 Temp       Numeric    Soil temperature    
#> 3 Moisture   Numeric    Soil moisture       
#> 4 Cover      Numeric    Canopy cover        
   beetle_abund <- load_safe_data(1400562, "Ant-Psel")
   show_worksheet(beetle_abund, extended_fields = TRUE)
#> Record ID: 1400562
#> Worksheet name: Ant-Psel
#> Number of data rows: 20
#> Number of data fields: 3
#> Description:
#> Ant-Pselaphine data
#> Status: open
#> 
#> Fields:
#> Site :
#>  - field_type: Location
#>  - interaction_name: Na
#>  - description: Site where sample was collected
#> ant species richness :
#>  - field_type: Numeric
#>  - description: Number of ant species in sample
#>  - method: Winkler bag
#>  - units: #
#> ant abundance :
#>  - field_type: Abundance
#>  - description: Total number of ants in sample
#>  - method: Winkler bag
#>  - units: #
#>  - taxon_name: Ants
#> 
   set_example_safedata_dir(on=FALSE)
#> No response from server, skipping updates: http://example.safedata.server
#> Loading and caching index
#> Validating directory
```
