# Create a \`DuckDB\` source

Create a \`DuckDB\` source

## Usage

``` r
duckdbSource(con = NULL, writeSchema = NULL, writePrefix = NULL)
```

## Arguments

- con:

  A DuckDB connection or path to a duckdb file. If NULL a temp
  connection will be created.

- writeSchema:

  ws

- writePrefix:

  wp

## Value

a \`DuckDB\` source.

## Examples

``` r
library(OmopOnDuckDB)

src <- duckdbSource()
#> ℹ Creating duckdb connection in /tmp/RtmpWqKVAI/file18f92a401fcb.duckdb.
```
