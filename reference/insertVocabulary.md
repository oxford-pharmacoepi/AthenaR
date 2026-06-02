# Insert the vocabulary tables into a database

Insert the vocabulary tables into a database

## Usage

``` r
insertVocabulary(
  vocabulary,
  con,
  cdmSchema,
  vocabularyPath = omopDataFolder("AthenaR")
)
```

## Arguments

- vocabulary:

  The vocabulary version of interest. It will be downloaded if not
  already provided.

- con:

  A DBI_Connection to a database.

- cdmSchema:

  An existing schema where to insert the vocabularies.

- vocabularyPath:

  Path to save/read the vocabulary files.

## Value

invisible TRUE or infromative error.

## Examples

``` r
if (FALSE) { # \dontrun{
library(AthenaR)
library(duckdb)

con <- dbConnect(drv = duckdb())

insertVocabulary(vocabulary = "v20260227", con = con, cdmSchema = "main")

} # }
```
