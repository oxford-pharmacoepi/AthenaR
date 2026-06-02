# Download a certain vocabulary version from ATHENA

Download a certain vocabulary version from ATHENA

## Usage

``` r
downloadVocabulary(vocabulary, path = omopDataFolder("AthenaR"))
```

## Arguments

- vocabulary:

  The vocabulary version to download.

- vocabularyPath:

  Path to save the vocabulary files.

## Value

The vocabularies are downloaded in path.

## Examples

``` r
if (FALSE) { # \dontrun{
library(AthenaR)

downloadVocabulary(vocabulary = "v20260227")
} # }
```
