# Create a vocabulary cdm_reference

Create a vocabulary cdm_reference

## Usage

``` r
cdmFromVocabulary(vocabulary, vocabularyPath = omopDataFolder("AthenaR"))
```

## Arguments

- vocabulary:

  The vocabulary version of interest. It will be downloaded if not
  already provided.

- vocabularyPath:

  Path to save/read the vocabulary files.

## Value

A cdm_reference object with the vocabulary of interest.

## Examples

``` r
if (FALSE) { # \dontrun{
library(AthenaR)

cdm <- cdmFromVocabulary(vocabulary = "v20260227")
} # }
```
