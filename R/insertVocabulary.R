
#' Insert the vocabulary tables into a database
#'
#' @param vocabulary The vocabulary version of interest. It will be downloaded
#' if not already provided.
#' @param con A DBI_Connection to a database.
#' @param cdmSchema An existing schema where to insert the vocabularies.
#' @param vocabularyPath Path to save/read the vocabulary files.
#'
#' @returns invisible TRUE or infromative error.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AthenaR)
#' library(duckdb)
#'
#' con <- dbConnect(drv = duckdb())
#'
#' insertVocabulary(vocabulary = "v20260227", con = con, cdmSchema = "main")
#'
#' }
#'
insertVocabulary <- function(vocabulary,
                             con,
                             cdmSchema,
                             vocabularyPath = file.path(omopDataFolder(), "AthenaR")) {
  insertVocabularyInternal(
    vocabulary = vocabulary,
    con = con,
    cdmSchema = cdmSchema,
    vocabularyPath = vocabularyPath
  )
}

insertVocabularyInternal <- function(vocabulary,
                                     con,
                                     cdmSchema,
                                     vocabularyPath) {
  src <- CDMConnector::dbSource(con = con, writeSchema = cdmSchema)

  # download vocabulary
  downloadVocabularyInternal(
    vocabulary = vocabulary,
    vocabularyPath = vocabularyPath,
    overwrite = "no"
  )

  # unzip
  zipFile <- file.path(vocabularyPath, paste0(vocabulary, ".zip"))
  tmpDir <- file.path(tempdir(), omopgenerics::uniqueTableName())
  dir.create(tmpDir)
  zip::unzip(zipfile = zipFile, exdir = tmpDir)

  # extract CPT4 code

  # insert tables
  spec <- tableSpec()
  for (nm in names(spec)) {
    cli::cli_inform(c(i = "Inserting table: {.pkg {nm}}."))
    file <- file.path(tmpDir, paste0(toupper(nm), ".csv"))
    x <- readr::read_delim(file = file, delim = "\t", col_types = spec[[nm]])
    omopgenerics::insertTable(src, name = nm, table = x)
  }
  unlink(tmpDir, recursive = TRUE)

  # create cdm reference
  CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = cdmSchema,
    writeSchema = cdmSchema,
    cdmName = vocabulary,
    .softValidation = TRUE
  )
}
tableSpec <- function() {
  list(
    "concept_ancestor" = c(
      ancestor_concept_id = "i", descendant_concept_id = "i",
      min_levels_of_separation = "i", max_levels_of_separation = "i"
    ),
    "concept_class" = c(
      concept_class_id = "c", concept_class_name = "c",
      concept_class_concept_id = "i"
    ),
    "concept_relationship" = c(
      concept_id_1 = "i", concept_id_2 = "i", relationship_id = "c",
      valid_start_date = "D", valid_end_date = "D", invalid_reason = "c"
    ),
    "concept_synonym" = c(
      concept_id = "i", concept_synonym_name = "c", language_concept_id = "i"
    ),
    "concept" = c(
      concept_id = "i", concept_name = "c", domain_id = "c",
      vocabulary_id = "c", concept_class_id = "c", standard_concept = "c",
      concept_code = "c", valid_start_date = "D", valid_end_date = "D",
      invalid_reason = "c"
    ),
    "domain" = c(domain_id = "c", domain_name ="c", domain_concept_id = "i"),
    "drug_strength" = c(
      drug_concept_id = "i", ingredient_concept_id = "i", amount_value = "d",
      amount_unit_concept_id = "i", numerator_value = "d",
      numerator_unit_concept_id = "i", denominator_value = "d",
      denominator_unit_concept_id = "i", box_size = "i", valid_start_date = "D",
      valid_end_date = "D", invalid_reason = "c"
    ),
    "relationship" = c(
      relationship_id = "c", relationship_name = "c", is_hierarchical = "c",
      defines_ancestry = "c", reverse_relationship_id = "c",
      relationship_concept_id = "i"
    ),
    "vocabulary" = c(
      vocabulary_id = "c", vocabulary_name = "c", vocabulary_reference = "c",
      vocabulary_version = "c", vocabulary_concept_id = "i"
    )
  )
}
