
#' Create a vocabulary cdm_reference
#'
#' @param vocabulary The vocabulary version of interest. It will be downloaded
#' if not already provided.
#' @param vocabularyPath Path to save/read the vocabulary files.
#'
#' @returns A cdm_reference object with the vocabulary of interest.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AthenaR)
#'
#' cdm <- cdmFromVocabulary(vocabulary = "v20260227")
#' }
#'
cdmFromVocabulary <- function(vocabulary,
                              vocabularyPath = file.path(omopDataFolder(), "AthenaR")) {
  vocabularies <- fetchVocabularies()
  vocabulary <- validateVocabulary(vocabulary, vocabularies)
  vocabularyPath <- validateVocabularyPath(vocabularyPath)

  # assert if cdm exists
  dbdir <- file.path(vocabularyPath, paste0(vocabulary, ".duckdb"))
  if (!file.exists(dbdir)) {
    # download vocabulary
    downloadVocabularyInternal(
      vocabulary = vocabulary,
      vocabularyPath = vocabularyPath,
      overwrite = "no"
    )

    # create duckdb connection
    con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = dbdir))
    duckdb::dbSendQuery(conn = con, statement = "CREATE SCHEMA results")

    # insert tables
    insertVocabularyInternal(
      vocabulary = vocabulary,
      con = con,
      cdmSchema = "main",
      vocabularyPath = vocabularyPath
    ) |>
      omopgenerics::emptyOmopTable(name = "person") |>
      omopgenerics::emptyOmopTable(name = "observation_period")
  } else {
    cli::cli_inform(c(i = "Reading existing {.path {dbdir}}."))
    con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = dbdir))
  }

  CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "results",
    cdmName = vocabulary
  )
}
