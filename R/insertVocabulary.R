
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
                             vocabularyPath = omopDataFolder("AthenaR")) {
  vocabularies <- fetchVocabularies()
  vocabulary <- validateVocabulary(vocabulary, vocabularies)

}
