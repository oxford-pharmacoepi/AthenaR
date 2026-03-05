
#' Download a certain vocabulary version from ATHENA
#'
#' @param vocabulary The vocabulary version to download.
#' @param path Path to save the vocabulary files.
#'
#' @returns The vocabularies are downloaded in path.
#' @export
#'
#' @examples
#' \dontrun{
#' downloadVocabulary(vocabulary = "v20260227")
#' }
#'
downloadVocabulary <- function(vocabulary, path = getwd()) {
  # input check
  vocabularies <- fetchVocabularies()
  omopgenerics::assertChoice(vocabulary, vocabularies$vocabulary_version, length = 1)
  omopgenerics::assertCharacter(path, length = 1)
  url <- vocabularies$link[vocabularies$vocabulary_version == vocabulary]

  if (!dir.exists(path)) {
    cli::cli_abort(c("x" = "{.path {path}} does not exist."))
  }

  pathV <- file.path(path, vocabulary)
  if (dir.exists(pathV)) {
    cli::cli_inform(c("!" = "Folder {.pkg {vocabulary}} already exists in {.path {path}}"))
    if (!rlang::is_interactive()) {
      cli::cli_inform(c("!" = "Deleting existing content in {.path {pathV}}."))
      unlink(pathV, recursive = TRUE)
    } else {
      overwrite <- utils::menu(choices = c("Yes, delete content.", "No, abort."), title = "Do you want to overwrite the content?")
      if (overwrite == 1) {
        unlink(pathV, recursive = TRUE)
      } else {
        cli::cli_abort(c("x" = "Aborting download, files already present"))
      }
    }
  }

  dir.create(pathV)

  utils::download.file(url = url, destfile = file.path(pathV, "raw.zip"))
}

fetchVocabularies <- function() {
  utils::read.csv(file = "https://raw.githubusercontent.com/oxford-pharmacoepi/AthenaR/refs/heads/main/extras/links.csv")
}
