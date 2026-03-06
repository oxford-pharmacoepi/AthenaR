
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
  url <- vocabularies$url[vocabularies$vocabulary_version == vocabulary]

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

  safeDownload(url = url, dest = file.path(pathV, "raw.zip"))
}

fetchVocabularies <- function() {
  utils::read.csv(file = "https://raw.githubusercontent.com/oxford-pharmacoepi/AthenaR/refs/heads/main/extras/links.csv")
}
safeDownload <- function(url, dest) {
  to <- getOption("timeout")
  cli::cli_inform(c("i" = "Attempting download with {.emph timeout = {.pkg {to}}}"))

  dw <- tryCatch({
    download(url = url, dest = dest, to = to)
    TRUE
  },
  error = function(e) {
    FALSE
  })

  if (isFALSE(dw)) {
    cli::cli_inform(c("!" = "First attempt failed, attempting second download with {.emph timeout = {.pkg {5 * to}}}"))
    dw <- tryCatch({
      download(url = url, dest = dest, to = 5 * to)
      TRUE
    },
    error = function(e) {
      FALSE
    })
    if (isFALSE(dw)) {
      cli::cli_inform(c("x" = "Second attempt failed, try increase manually timeout with {.code options(timeout = xxx)}."))
    }
  }

  return(dw)
}
download <- function(url, dest, to) {
  withr::with_options(list(timeout = to), {
    utils::download.file(
      url = url, destfile = dest, mode = "wb", method = "auto", quiet = FALSE
    )
  })
}
