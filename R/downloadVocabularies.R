
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

  nm <- paste0(vocabulary, ".zip")
  fullPath <- file.path(path, nm)
  if (file.exists(fullPath)) {
    cli::cli_inform(c("!" = "File {.path {fullPath}} already exists."))
    overwrite <- utils::menu(choices = c("Yes, delete content.", "No, abort."), title = "Do you want to overwrite the content?")
    if (overwrite == 1) {
      unlink(fullPath)
    } else {
      cli::cli_abort(c("x" = "Aborting download, file already present"))
    }
  }

  safeDownload(url = url, dest = fullPath)
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
