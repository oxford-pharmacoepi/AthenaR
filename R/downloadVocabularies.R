
#' Download a certain vocabulary version from ATHENA
#'
#' @param vocabulary The vocabulary version to download.
#' @param vocabularyPath Path to save the vocabulary files.
#'
#' @returns The vocabularies are downloaded in path.
#' @export
#'
#' @examples
#' \dontrun{
#' library(AthenaR)
#'
#' downloadVocabulary(vocabulary = "v20260227")
#' }
#'
downloadVocabulary <- function(vocabulary,
                               vocabularyPath = file.path(omopDataFolder(), "AthenaR")) {
  downloadVocabularyInternal(
    vocabulary = vocabulary,
    vocabularyPath = vocabularyPath,
    overwrite = "ask"
  )
}

downloadVocabularyInternal <- function(vocabulary,
                                       vocabularyPath,
                                       overwrite = "ask",
                                       call = parent.frame()) {
  # input check
  vocabularies <- fetchVocabularies()
  vocabulary <- validateVocabulary(vocabulary, vocabularies, call)
  vocabularyPath <- validateVocabularyPath(vocabularyPath, call)

  url <- vocabularies$url[vocabularies$vocabulary_version == vocabulary]

  nm <- paste0(vocabulary, ".zip")
  fullPath <- file.path(vocabularyPath, nm)
  if (file.exists(fullPath)) {
    if (overwrite == "no") {
      return(TRUE)
    } else if (overwrite == "ask") {
      cli::cli_inform(c("!" = "File {.path {fullPath}} already exists."))
      overwrite <- utils::menu(choices = c("Yes, delete content.", "No"), title = "Do you want to overwrite the content?")
      if (overwrite == 1) {
        unlink(fullPath)
      } else {
        return(TRUE)
      }
    }
  }

  safeDownload(url = url, dest = fullPath, call = call)
}
safeDownload <- function(url, dest, call) {
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
      cli::cli_abort(c("x" = "Second attempt failed, try increase manually timeout with {.code options(timeout = xxx)}."), call = call)
    }
  }

  return(dw)
}
download <- function(url, dest, to) {
  withr::with_options(list(timeout = to), {
    curl::curl_download(url = url, destfile = dest, quiet = FALSE)
  })
}
