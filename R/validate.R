
hasInternet <- function() {
  tryCatch({
    readLines("https://www.google.com", n = 1, warn = FALSE)
    TRUE
  }, error = function(e) FALSE)
}
fetchVocabularies <- function() {
  if (hasInternet()) {
    file <- "https://raw.githubusercontent.com/oxford-pharmacoepi/AthenaR/refs/heads/main/inst/links.csv"
  } else {
    file <- system.file("links.csv", package = "AthenaR")
  }
  utils::read.csv(file = file)
}
validateVocabulary <- function(vocabulary, vocabularies, call = parent.frame()) {
  omopgenerics::assertChoice(vocabulary, vocabularies$vocabulary_version, length = 1, call = call)
}
validateVocabularyPath <- function(vocabularyPath, call = parent.frame()) {
  omopgenerics::assertCharacter(vocabularyPath, length = 1, call = call)
  if (!dir.exists(vocabularyPath)) {
    cli::cli_inform(c(i = "{.path {nm}} does not exist, creating it..."))
    dir.create(vocabularyPath)
  }
  invisible(vocabularyPath)
}
