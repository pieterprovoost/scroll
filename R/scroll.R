#' scroll
#'
#' @docType package
#' @name scroll
"_PACKAGE"

color_na <- function(text) {
  gsub("\\bNA\\b", "\033[38;5;244mNA\033[0m", text)
}

color_numeric <- function(text) {
  gsub(
    "(^|\\s)(-?\\d+(\\.\\d+)?(e[+-]?\\d+)?)(?=\\s|$)",
    "\\1\033[38;5;136m\\2\033[0m",
    text,
    perl = TRUE
  )
}

color_bool <- function(text) {
  gsub("\\b(TRUE|FALSE)\\b", "\033[38;5;70m\\1\033[0m", text)
}

#' Scroll through a data frame's rows and columns using less.
#' 
#' @param df A data frame.
#' @export
scroll <- function(df) {
  tmpfile <- tempfile("scroll")
  on.exit(unlink(tmpfile))
  lines <- strsplit(knitr::kable(df, format = "simple"), "\n")
  for (i in seq_along(lines)) {
    lines[i] <- lines[i] |> color_numeric() |> color_na() |> color_bool()
  }
  writeLines(as.character(lines), tmpfile)
  system(paste("less --header=1 -S -R", shQuote(tmpfile)))
}
