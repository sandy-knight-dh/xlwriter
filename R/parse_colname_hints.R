#' @title Add formatting type hints to column names
#' @param df A data frame
#' @param .cols A vector of column names or indices. Names must be quoted.
#' @param formats A character vector of shorthand formatting types (see Details)
#' @details
#' Formatting types:
#'
#' | Format  | Shorthand |
#' |---------|-----------|
#' | String  | str       |
#' | Percent | pct       |
#' | Integer | int       |
#' | Double  | dbl       |
#' | Year    | yr        |
#' | Date    | date      |
add_colname_hints <- function(df, .cols, formats = NULL) {
  .cols <- unique(.cols)
  stopifnot(length(.cols) == length(formats))
  if (is.numeric(.cols)) {
  colnames(df)[.cols] <- paste(colnames(df)[.cols], formats, sep = ".")
  } else {
  .cols <- purrr::map_int(.x = .cols, .f = \(x) which(x == colnames(df)))
  colnames(df)[.cols] <- paste(colnames(df)[.cols], formats, sep = ".")
  }
  df
}

parse_colname_hints <- function(df) {
  nms <- colnames(df)

  hints <- stringr::str_extract(nms, "\\.int$|\\.pct$|\\.str$|\\.dbl$|\\.yr$|\\.date$")

  dplyr::recode_values(
    hints,
    from = c(NA_character_, ".str", ".pct", ".int", ".dbl", ".yr", ".date"),
    to = c("string", "string", "percent", "integer", "double", "year", "date")
  )
}
