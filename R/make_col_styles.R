#' @title Parse columns for styling
#' @param df A data frame.
#' @param row_pos Starting row (numeric).
#' @param col_pos Starting col (numeric).
#' @param pct_cols Character vector of columns that should take percent format
#' @param use_colname_hints Use column names added by \code{add_colname_hints()} to determine formatting style. \code{FALSE} by default.
#' @export
make_col_styles <- function(
  df,
  row_pos,
  col_pos,
  pct_cols = NULL,
  use_colname_hints = FALSE
) {
  if (isTRUE(use_colname_hints)) {
    hints <- parse_colname_hints(df)
    col_styles <-
      list(
        "string" = which(hints %in% c("string", "year")),
        "double" = which(hints == "double"),
        "integer" = which(hints == "integer"),
        "percent" = which(hints == "percent"),
        "date" = which(hints == "date")
      )
  } else {
    coltypes <- purrr::map_chr(.x = df, .f = \(x) class(x)[1])

    col_styles <-
      list(
        "string" = which(coltypes %in% c("factor", "character")),
        "double" = which(
          (coltypes == "numeric") & !(colnames(df) %in% pct_cols)
        ),
        "integer" = which(coltypes == "integer"),
        "percent" = which(colnames(df) %in% pct_cols),
        "date" = which(coltypes == "Date")
      )
  }

  col_styles <-
    purrr::map(.x = col_styles, .f = \(cols) {
      if (length(cols) == 0) {
        return(NULL)
      }
      openxlsx2::wb_dims(
        rows = seq_len(nrow(df)),
        cols = cols,
        from_row = row_pos + 1,
        from_col = col_pos
      )
    })

  df_header_dims <-
    openxlsx2::wb_dims(
      rows = 1,
      cols = seq_along(colnames(df)),
      from_col = col_pos,
      from_row = row_pos
    )

  col_styles <- append(col_styles, list("header" = df_header_dims))

  col_styles
}
