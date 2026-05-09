#' @title Apply a style to a \code{wb_dims} range.
#' @param wb A workbook object.
#' @param dims An Excel range.
#' @param style_name The name of a registered style.
#' @export
apply_style <- function(wb, dims, style_name) {
  if (is.null(dims)) {
    return(invisible(NULL))
  }
  wb$set_cell_style(
    dims = dims,
    style = wb$styles_mgr$get_xf_id(style_name)
  )
}

#' @title Add a data frame with cell styling
#' @param wb A workbook object.
#' @param df A data frame.
#' @param row_pos Start row (numeric).
#' @param col_pos Start col (numeric).
#' @param pct_cols Character vector of column names to be percent formatted.
#' @param year_cols Character vector of column containing year values.
#' @param use_colname_hints Use column names to determine formatting.
#' @param data_title Optional string written one row above the column headers
#'   using the registered \code{data_title_style}.
#' @export
add_styled_data <- function(
  wb,
  df,
  row_pos,
  col_pos,
  pct_cols = NULL,
  year_cols = NULL,
  use_colname_hints = FALSE,
  data_title = NULL
) {
  if (isTRUE(use_colname_hints)) {
    yr_cols <- which(parse_colname_hints(df) == "year")
    if (length(yr_cols) > 0) {
      df <- dplyr::mutate(
        df,
        dplyr::across(tidyselect::all_of(yr_cols), as.factor)
      )
    }
  }

  if (!is.null(year_cols) && any(year_cols %in% colnames(df))) {
    df <-
      df |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(year_cols), as.factor))
  }

  col_styles <- make_col_styles(
    df,
    row_pos,
    col_pos,
    pct_cols,
    use_colname_hints
  )

  colnames(df) <- stringr::str_remove(
    colnames(df),
    "\\.(int|pct|str|dbl|yr|date)$"
  )

  wb$add_data(
    x = df,
    start_col = col_pos,
    start_row = row_pos,
    na.strings = ""
  )

  if (!is.null(data_title)) {
    title_row <- row_pos - 1
    title_dims <- openxlsx2::wb_dims(
      from_col = col_pos,
      from_row = title_row,
      rows = 1,
      cols = seq(ncol(df))
    )

    wb$add_data(
      sheet = openxlsx2::current_sheet(),
      x = data_title,
      start_col = col_pos,
      start_row = title_row
    )

    apply_style(wb, title_dims, "data_title_style")
  }

  apply_style(wb, col_styles$header, "header_style")
  apply_style(wb, col_styles$string, "text_values_style")
  apply_style(wb, col_styles$integer, "integer_values_style")
  apply_style(wb, col_styles$double, "small_values_style")
  apply_style(wb, col_styles$percent, "percent_values_style")
  apply_style(wb, col_styles$date, "date_values_style")

  wb <- wb$set_col_widths(
    sheet = openxlsx2::current_sheet(),
    cols = seq(col_pos, length.out = ncol(df)),
    widths = "auto"
  )

  wb
}
