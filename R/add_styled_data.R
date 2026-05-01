#' @title Add a data frame with cell styling
#' @param wb A workbook object.
#' @param df A data frame.
#' @param row_pos Start row (numeric).
#' @param col_pos Start col (numeric).
#' @param pct_cols Character vector of column names to be percent formatted.
#' @param year_cols Character vector of column containing year values.
#' @export
add_styled_data <- function(wb, df, row_pos, col_pos, pct_cols = NULL, year_cols = NULL) {
  
  if(year_cols %in% colnames(df)) {
    df <-
      df |> 
      dplyr::mutate(across(tidyselect::all_of(year_cols), as.factor))
  }
  
  colnames(df) <- snakecase::to_sentence_case(colnames(df))

  col_styles <- make_col_styles(df, row_pos, col_pos, pct_cols)
  
  print(col_styles)

  wb$add_data(
    x = df,
    start_col = col_pos,
    start_row = row_pos,
    na.strings = ""
  )

  apply_style <- function(dims, style_name) {
    if (is.null(dims)) return(invisible(NULL))
    wb$set_cell_style(
      dims = dims,
      style = wb$styles_mgr$get_xf_id(style_name)
    )
  }

  apply_style(col_styles$header,  "header_style")
  apply_style(col_styles$string,  "text_values_style")
  apply_style(col_styles$integer, "integer_values_style")
  apply_style(col_styles$double,  "small_values_style")
  apply_style(col_styles$percent, "percent_values_style")
  apply_style(col_styles$date,    "date_values_style")

  wb <- wb$set_col_widths(
    sheet = current_sheet(),
    cols = wb_dims(row_pos:nrow(df), col_pos:ncol(df)),
    widths = "auto"
    )
  
  wb
}