#' @title Add a list of data frames to a workbook sheet
#' @param wb A workbook object
#' @param data_list A list of data frames
#' @param start_row Starting row position (numeric)
#' @param start_col Starting col position (numeric)
#' @param use_colname_hints Use column name suffixes to determine formatting.
#'   Applied to every data frame in `data_list`.
#' @export
add_data_list <- function(wb, data_list, start_row = 2, start_col = 2, use_colname_hints = FALSE) {
  nrows <- purrr::map_int(.x = data_list, nrow)

  gap <- 4

  steps <- c(start_row + gap, utils::head(nrows, -1) + gap)

  row_positions <- cumsum(steps)

  for (i in seq_along(data_list)) {
    data_title <- names(data_list[i])
    title_row <- row_positions[i] - 2

    data_title_dims <-
      openxlsx2::wb_dims(
        from_col = start_col,
        from_row = title_row,
        rows = 1,
        cols = seq(ncol(data_list[[i]]))
      )

    wb$add_data(
      sheet = openxlsx2::current_sheet(),
      x = data_title,
      start_col = start_col,
      start_row = title_row
    )

    xlwriter::apply_style(wb, dims = data_title_dims, "data_title_style")

    wb <- xlwriter::add_styled_data(
      wb,
      data_list[[i]],
      row_pos = row_positions[i],
      col_pos = start_col,
      use_colname_hints = use_colname_hints
    )
  }

  wb
}
