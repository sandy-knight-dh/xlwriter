#' @title Add a list of data frames to a workbook sheet, placed horizontally
#' @param wb A workbook object
#' @param data_list A list of data frames
#' @param start_row Starting row position (numeric)
#' @param start_col Starting col position (numeric)
#' @param use_colname_hints Use column name suffixes to determine formatting.
#' @param name_title_range give the title ranges an Excel name
#'   Applied to every data frame in `data_list`.
#' @param gap Number of empty columns between consecutive data frames.
#' @param note Optional text placed between the title and the data, with a
#'   blank row on either side. Either a single string applied to every frame
#'   or a character vector matching `length(data_list)`.
#' @export
add_data_list_horizontal <- function(
  wb,
  data_list,
  start_row = 2,
  start_col = 2,
  use_colname_hints = FALSE,
  name_title_range = FALSE,
  gap = 1,
  note = NULL
) {

  notes <- resolve_notes(note, length(data_list))
  note_offset <- if (is.null(notes)) 0L else 2L

  ncols <- purrr::map_int(.x = data_list, ncol)

  steps <- c(start_col, utils::head(ncols, -1) + gap)

  col_positions <- cumsum(steps)

  title_row <- start_row
  note_row <- start_row + 2
  data_row <- start_row + 2 + note_offset

  for (i in seq_along(data_list)) {
    data_title <- names(data_list[i])
    col_pos <- col_positions[i]

    data_title_dims <-
      openxlsx2::wb_dims(
        from_col = col_pos,
        from_row = title_row,
        rows = 1,
        cols = seq(ncol(data_list[[i]]))
      )

    wb$add_data(
      sheet = openxlsx2::current_sheet(),
      x = data_title,
      start_col = col_pos,
      start_row = title_row
    )

    if (isTRUE(name_title_range)) {

      wb$add_named_region(
        dims = data_title_dims,
        name = create_name_for_range(data_title)
      )
    }

    xlwriter::apply_style(wb, dims = data_title_dims, "data_title_style")

    if (!is.null(notes)) {
      wb$add_data(
        sheet = openxlsx2::current_sheet(),
        x = notes[i],
        start_col = col_pos,
        start_row = note_row
      )
    }

    wb <- xlwriter::add_styled_data(
      wb,
      data_list[[i]],
      row_pos = data_row,
      col_pos = col_pos,
      use_colname_hints = use_colname_hints
    )
  }

  wb
}
