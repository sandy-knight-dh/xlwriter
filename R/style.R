#' @title Column header cell formatting
#' @return Character vector (1L), XML Excel formatting string
#' @export
header_style <- function(wb) {
  
  header_font <- openxlsx2::create_font(b = TRUE)

  header_border <- openxlsx2::create_border(bottom = "medium")

  wb$styles_mgr$add(header_font, "header_font")

  wb$styles_mgr$add(header_border, "header_border")
  
  openxlsx2::create_cell_style(
    font_id = wb$styles_mgr$get_font_id("header_font"),
    border_id = wb$styles_mgr$get_border_id("header_border"),
    horizontal = "left",
    indent = 1,
    num_fmt_id = 3,
    vertical = "center"
  )
}

#' @title Cell formatting: left-align, indent 1, thousand comma
#' @return Character vector (1L), xml Excel formatting string
#' @export
values_style <- function() {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    num_fmt_id = 3,
    vertical = "center"
  )
}

#' @title Two decimal place cell formatting
#' @return Character vector (1L), xml Excel formatting string
#' @export
small_values_style <- function()  {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    num_fmt_id = 2,
    vertical = "center"
  )
}

#' @title Percent cell formatting
#' @return Character vector (1L), xml Excel formatting string
#' @export
percent_values_style <-  function() {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    num_fmt_id = 9,
    vertical = "center"
  )
}

#' @title Date cell formatting. Uses "mmm-yy" by default.
#' @return Character vector (1L), xml Excel formatting string
#' @export
date_values_style <- function() {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    num_fmt_id = 17,
    vertical = "center"
  )
}
