#' @title Column header cell formatting
#' @param wb A workbook object.
#' @return Character vector (1L), XML Excel formatting string
#' @export
data_title_style <- function(wb) {
  
  data_title_font <- openxlsx2::create_font(b = TRUE)

  data_title_border <- openxlsx2::create_border(bottom = "thick")

  wb$styles_mgr$add(data_title_font, "data_title_font")

  wb$styles_mgr$add(data_title_border, "data_title_border")
  
  openxlsx2::create_cell_style(
    font_id = wb$styles_mgr$get_font_id("data_title_font"),
    border_id = wb$styles_mgr$get_border_id("data_title_border"),
    horizontal = "left",
    indent = 1,
    num_fmt_id = 3,
    vertical = "center"
  )
}


#' @title Column header cell formatting
#' @param wb A workbook object.
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
#' @return Character vector (1L), XML Excel formatting string
#' @export
integer_values_style <- function() {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    num_fmt_id = 3,
    vertical = "center"
  )
}

#' @title Cell formatting: left-align, indent 1, thousand comma
#' @return Character vector (1L), XML Excel formatting string
#' @export
text_values_style <- function() {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    vertical = "center"
  )
}

#' @title Two decimal place cell formatting
#' @return Character vector (1L), XML Excel formatting string
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
#' @return Character vector (1L), XML Excel formatting string
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
#' @return Character vector (1L), XML Excel formatting string
#' @export
date_values_style <- function() {
  openxlsx2::create_cell_style(
    horizontal = "left",
    indent = 1,
    num_fmt_id = 17,
    vertical = "center"
  )
}
