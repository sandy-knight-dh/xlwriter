#' @title Make an empty workbook with cell styles (and one sheet)
#' @param first_sheet_name Name of the initial sheet to be created.
#' @export
make_template_workbook <- function(first_sheet_name = NULL) {
  
  if (is.null(first_sheet_name)) {
    first_sheet_name <- "Sheet1"
  }

  wb <- openxlsx2::wb_workbook()$add_worksheet(first_sheet_name)

  wb$styles_mgr$add(header_style(wb), "header_style")
  
  wb$styles_mgr$add(values_style(), "values_style")
  
  wb$styles_mgr$add(small_values_style(), "small_values_style")
  
  wb$styles_mgr$add(percent_values_style(), "percent_values_style")
  
  wb$styles_mgr$add(date_values_style(), "date_values_style")

  wb
}
