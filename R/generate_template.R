#' Generate a sample CSV template for authordown
#'
#' @description Creates a sample CSV (or Excel) with columns for authors,
#' affiliations, acknowledgments, conflicts, etc.
#' @param file A file path where the template should be written.
#'   Defaults to \"authordown_template.csv\" in the current directory.
#' @param excel Logical. If TRUE, writes an Excel file (.xlsx) instead of CSV.
#' @return Invisibly returns the data frame used for the template.
#' @export
#' @examples
#' \dontrun{
#' generate_template() # writes authordown_template.csv
#' }
generate_template <- function(file = "authordown_template.csv", excel = FALSE) {
  # A minimal example with multiple affiliations possible
  template_data <- data.frame(
    FirstName = c("Alice", "Bob"),
    MiddleName = c("M.", ""),
    LastName = c("Smith", "Johnson"),
    Degree = c("PhD", "MD"),
    Email = c("alice@example.com", "bob@example.com"),
    Rank = c(1, 2),
    Correspondence = c(TRUE, FALSE),
    Acknowledgement = c("Thanks to funder A", ""),
    Conflict = c("No conflict", "Consultant at Company Z"),
    Contribution = c(
      "Conceptualization; Writing - Original Draft",
      "Writing - Review & Editing"
    ),
    Affiliation1 = c("University of X, Dept. of Y", "University of X, Dept. of Y"),
    Affiliation2 = c(NA, "Company Z, Research Division")
  )

  if (!excel) {
    # Write CSV
    utils::write.csv(template_data, file = file, row.names = FALSE)
    message("Template CSV written to: ", file)
  } else {
    # Write Excel
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' required for Excel output. Please install it.")
    }
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "authordown_template")
    openxlsx::writeData(wb, "authordown_template", template_data)
    if (!grepl("\\.xlsx$", file)) {
      file <- paste0(file, ".xlsx")
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    message("Template Excel file written to: ", file)
  }

  invisible(template_data)
}
