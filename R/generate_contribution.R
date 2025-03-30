#' Generate Author Contributions (CRediT)
#'
#' @description Merges each author's contribution statement into a single block.
#' Optionally includes the author's name in front for clarity.
#'
#' @param data A data frame containing a column \code{Contribution}.
#' @param list_style If TRUE, each author’s contribution is on a new line.
#' @return A character string summarizing contributions.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   LastName = c("Smith", "Johnson"),
#'   Contribution = c("Conceptualization; Data curation", "Supervision; Writing - review")
#' )
#' generate_contribution(authors)
generate_contribution <- function(data, list_style = TRUE) {
  if (!"Contribution" %in% names(data)) {
    return("No contribution information found.\n")
  }

  non_empty <- data[!is.na(data$Contribution) & data$Contribution != "", ]
  if (nrow(non_empty) == 0) {
    return("No contributions provided.\n")
  }

  lines <- apply(non_empty, 1, function(row) {
    author_name <- paste(row[["FirstName"]], row[["LastName"]])
    contrib <- row[["Contribution"]]
    paste0(author_name, ": ", contrib)
  })

  if (list_style) {
    # each author on a new line
    final_text <- paste(lines, collapse = "\n")
    final_text <- paste("Author Contributions:\n", final_text)
  } else {
    # single paragraph
    final_text <- paste(lines, collapse = ". ")
    final_text <- paste("Author Contributions:", final_text)
  }

  return(final_text)
}
