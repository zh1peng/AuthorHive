#' Generate Author Contributions
#'
#' @description Combines each author's contribution statement into a clear paragraph.
#' Each line indicates the author and their specific contribution.
#'
#' @param data A data frame containing at least the columns: FirstName, LastName, and Contribution.
#' @return A character string summarizing the author contributions.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   LastName = c("Smith", "Johnson"),
#'   Contribution = c("Conceptualization; Data curation", "Supervision; Writing - review"),
#'   stringsAsFactors = FALSE
#' )
#' generate_contribution(authors)
generate_contribution <- function(data) {
  if (!("Contribution" %in% names(data))) {
    return("No author contributions provided.")
  }
  
  valid <- data[!is.na(data$Contribution) & data$Contribution != "", ]
  if (nrow(valid) == 0) {
    return("No author contributions provided.")
  }
  
  lines <- apply(valid, 1, function(row) {
    paste0(row["FirstName"], " ", row["LastName"], " contributed as follows: ", row["Contribution"])
  })
  result <- paste(lines, collapse = "\n")
  return(result)
}