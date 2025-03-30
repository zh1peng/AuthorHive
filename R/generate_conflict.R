#' Generate a Conflict of Interest Statement
#'
#' @description Reads each author's Conflict text (if any) and merges them
#' into a single statement. If all authors say \"No conflict\", returns a
#' single line: \"All authors declare no conflict of interest.\"
#'
#' @param data A data frame containing a column \code{Conflict}.
#' @return A character string for the conflict of interest statement.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   LastName = c("Smith", "Johnson"),
#'   Conflict = c("No conflict", "Consultant at Company Z")
#' )
#' generate_conflict(authors)
generate_conflict <- function(data) {
  if (!"Conflict" %in% names(data)) {
    return("No conflict of interest information found.\n")
  }

  conflicts <- stats::na.omit(data$Conflict)

  if (length(conflicts) == 0) {
    return("No conflict of interest statements provided.\n")
  }

  # Check if all are "No conflict"
  all_no_conflict <- all(tolower(conflicts) == "no conflict")
  if (all_no_conflict) {
    return("All authors declare no conflict of interest.\n")
  }

  # Otherwise, list each author’s conflict
  # optionally include author name
  # e.g. "Alice Smith: No conflict. Bob Johnson: Consultant at Company Z."
  lines <- apply(data, 1, function(row) {
    if (is.na(row[["Conflict"]]) || row[["Conflict"]] == "") {
      return(NULL)
    }
    paste0(row[["FirstName"]], " ", row[["LastName"]], ": ", row[["Conflict"]])
  })
  lines <- lines[!sapply(lines, is.null)]

  final_text <- paste(lines, collapse = ". ")
  paste0("Conflict of Interest: ", final_text, ".")
}
