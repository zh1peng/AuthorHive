#' Generate a Conflict of Interest Statement
#'
#' @description Produces a readable conflict statement. If all authors report no conflict,
#' it states so. Otherwise, it lists the authors reporting conflicts and then indicates that
#' the remaining authors declare no conflict.
#'
#' @param data A data frame containing at least the columns: FirstName, LastName, and Conflict.
#' @return A character string with the formatted conflict statement.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   LastName = c("Smith", "Johnson"),
#'   Conflict = c("No conflict", "Consultant at Company Z"),
#'   stringsAsFactors = FALSE
#' )
#' generate_conflict(authors)
generate_conflict <- function(data) {
  if (!("Conflict" %in% names(data))) {
    return("No conflict of interest information provided.")
  }
  
  data$Conflict <- trimws(as.character(data$Conflict))
  
  conflict_authors <- data[!is.na(data$Conflict) & data$Conflict != "" &
                              tolower(data$Conflict) != "no conflict", ]
  non_conflict_authors <- data[is.na(data$Conflict) | data$Conflict == "" |
                                 tolower(data$Conflict) == "no conflict", ]
  
  if (nrow(conflict_authors) == 0) {
    return("All authors declare no conflict of interest.")
  } else {
    conflict_lines <- apply(conflict_authors, 1, function(row) {
      paste0(row["FirstName"], " ", row["LastName"], " (", row["Conflict"], ")")
    })
    conflict_text <- paste(conflict_lines, collapse = "; ")
    result <- paste0("The following authors report conflicts of interest: ", 
                     conflict_text, ".")
    if (nrow(non_conflict_authors) > 0) {
      non_conflict_names <- apply(non_conflict_authors, 1, function(row) {
        paste(row["FirstName"], row["LastName"])
      })
      result <- paste0(result, " All other authors (", 
                       paste(non_conflict_names, collapse = ", "), 
                       ") declare no conflict of interest.")
    }
    return(result)
  }
}