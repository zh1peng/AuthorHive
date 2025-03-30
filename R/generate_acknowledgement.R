#' Generate an Acknowledgement Section
#'
#' @description Combines acknowledgements from each author into a formatted paragraph.
#' Each line indicates the author and their acknowledgement.
#'
#' @param data A data frame containing at least the columns: FirstName, LastName, and Acknowledgement.
#' @return A character string with the formatted acknowledgements.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   LastName = c("Smith", "Johnson"),
#'   Acknowledgement = c("Thanks for funding A", "Supported by XYZ"),
#'   stringsAsFactors = FALSE
#' )
#' generate_acknowledgement(authors)
generate_acknowledgement <- function(data) {
  if (!("Acknowledgement" %in% names(data))) {
    return("No acknowledgement information provided.")
  }
  
  valid <- data[!is.na(data$Acknowledgement) & data$Acknowledgement != "", ]
  if (nrow(valid) == 0) {
    return("No acknowledgements provided.")
  }
  
  lines <- apply(valid, 1, function(row) {
    paste0(row["FirstName"], " ", row["LastName"], " acknowledges: ", row["Acknowledgement"])
  })
  result <- paste(lines, collapse = "\n")
  return(result)
}
