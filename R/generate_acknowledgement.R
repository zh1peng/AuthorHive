#' Generate an Acknowledgement Section
#'
#' @description Reads each author's Acknowledgement text (if any) and merges 
#' them into a single paragraph or bullet list.
#'
#' @param data A data frame containing a column \code{Acknowledgement}.
#' @param style A character string to control the format (\"paragraph\" or \"bullets\").
#' @return A character string representing the combined acknowledgements.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   Acknowledgement = c("Thanks to funder A", "")
#' )
#' generate_acknowledgement(authors)
generate_acknowledgement <- function(data, style = c("paragraph", "bullets")) {
  style <- match.arg(style)
  if (!"Acknowledgement" %in% names(data)) {
    return("No acknowledgement column found.\n")
  }
  
  # Filter out empty acknowledgements
  non_empty <- data[!is.na(data$Acknowledgement) & data$Acknowledgement != "", ]
  if (nrow(non_empty) == 0) {
    return("No acknowledgements provided.\n")
  }
  
  # Build lines
  lines <- apply(non_empty, 1, function(row){
    # optional: you could include the author name if you want
    # paste0(row[["FirstName"]], " ", row[["LastName"]], ": ", row[["Acknowledgement"]])
    row[["Acknowledgement"]]
  })
  
  if (style == "paragraph") {
    ack_text <- paste(lines, collapse = " ")
    ack_text <- paste("Acknowledgements:", ack_text)
  } else {
    # bullet style
    bullet_lines <- paste("- ", lines, collapse = "\n")
    ack_text <- paste("Acknowledgements:\n", bullet_lines)
  }
  
  return(ack_text)
}
