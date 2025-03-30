#' Generate a Title Page
#'
#' @description Produces a formatted title page that includes the paper title,
#' author names with affiliation indices, and an affiliation legend. Optionally adds
#' a note for co-first authors.
#'
#' @param data A data frame containing at least: FirstName, MiddleName, LastName,
#' (optionally) Rank, Correspondence, and one or more Affiliation* columns.
#' @param style A character string specifying the style (e.g., "default", "APA", "Nature").
#' @param title An optional paper title.
#' @param co_first_footnote Logical. If TRUE and Rank is provided, adds a note for co-first authors.
#' @return A character string with the formatted title page.
#' @export
#' @examples
#' authors <- data.frame(
#'   FirstName = c("Alice", "Bob"),
#'   MiddleName = c("M.", ""),
#'   LastName = c("Smith", "Johnson"),
#'   Degree = c("PhD", "MD"),
#'   Email = c("alice@example.com", "bob@example.com"),
#'   Rank = c(1, 2),
#'   Correspondence = c(TRUE, FALSE),
#'   Affiliation1 = c("University of X, Dept. of Y", "University of X, Dept. of Y"),
#'   Affiliation2 = c(NA, "Company Z, Research Division"),
#'   stringsAsFactors = FALSE
#' )
#' generate_title_page(authors, style = "default", title = "Example Paper")
generate_title_page <- function(data, style = c("default", "APA", "Nature"),
                                title = NULL, co_first_footnote = TRUE) {
  style <- match.arg(style)
  
  # Sort authors by Rank if available
  if ("Rank" %in% names(data)) {
    data <- data[order(data$Rank), ]
  }
  
  # Identify affiliation columns (names starting with "Affiliation")
  aff_cols <- grep("^Affiliation", names(data), value = TRUE)
  all_affils <- unique(unlist(lapply(data[, aff_cols, drop = FALSE], function(x) {
    x <- as.character(x)
    x[x != "" & !is.na(x)]
  })))
  
  # Create mapping for affiliations
  affil_indices <- seq_along(all_affils)
  
  # Function to get indices for an author's affiliations
  get_aff_indices <- function(row) {
    affs <- as.character(unlist(row[aff_cols]))
    affs <- affs[affs != "" & !is.na(affs)]
    if(length(affs) == 0) return("")
    indices <- sapply(affs, function(x) which(all_affils == x))
    paste0(indices, collapse = ",")
  }
  
  # Build author lines with name and affiliation indices
  author_lines <- apply(data, 1, function(row) {
    name <- paste(row["FirstName"], row["MiddleName"], row["LastName"])
    name <- gsub("  ", " ", name)  # remove extra spaces if MiddleName is missing
    aff_index <- get_aff_indices(row)
    corr_symbol <- ""
    if ("Correspondence" %in% names(data)) {
      corr_val <- tolower(as.character(row["Correspondence"]))
      if (corr_val %in% c("true", "yes", "1")) {
        corr_symbol <- "*"
      }
    }
    paste0(name, corr_symbol, " [", aff_index, "]")
  })
  
  # Build affiliation legend
  affil_lines <- mapply(function(i, aff) {
    paste0("[", i, "] ", aff)
  }, affil_indices, all_affils)
  
  # Add co-first authors note if applicable
  co_first_note <- ""
  if (co_first_footnote && "Rank" %in% names(data)) {
    min_rank <- min(data$Rank, na.rm = TRUE)
    if(sum(data$Rank == min_rank) > 1) {
      co_first_note <- "Note: Authors with the same rank contributed equally."
    }
  }
  
  # Compose the full title page text
  title_text <- if (!is.null(title)) paste0("Title: ", title, "\n\n") else ""
  author_text <- paste(author_lines, collapse = "\n")
  affil_text <- paste(affil_lines, collapse = "\n")
  
  content <- paste0(title_text, author_text, "\n\nAffiliations:\n", affil_text, "\n\n", co_first_note)
  return(content)
}